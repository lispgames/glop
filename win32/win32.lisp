(in-package #:glop-win32)

;; WIN32
(define-foreign-library user32
    (t (:default "user32")))
(use-foreign-library user32)

(defcfun ("GetClientRect" get-client-rect) bool
  (wnd hwnd) (rect-out :pointer))

(defun get-geometry (wnd)
  (with-foreign-object (rct 'rect)
    (get-client-rect wnd rct)
    (with-foreign-slots ((left top right bottom) rct rect)
      (values left bottom
              (- right left)
              (- bottom top)))))

(defcfun ("SetCapture" set-capture) hwnd
  (wnd hwnd))

(defcfun ("ReleaseCapture" release-capture) bool)

(defcfun ("GetDC" get-dc) hdc
  (wnd hwnd))

(defcfun ("ReleaseDC" %release-dc) :int
  (wnd hwnd) (dc hdc))

(defcfun ("PostQuitMessage" %post-quit-message) :void
  (exit-code :int))

(defcfun ("DefWindowProcA" %def-window-proc) :long
  (wnd hwnd) (msg :uint) (w-param wparam) (l-param lparam))

(defcfun ("GetMessageA" %get-message) bool
  (msg :pointer) (wnd hwnd) (filter-min :uint) (filter-max :uint))

(defcfun ("TranslateMessage" %translate-message) bool
  (msg :pointer))

(defcfun ("DispatchMessageA" %dispatch-message) bool
  (msg :pointer))

(defcfun ("PeekMessageA" %peek-message) bool
  (lpmsg :pointer) (h-wnd hwnd)
  (filter-min :uint) (filter-max :uint)
  (remove remove-msg))

(defcfun ("GetKeyboardState" get-keyboard-state) bool
  (state-out :pointer))

(defcfun ("ToAscii" to-ascii) :int
  (vkey :uint) (scan-code :uint) (kbd-state :pointer) (buffer :pointer) (flags :uint))

;; XXX: this is an ugly hack and should probably be changed
;; We use the *event* var to allow window-proc callback to generate glop:event objects
;; that can be return from next-event

(defvar *event* nil)

(defun next-event (wnd &optional blocking)
  (with-foreign-object (msg 'msg)
    (if blocking
        (when (> (%get-message msg wnd 0 0) 0)
          (%translate-message msg)
          (%dispatch-message msg))
        (when (%peek-message msg wnd 0 0 :pm-remove)
          (%translate-message msg)
          (%dispatch-message msg))))
  *event*)

;; XXX: we probably have problems with negative numbers here...
(defun low-word (value)
  (logand value #xFFFF))

(defun high-word (value)
  (logand (ash value -16) #xFFFF))

(defun win32-lookup-key (w-param l-param)
  (let ((key (foreign-enum-keyword 'vkey-type w-param :errorp nil)))
    (or key
        (with-foreign-object (kbd-state :char 256)
          (when (get-keyboard-state kbd-state)
            (with-foreign-object (buffer :int16)
              (unless (/= (to-ascii w-param l-param kbd-state buffer 0) 1)
                (code-char (mem-ref buffer :char)))))))))


(let ((last-x 0)
      (last-y 0)
      (from-configure nil))
  (defcallback window-proc :long ((wnd hwnd) (msg :uint) (w-param wparam) (l-param lparam))
     (let ((msg-type (foreign-enum-keyword 'msg-type msg :errorp nil)))
       (case msg-type
         (:wm-close
          (setf *event* (glop::make-event :type :close))
          (return-from window-proc 0))
         (:wm-destroy
          (%post-quit-message 0)
          (return-from window-proc 0))
         (:wm-mouse-move
          (let ((low (low-word l-param))
                (high (high-word l-param)))
            (when (or (/= low last-x) (/= high last-y))
              (setf *event* (glop::make-event :type :mouse-motion
                                              :x low :y high
                                              :dx (- low last-x) :dy (- high last-y)))
              (setf last-x low last-y high))
            (return-from window-proc 0)))
         (:wm-paint
          ;; XXX: this is an ugly hack but WM_SIZE acts strangely...
          (multiple-value-bind (x y width height) (get-geometry wnd)
            (setf *event* (glop::make-event :type (if from-configure
                                                      (progn (setf from-configure nil)
                                                             :configure)
                                                      :expose)
                                                :width width :height height))))
         (:wm-lbutton-down
          (set-capture wnd)
          (setf *event* (glop::make-event :type :button-press
                                          :button :left-button))
          (return-from window-proc 0))
         (:wm-lbutton-up
          (release-capture)
          (setf *event* (glop::make-event :type :button-release
                                          :button :left-button))
          (return-from window-proc 0))
         (:wm-rbutton-down
          (set-capture wnd)
          (setf *event* (glop::make-event :type :button-press
                                          :button :right-button))
          (return-from window-proc 0))
         (:wm-rbutton-up
          (release-capture)
          (setf *event* (glop::make-event :type :button-release
                                          :button :right-button))
          (return-from window-proc 0))
         (:wm-mbutton-down
          (set-capture wnd)
          (setf *event* (glop::make-event :type :button-press
                                          :button :middle-button))
          (return-from window-proc 0))
         (:wm-mbutton-up
          (release-capture)
          (setf *event* (glop::make-event :type :button-release
                                          :button :middle-button))
          (return-from window-proc 0))
         (:wm-key-up
          (let ((key (win32-lookup-key w-param l-param)))
            (when key
              (setf *event* (glop::make-event :type :key-release
                                              :key  key))))
          (return-from window-proc 0))
         (:wm-key-down
          (let ((key (win32-lookup-key w-param l-param)))
            (when key
              (setf *event* (glop::make-event :type :key-press
                                              :key  key))))
          (return-from window-proc 0))
         (:wm-mouse-wheel
          (format t "WM_MOUSEWHEEL: ~S => ~S~%" w-param (high-word w-param))
          (setf *event* (glop::make-event :type :button-press
                                          :button (if (> w-param 0)
                                                      :wheel-up :wheel-down)))
          (return-from window-proc 0))
         (:wm-size
          (format t"WM_SIZE !!!!~%")
          ;; XXX: other part of above ugly hack...
          ;; Not sure why but it looks WM_SIZE doesn't get out of window-proc
          ;; until mouse button is released...
          ;; Maybe we can use WM_EXITSIZEMOVE and send :configure event only at the end
          ;; of the window resize while still sending a proper expose event...
          ;; With this hack it seems that WM_PAINT is handled directly thus overwriting
          ;; our glop :configure event with a :expose event...weird
          (setf from-configure t)
          (return-from window-proc 0))
         (:wm-show-window
          (multiple-value-bind (x y width height) (get-geometry wnd)
            (setf *event* (glop::make-event :type (if (zerop w-param)
                                                      :hide
                                                      :show)
                                            :width width :height height))))
         (:wm-set-focus
          (format t "WM_SETFOCUS~%")
          (return-from window-proc 0)))
       ;; (:wm-sys-command
       ;;  (format t "WM_SYSCOMMAND~%")))
       (%def-window-proc wnd msg w-param l-param))))

(defcfun ("RegisterClassA" %register-class) :int16
  (wndclass :pointer))

(defcfun ("RegisterClassExA" %register-class-ex) :int16
  (wndclass-ex :pointer))


(defcfun ("UnregisterClassA" unregister-class) bool
  (class-name :string) (instance hinstance))

(defun create-and-register-class (module-instance name)
  (with-foreign-object (class 'wndclass)
    (with-foreign-slots ((style wndproc cls-extra wnd-extra instance icon cursor
                                br-background menu-name class-name) class wndclass)
      (setf style (foreign-bitfield-value 'class-style-flags
                                          '(:cs-hredraw :cs-vredraw :cs-own-dc))
            wndproc (callback window-proc)
            cls-extra 0
            wnd-extra 0
            instance module-instance
            icon  (null-pointer)
            cursor (null-pointer)
            br-background (null-pointer)
            menu-name (null-pointer)
            class-name name))
    (when (zerop (%register-class class))
      (format t "Error registering class ~S: ~S~%" name (get-last-error)))))

(defcfun ("SetWindowText" set-window-text) bool
  (wnd hwnd) (title :string))

(defcfun ("CreateWindowExA" create-window-ex) hwnd
  (ex-style wex-style) (class-name :string) (win-name :string)
  (style wstyle) (x :int) (y :int) (width :int) (height :int)
  (parent hwnd) (menu hmenu) (instance hinstance) (param :pointer))

(defcfun ("DestroyWindow" destroy-window) bool
  (wnd hwnd))

(defcfun ("UpdateWindow" update-window) bool
  (wnd hwnd))

(defcfun ("ShowWindow" show-window) bool
  (wnd hwnd) (cmd-show sw-cmd-show))

(defcfun ("SetForegroundWindow" set-foreground-window) bool
  (wnd hwnd))

(defcfun ("SetFocus" set-focus) hwnd
  (wnd hwnd))

(define-foreign-library kernel32
    (t (:default "kernel32")))
(use-foreign-library kernel32)

(defcfun ("GetModuleHandleW" get-module-handle) hmodule
  (module-name :string))

(defcfun ("GetLastError" get-last-error) :int32)

(define-foreign-library gdi32
    (t (:default "gdi32")))
(use-foreign-library gdi32)

(defcfun ("ChoosePixelFormat" %choose-pixel-format) :int
  (dc hdc) (pfd :pointer))

(defcfun ("SetPixelFormat" %set-pixel-format) bool
  (dc hdc) (pixel-format :int) (pfd :pointer))

(defun choose-pixel-format (dc &key (double-buffer t)
                                    stereo
                                    (red-size 0)
                                    (green-size 0)
                                    (blue-size 0)
                                    (alpha-size 0)
                                    (depth-size 0)
                                    accum-buffer
                                    (accum-red-size 0)
                                    (accum-green-size 0)
                                    (accum-blue-size 0)
                                    stencil-buffer (stencil-size 0))
  (with-foreign-object (pfd 'pixelformatdescriptor)
    (format t "Creating PIXELFORMATDESCRIPTOR struct~%")
    (with-foreign-slots ((size version flags pixel-type color-bits
                               red-bits green-bits blue-bits alpha-bits
                               accum-bits accum-red-bits accum-green-bits accum-blue-bits
                               stencil-bits
                               depth-bits) pfd pixelformatdescriptor)
      (setf size (foreign-type-size 'pixelformatdescriptor)
            version 1
            flags (foreign-bitfield-value 'pfd-flags
                       (list :pfd-draw-to-window :pfd-support-opengl
                             (if double-buffer
                                 :pfd-double-buffer
                                 :pfd-double-buffer-dont-care)
                             (if stereo
                                 :pfd-stereo
                                 :pfd-stereo-dont-care)
                             ))
            pixel-type (foreign-enum-value 'pfd-pixel-type :pfd-type-rgba)
            color-bits 32 ;; we want proper RGBA but not sure to understand this struct field
            red-bits red-size
            green-bits green-size
            blue-bits blue-size
            alpha-bits alpha-size
            accum-bits (if accum-buffer
                           (+ accum-red-size accum-green-size accum-blue-size)
                           0)
            accum-red-bits accum-red-size
            accum-green-bits accum-green-size
            accum-blue-bits accum-blue-size
            depth-bits depth-size
            stencil-bits stencil-size))
    (format t "Choosing pixel format !!~%")
    (let ((fmt (%choose-pixel-format dc pfd)))
      (%set-pixel-format dc fmt pfd)
      fmt)))

(defcfun ("SwapBuffers" swap-buffers) bool
  (dc hdc))

;; WGL
(define-foreign-library opengl
  (t (:default "opengl32")))
(use-foreign-library opengl)

(defctype hglrc handle)

(defcfun ("wglCreateContext" wgl-create-context) hglrc
  (dc hdc))

(defcfun ("wglMakeCurrent" wgl-make-current) bool
  (dc hdc) (rc hglrc))

(defcfun ("wglDeleteContext" wgl-delete-context) bool
  (rc hglrc))

(defcfun ("wglGetProcAddress" wgl-get-proc-address) :pointer
  (proc-name :string))

;; test
(defvar *running* t)


;; GLOP
(in-package #:glop)

(setf gl-get-proc-address #'glop-win32::wgl-get-proc-address)


(defstruct (win32-window (:include window))
  module-handle
  class-name
  pixel-format
  dc
  id)

(defstruct wgl-context
  ctx)

(defmethod create-gl-context ((win win32-window) &key (make-current t) major minor)
  (let ((ctx (make-wgl-context)))
    (let ((wgl-ctx (glop-win32::wgl-create-context (win32-window-dc win))))
      (unless wgl-ctx
        (format t "Error creating GL context: ~S~%" (glop-win32::get-last-error)))
      (setf (wgl-context-ctx ctx) wgl-ctx))
    (when make-current
      (attach-gl-context win ctx))
    ctx))

(defmethod destroy-gl-context (ctx)
  (detach-gl-context ctx)
  (glop-win32::wgl-delete-context (wgl-context-ctx ctx)))

(defmethod attach-gl-context ((win win32-window) (ctx wgl-context))
  (glop-win32::wgl-make-current (win32-window-dc win) (wgl-context-ctx ctx)))

(defmethod detach-gl-context ((ctx wgl-context))
  (glop-win32::wgl-make-current (cffi:null-pointer) (cffi:null-pointer)))

(defmethod create-window (title width height &key major minor
                                                  (double-buffer t)
                                                  stereo
                                                  (red-size 0)
                                                  (green-size 0)
                                                  (blue-size 0)
                                                  (alpha-size 0)
                                                  (depth-size 0)
                                                  accum-buffer
                                                  (accum-red-size 0)
                                                  (accum-green-size 0)
                                                  (accum-blue-size 0)
                                                  stencil-buffer (stencil-size 0))
  (let ((win (make-win32-window
              :module-handle (glop-win32::get-module-handle (cffi:null-pointer)))))
    (when (and major minor)
      (error "Specific context creation isn't supported yet. Leave :major and :minor as NIL."))
    ;; create window class
    (glop-win32::create-and-register-class (win32-window-module-handle win) "OpenGL")
    (setf (win32-window-class-name win) "OpenGL")
    (let ((wnd (glop-win32::create-window-ex '(:ws-ex-app-window :ws-ex-window-edge)
                                  "OpenGL"
                                  title
                                  '(:ws-overlapped-window :ws-clip-siblings :ws-clip-children)
                                  0 0 width height (cffi:null-pointer) (cffi:null-pointer)
                                  (win32-window-module-handle win) (cffi:null-pointer))))
      (unless wnd
        (error "Can't create window (error ~S)~%" (glop-win32::get-last-error)))
      (setf (win32-window-id win) wnd))
    (setf (win32-window-width win) width)
    (setf (win32-window-height win) height)
    (setf (win32-window-dc win) (glop-win32::get-dc (win32-window-id win)))
    ;; choose pixel format
    ;; XXX: kwargs passing is ugly here and we need something else...
    (setf (win32-window-pixel-format win) (glop-win32::choose-pixel-format
                                           (win32-window-dc win)
                                           :double-buffer double-buffer
                                           :stereo stereo
                                           :red-size red-size
                                           :green-size green-size
                                           :blue-size blue-size
                                           :alpha-size alpha-size
                                           :depth-size depth-size
                                           :accum-buffer accum-buffer
                                           :accum-red-size accum-red-size
                                           :accum-green-size accum-green-size
                                           :accum-blue-size accum-blue-size
                                           :stencil-buffer stencil-buffer
                                           :stencil-size stencil-size))
    ;; create GL context and make it current
    (setf (window-gl-context win) (create-gl-context win :make-current t))
    ;; show window
    (glop-win32::set-foreground-window (win32-window-id win))
    (glop-win32::update-window (win32-window-id win))
    (show-window win)
    ;; return created window
    win))

(defmethod show-window ((win win32-window))
  (glop-win32::show-window (win32-window-id win) :sw-show)
  (glop-win32::set-focus (win32-window-id win)))

(defmethod hide-window ((win win32-window))
  (glop-win32::show-window (win32-window-id win) :sw-hide))

(defmethod set-window-title ((win win32-window) title)
  (setf (slot-value win 'title) title)
  (glop-win32::set-window-text (win32-window-id win) title))

(defmethod destroy-window ((win win32-window))
  (glop-win32::destroy-window (win32-window-id win))
  (glop-win32::unregister-class (win32-window-class-name win)
                                 (win32-window-module-handle win)))

(defmethod swap-buffers ((win win32-window))
  (glop-win32::swap-buffers (win32-window-dc win)))

(defmethod next-event ((win win32-window) &key blocking)
  (let ((evt (glop-win32::next-event (win32-window-id win) blocking)))
    (setf glop-win32::*event* nil)
    evt))