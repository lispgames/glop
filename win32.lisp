(defpackage :glop-win32
  (:use #:cl #:cffi)
  (:export))

(in-package #:glop-win32)

;; only on windows 32 bit
(defctype wparam :int32)
(defctype lparam :int32)

(defctype dword  :int32)

(defctype bool :int)

(defctype handle :pointer)
(defctype hwnd handle)
(defctype hdc handle)
(defctype hmenu handle)
(defctype hmodule handle)
(defctype hinstance handle)
(defctype hicon handle)
(defctype hcursor handle)
(defctype hbrush handle)

(defcstruct point
  (x :long)
  (y :long))

(defcstruct msg
  (h-wnd hwnd)
  (message :unsigned-int)
  (w-param wparam)
  (l-param lparam)
  (time dword)
  (pt point))

(defbitfield wex-style
  (:ws-ex-app-window #x40000)
  (:ws-ex-window-edge 256))

(defbitfield wstyle
  (:ws-clip-children #x2000000)
  (:ws-clip-siblings #x4000000)
  (:ws-overlapped-window #xcf0000))

(defbitfield class-style-flags
  (:cs-byte-align-client 4096)
  (:cs-byte-align-window 8192)
  (:cs-key-cvt-window 4)
  (:cs-no-key-cvt 256)
  (:cs-class-dc 64)
  (:cs-dbl-clks 8)
  (:cs-global-class 16384)
  (:cs-hredraw 2)
  (:cs-no-close 512)
  (:cs-own-dc 32)
  (:cs-parent-dc 128)
  (:cs-save-bits 2048)
  (:cs-vredraw 1)
  (:cs-ime #x10000)
  (:cs-drop-shadow #x20000))

(defcstruct wndclass
  (style class-style-flags)
  (wndproc :pointer)
  (cls-extra :int)
  (wnd-extra :int)
  (instance hinstance)
  (icon hicon)
  (cursor hcursor)
  (br-background hbrush)
  (menu-name :string)
  (class-name :string))


(defcstruct wndclass-ex
  (size :uint)
  (style class-style-flags)
  (wndproc :pointer)
  (cls-extra :int)
  (wnd-extra :int)
  (instance hinstance)
  (icon hicon)
  (cursor hcursor)
  (br-background hbrush)
  (menu-name :string)
  (class-name :string)
  (small-icon hicon))

(defcenum msg-type
  (:wm-destroy 2)
  (:wm-close 16)
  (:wm-mouse-move 512)
  (:wm-paint 15)
  (:wm-lbutton-down 513)
  (:wm-lbutton-up 514)
  (:wm-rbutton-down 516)
  (:wm-rbutton-up 517)
  (:wm-mbutton-down 519)
  (:wm-mbutton-up 520)
  (:wm-key-up 257)
  (:wm-key-down 256)
  (:wm-mouse-wheel 522)
  (:wm-size 5)
  (:wm-show-window 24)
  (:wm-set-focus 7)
  (:wm-sys-command 274))

(defcenum system-command-type
  (:sc-minimize #xf020)
  (:sc-maximize #xf040)
  (:sc-restore #xf120))

(defcenum sw-cmd-show
  (:sw-hide 0)
  :sw-normal
  (:sw-show-normal 1)
  :sw-show-minimized
  :sw-maximize
  (:sw-show-maximized 3)
  :sw-show-no-activate
  :sw-show
  :sw-minimize
  :sw-show-min-no-activate
  :sw-show-na
  :sw-restore
  :sw-show-default
  :sw-force-minimize
  (:sw-max 11))

(defcenum remove-msg
  (:pm-no-remove 0)
  (:pm-remove 1))

;; WGL
(defcstruct pixelformatdescriptor
  (size :int16)
  (version :int16)
  (flags :int32)
  (pixel-type :int8)
  (color-bits :int8)
  (red-bits :int8)
  (red-shift :int8)
  (green-bits :int8)
  (green-shift :int8)
  (blue-bits :int8)
  (blue-shift :int8)
  (alpha-bits :int8)
  (alpha-shift :int8)
  (accum-bits :int8)
  (accum-red-bits :int8)
  (accum-green-bits :int8)
  (accum-blue-bits :int8)
  (accum-alpha-bits :int8)
  (depth-bits :int8)
  (stencil-bits :int8)
  (aux-buffers :int8)
  (layer-type :int8)
  (reserved :int8)
  (layer-mask :int32)
  (visible-mask :int32)
  (damage-mask :int32))

(defbitfield pfd-flags
  (:pfd-draw-to-window 4)
  (:pfd-draw-to-bitmap 8)
  (:pfd-support-gdi 16)
  (:pfd-support-opengl 32)
  (:pfd-generic-accelerated #x00001000)
  (:pfd-generic-format 64)
  (:pfd-need-palette 128)
  (:pfd-need-system-palette #x00000100)
  (:pfd-double-buffer 1)
  (:pfd-stereo 2)
  (:pfd-swap-layer-buffers  #x00000800)
  (:pfd-depth-dont-care     #x20000000)
  (:pfd-double-buffer-dont-care #x40000000)
  (:pfd-stereo-dont-care #x80000000)
  (:pfd-swap-copy #x00000400)
  (:pfd-swap-exchange #x00000200))

(defcenum pfd-pixel-type
  (:pfd-type-rgba 0)
  (:pdf-type-color-index 1))

;; WIN32
(define-foreign-library user32
    (t (:default "user32")))
(use-foreign-library user32)

(defcfun ("GetDC" %get-dc) hdc
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

(defun dispatch-events (wnd)
  (with-foreign-object (msg 'msg)
    (when (> (%get-message msg wnd 0 0) 0)
      (%translate-message msg)
      (%dispatch-message msg))))

(defun dispatch-events-no-block (wnd)
  (with-foreign-object (msg 'msg)
    (when (%peek-message msg wnd 0 0 :pm-remove)
      (%translate-message msg)
      (%dispatch-message msg))))

(defcfun ("RegisterClassA" %register-class) :int16
  (wndclass :pointer))

(defcfun ("RegisterClassExA" %register-class-ex) :int16
  (wndclass-ex :pointer))


(defcfun ("UnregisterClassA" %unregister-class) bool
  (class-name :string) (instance hinstance))

(defcallback window-proc :long ((wnd hwnd) (msg :uint) (w-param wparam) (l-param lparam))
   (let ((msg-type (foreign-enum-keyword 'msg-type msg :errorp nil)))
     (case msg-type
       (:wm-close
        (format t "WM_CLOSE~%")
        (%destroy-window wnd)
        (return-from window-proc 0))
       (:wm-destroy
        (format t "WM_DESTROY~%")
        (%post-quit-message 0)
        (return-from window-proc 0))
       (:wm-mouse-move
        (format t "WM_MOUSEMOVE~%")
        (return-from window-proc 0))
       (:wm-paint
        (format t "WM_PAINT~%"))
       (:wm-lbutton-down
        (format t "WM_LBUTTONDOWN~%")
        (return-from window-proc 0))
       (:wm-lbutton-up
        (format t "WM_LBUTTONUP~%")
        (return-from window-proc 0))
       (:wm-rbutton-down
        (format t "WM_RBUTTONDOWN~%")
        (return-from window-proc 0))
       (:wm-rbutton-up
        (format t "WM_RBUTTONUP~%")
        (return-from window-proc 0))
       (:wm-mbutton-down
        (format t "WM_MBUTTONDOWN~%")
        (return-from window-proc 0))
       (:wm-mbutton-up
        (format t "WM_MBUTTONUP~%")
        (return-from window-proc 0))
       (:wm-key-up
        (format t "WM_KEYUP~%")
        (return-from window-proc 0))
       (:wm-key-down
        (format t "WM_KEYDOWN~%")
        (return-from window-proc 0))
       (:wm-mouse-wheel
        (format t "WM_MOUSEWHEEL~%")
        (return-from window-proc 0))
       (:wm-size
        (format t "WM_SIZE~%")
        (return-from window-proc 0))
       (:wm-show-window
        (format t "WM_SHOWWINDOW~%"))
       (:wm-set-focus
        (format t "WM_SETFOCUS~%")
        (return-from window-proc 0)))
       ;; (:wm-sys-command
       ;;  (format t "WM_SYSCOMMAND~%")))
     (%def-window-proc wnd msg w-param l-param)))

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
      (format t "Error registering class ~S: ~S~%" name (%get-last-error)))))


(defcfun ("CreateWindowExA" %create-window-ex) hwnd
  (ex-style wex-style) (class-name :string) (win-name :string)
  (style wstyle) (x :int) (y :int) (width :int) (height :int)
  (parent hwnd) (menu hmenu) (instance hinstance) (param :pointer))

(defcfun ("DestroyWindow" %destroy-window) bool
  (wnd hwnd))

(defcfun ("UpdateWindow" %update-window) bool
  (wnd hwnd))

(defcfun ("ShowWindow" %show-window) bool
  (wnd hwnd) (cmd-show sw-cmd-show))

(defcfun ("SetForegroundWindow" %set-foreground-window) bool
  (wnd hwnd))

(defcfun ("SetFocus" %set-focus) hwnd
  (wnd hwnd))

(define-foreign-library kernel32
    (t (:default "kernel32")))
(use-foreign-library kernel32)

(defcfun ("GetModuleHandleW" %get-module-handle) hmodule
  (module-name :string))

(defcfun ("GetLastError" %get-last-error) :int32)

(define-foreign-library gdi32
    (t (:default "gdi32")))
(use-foreign-library gdi32)

(defcfun ("ChoosePixelFormat" %choose-pixel-format) :int
  (dc hdc) (pfd :pointer))

(defcfun ("SetPixelFormat" %set-pixel-format) bool
  (dc hdc) (pixel-format :int) (pfd :pointer))

(defun choose-pixel-format (dc)
  (with-foreign-object (pfd 'pixelformatdescriptor)
    (with-foreign-slots ((size version flags pixel-type color-bits
                               depth-bits) pfd pixelformatdescriptor)
      (setf size (foreign-type-size 'pixelformatdescriptor)
            version 1
            flags (foreign-bitfield-value 'pfd-flags
                       '(:pfd-draw-to-window :pfd-support-opengl :pfd-double-buffer))
            pixel-type (foreign-enum-value 'pfd-pixel-type :pfd-type-rgba)
            color-bits 32
            depth-bits 16))
    (let ((fmt (%choose-pixel-format dc pfd)))
      (%set-pixel-format dc fmt pfd)
      fmt)))

(defcfun ("SwapBuffers" %swap-buffers) bool
  (dc hdc))

;; WGL
(define-foreign-library opengl
  (t (:default "opengl32")))
(use-foreign-library opengl)

(defctype hglrc handle)

(defcfun ("wglCreateContext" %wgl-create-context) hglrc
  (dc hdc))

(defcfun ("wglMakeCurrent" %wgl-make-current) bool
  (dc hdc) (rc hglrc))

(defcfun ("wglDeleteContext" %wgl-delete-context) bool
  (rc hglrc))

;; test
(defvar *running* t)


;; GLOP
(in-package #:glop)

(defstruct system-window
  module-instance
  class-name
  pixel-format
  dc
  id)

(defstruct gl-context
  wgl-context)

(defun create-gl-context (win)
  (let ((ctx (make-gl-context)))
    (let ((wgl-ctx (glop-win32::%wgl-create-context (system-window-dc win))))
      (unless wgl-ctx
        (format t "Error creating GL context: ~S~%" (glop-win32::%get-last-error)))
      (setf (gl-context-wgl-context ctx) wgl-ctx))
    (format t "Created gl context: ~S~%" ctx)
    ctx))

(defun set-gl-context (ctx win)
  (glop-win32::%wgl-make-current (system-window-dc win) (gl-context-wgl-context ctx)))

(defun destroy-gl-context (ctx)
  (glop-win32::%wgl-delete-context (gl-context-wgl-context ctx)))

(defun swap-gl-buffers (win)
  (glop-win32::%swap-buffers (system-window-dc win)))

(defun create-system-window (title width height)
  (let ((win (make-system-window
              :module-instance (glop-win32::%get-module-handle (cffi:null-pointer)))))
    ;; create window class
    (glop-win32::create-and-register-class (system-window-module-instance win) "OpenGL")
    (setf (system-window-class-name win) "OpenGL")
    (let ((wnd (glop-win32::%create-window-ex '(:ws-ex-app-window :ws-ex-window-edge)
                                  "OpenGL"
                                  title
                                  '(:ws-overlapped-window :ws-clip-siblings :ws-clip-children)
                                  0 0 width height (cffi:null-pointer) (cffi:null-pointer)
                                  (system-window-module-instance win) (cffi:null-pointer))))
      (unless wnd
        (format t "Error creating window: ~S~%" (glop-win32::%get-last-error))
        (return-from create-system-window))
      (setf (system-window-id win) wnd))
    (setf (system-window-dc win) (glop-win32::%get-dc (system-window-id win)))
    ;; choose pixel format
    (setf (system-window-pixel-format win) (glop-win32::choose-pixel-format
                                            (system-window-dc win)))
    ;; show window
    (glop-win32::%show-window (system-window-id win) :sw-show)
    (glop-win32::%set-foreground-window (system-window-id win))
    (glop-win32::%set-focus (system-window-id win))
    (glop-win32::%update-window (system-window-id win))
    win))



(defun destroy-system-window (win)
  (glop-win32::%unregister-class (system-window-class-name win)
                                 (system-window-module-instance win)))


(defun dispatch-system-window-events (win &key (blocking nil))
  (if blocking
      (glop-win32::dispatch-events (system-window-id win))
      (glop-win32::dispatch-events-no-block (system-window-id win))))