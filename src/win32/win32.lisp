;; Win32  bindings
(defpackage :glop-win32
  (:use #:cl #:cffi)
  (:export #:handle #:hdc #:bool
           #:get-last-error #:get-module-handle #:create-and-register-class
           #:create-window-ex #:get-dc #:choose-pixel-format #:set-foreground-window
           #:set-focus #:update-window #:show-window #:set-window-text
           #:destroy-window #:unregister-class #:swap-buffers #:next-event
           #:%event%))

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
  (:wm-char 258)
  (:wm-mouse-wheel 522)
  (:wm-size 5)
  (:wm-show-window 24)
  (:wm-set-focus 7)
  (:wm-sys-command 274))

(defcenum vkey-type
  (:key-up 38)
  (:key-down 40)
  (:key-left 37)
  (:key-right 39)
  (:key-prior 33)
  (:key-next 34)
  (:key-home 36)
  (:key-end 35)
  (:key-clear 12)
  (:key-insert 45)
  (:key-delete 46)
  (:key-f1 #x70)
  :key-f2
  :key-f3
  :key-f4
  :key-f5
  :key-f6
  :key-f7
  :key-f8
  :key-f9
  :key-f10
  :key-f11
  (:key-lshift #xA0)
  :key-rshift
  :key-lcontrol
  :key-rcontrol)

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

(defcstruct rect
  (left :long)
  (top :long)
  (right :long)
  (bottom :long))

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
;; We use the %event% var to allow window-proc callback to generate glop:event objects
;; that can be return from next-event

(defvar %event% nil)

(defun next-event (wnd &optional blocking)
  (with-foreign-object (msg 'msg)
    (if blocking
        (when (> (%get-message msg wnd 0 0) 0)
          (%translate-message msg)
          (%dispatch-message msg))
        (when (%peek-message msg wnd 0 0 :pm-remove)
          (%translate-message msg)
          (%dispatch-message msg))))
  %event%)

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
          (setf %event% (glop::make-event :type :close))
          (return-from window-proc 0))
         (:wm-destroy
          (%post-quit-message 0)
          (return-from window-proc 0))
         (:wm-mouse-move
          (let ((low (low-word l-param))
                (high (high-word l-param)))
            (when (or (/= low last-x) (/= high last-y))
              (setf %event% (glop::make-event :type :mouse-motion
                                              :x low :y high
                                              :dx (- low last-x) :dy (- high last-y)))
              (setf last-x low last-y high))
            (return-from window-proc 0)))
         (:wm-paint
          ;; XXX: this is an ugly hack but WM_SIZE acts strangely...
          (multiple-value-bind (x y width height) (get-geometry wnd)
            (setf %event% (glop::make-event :type (if from-configure
                                                      (progn (setf from-configure nil)
                                                             :configure)
                                                      :expose)
                                                :width width :height height))))
         (:wm-lbutton-down
          (set-capture wnd)
          (setf %event% (glop::make-event :type :button-press
                                          :button :left-button))
          (return-from window-proc 0))
         (:wm-lbutton-up
          (release-capture)
          (setf %event% (glop::make-event :type :button-release
                                          :button :left-button))
          (return-from window-proc 0))
         (:wm-rbutton-down
          (set-capture wnd)
          (setf %event% (glop::make-event :type :button-press
                                          :button :right-button))
          (return-from window-proc 0))
         (:wm-rbutton-up
          (release-capture)
          (setf %event% (glop::make-event :type :button-release
                                          :button :right-button))
          (return-from window-proc 0))
         (:wm-mbutton-down
          (set-capture wnd)
          (setf %event% (glop::make-event :type :button-press
                                          :button :middle-button))
          (return-from window-proc 0))
         (:wm-mbutton-up
          (release-capture)
          (setf %event% (glop::make-event :type :button-release
                                          :button :middle-button))
          (return-from window-proc 0))
         (:wm-key-up
          (let ((key (win32-lookup-key w-param l-param)))
            (when key
              (setf %event% (glop::make-event :type :key-release
                                              :key  key))))
          (return-from window-proc 0))
         (:wm-key-down
          (let ((key (win32-lookup-key w-param l-param)))
            (when key
              (setf %event% (glop::make-event :type :key-press
                                              :key  key))))
          (return-from window-proc 0))
         (:wm-mouse-wheel
          (format t "WM_MOUSEWHEEL: ~S => ~S~%" w-param (high-word w-param))
          (setf %event% (glop::make-event :type :button-press
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
            (setf %event% (glop::make-event :type (if (zerop w-param)
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
