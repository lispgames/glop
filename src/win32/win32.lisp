;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

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

(defctype bool :int) ;; XXX: Win32 BOOL isn't used as a boolean (e.g.: see GetMessage)

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
  (:l-button 1)
  :r-button
  :cancel
  :m-button
  :x-button1
  :x-button2
  :backspace
  :tab
  :clear
  :return
  (:shift 16)
  (:control 17)
  :menu
  :pause
  :capital
  (:kana #x15)
  (:junja #x17)
  (:final #x18)
  (:hanja #x19)
  (:escape #x1B)
  (:convert #x1C)
  (:no-convert #x1D)
  (:accept #x1E)
  (:mode-change #x1F)
  (:space 32)
  (:page-up 33)
  (:page-down 34)
  (:end 35)
  (:home 36)
  (:left 37)
  (:up 38)
  (:right 39)
  (:down 40)
  (:select 41)
  (:print 42)
  (:execute 43)
  (:snapshot 44)
  (:insert 45)
  (:delete 46)
  (:help 47)
  (:0 #x30)
  :1
  :2
  :3
  :4
  :5
  :6
  :7
  :8
  :9
  (:a #x41)
  :b
  :c
  :d
  :e
  :f
  :g
  :h
  :i
  :j
  :k
  :l
  :m
  :n
  :o
  :p
  :q
  :r
  :s
  :t
  :u
  :v
  :w
  :x
  :y
  :z
  (:lwin #x5B)
  (:rwin #x5C)
  (:apps #x5D)
  (:sleep #x5F)
  (:numpad0 #x60)
  (:numpad1 #x61)
  (:numpad2 #x62)
  (:numpad3 #x63)
  (:numpad4 #x64)
  (:numpad5 #x65)
  (:numpad6 #x66)
  (:numpad7 #x67)
  (:numpad8 #x68)
  (:numpad9 #x69)
  (:multiply #x6A)
  (:add #x6B)
  (:separator #x6C)
  (:substract #x6D)
  (:decimal #x6E)
  (:divide #x6F)
  (:f1 #x70)
  :f2
  :f3
  :f4
  :f5
  :f6
  :f7
  :f8
  :f9
  :f10
  :f11
  :f12
  :f13
  :f14
  :f15
  :f16
  :f17
  :f18
  :f19
  :f20
  :f21
  :f22
  :f23
  :f24
  (:numlock #x90)
  :scroll
  (:shift-l #xA0)
  :shift-r
  :control-l
  :control-r
  :menu-l
  :menu-r
  :browser-back
  :browser-forward
  :browser-refresh
  :browser-stop
  :browser-search
  :browser-favorites
  :browser-home
  :volume-mute
  :volume-down
  :volume-up
  :media-next-track
  :media-prev-track
  :media-stop
  :media-play-pause
  :launch-mail
  :maunch-media-select
  :launch-app1
  :launch-app2
  :oem1
  :oem-plus
  :oem-comma
  :oem-minus
  :oem-period
  :oem2
  (:oem3 #xC0)
  (:oem4 #xDB)
  :oem5
  :oem6
  :oem7
  :oem8
  (:oem102 #xE2)
  (:process-key #xE5)
  (:packet #xE7)
  (:attn #xF6)
  :crsel
  :exsel
  :ereof
  :play
  :zoom
  :no-name
  :pa1
  :oem-clear)

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
  (values (foreign-enum-keyword 'vkey-type w-param :errorp nil)
          (with-foreign-object (kbd-state :char 256)
            (when (get-keyboard-state kbd-state)
              (with-foreign-object (buffer :int16)
                (let ((res (to-ascii (ldb (byte 32 0) w-param)
                                     (ldb (byte 32 0) l-param)
                                     kbd-state buffer 0)))
                  (case res
                    (0 nil)
                    (t (foreign-string-to-lisp buffer)))))))))


(let ((last-x 0)
      (last-y 0)
      (from-configure nil))
  (defcallback window-proc :long ((wnd hwnd) (msg :uint) (w-param wparam) (l-param lparam))
     (let ((msg-type (foreign-enum-keyword 'msg-type msg :errorp nil)))
       (case msg-type
         (:wm-close
          (setf %event% (glop::make-instance 'glop:close-event))
          (return-from window-proc 0))
         (:wm-destroy
          (%post-quit-message 0)
          (return-from window-proc 0))
         (:wm-mouse-move
          (let ((low (low-word l-param))
                (high (high-word l-param)))
            (when (or (/= low last-x) (/= high last-y))
              (setf %event% (glop::make-instance 'glop:mouse-motion-event
                                                 :x low :y high
                                                 :dx (- low last-x) :dy (- high last-y)))
              (setf last-x low last-y high))
            (return-from window-proc 0)))
         (:wm-paint
          ;; XXX: this is an ugly hack but WM_SIZE acts strangely...
          (multiple-value-bind (x y width height) (get-geometry wnd)
            (declare (ignore x y))
            (setf %event% (glop::make-instance (if from-configure
                                                   (progn (setf from-configure nil)
                                                          'glop:resize-event)
                                                      'glop:expose-event)
                                                :width width :height height))))
         (:wm-lbutton-down
          (set-capture wnd)
          (setf %event% (glop::make-instance 'glop:button-press-event
                                             :button 1))
          (return-from window-proc 0))
         (:wm-lbutton-up
          (release-capture)
          (setf %event% (glop::make-instance 'glop:button-release-event
                                             :button 1))
          (return-from window-proc 0))
         (:wm-rbutton-down
          (set-capture wnd)
          (setf %event% (glop::make-instance 'glop:button-press-event
                                             :button 3))
          (return-from window-proc 0))
         (:wm-rbutton-up
          (release-capture)
          (setf %event% (glop::make-instance 'glop:button-release-event
                                             :button 3))
          (return-from window-proc 0))
         (:wm-mbutton-down
          (set-capture wnd)
          (setf %event% (glop::make-instance 'glop:button-press-event
                                             :button 2))
          (return-from window-proc 0))
         (:wm-mbutton-up
          (release-capture)
          (setf %event% (glop::make-instance 'glop:button-release-event
                                             :button 2))
          (return-from window-proc 0))
         (:wm-key-up
          (multiple-value-bind (keysym text) (win32-lookup-key w-param l-param)
            (setf %event% (glop::make-instance 'glop:key-release-event
                                               :keycode  w-param
                                               :keysym keysym
                                               :text text)))
          (return-from window-proc 0))
         (:wm-key-down
          (multiple-value-bind (keysym text) (win32-lookup-key w-param l-param)
            (setf %event% (glop::make-instance 'glop:key-press-event
                                               :keycode  w-param
                                               :keysym keysym
                                               :text text)))
          (return-from window-proc 0))
         (:wm-mouse-wheel
          (setf %event% (glop::make-instance 'glop:button-press-event
                                             :button (if (> w-param 0)
                                                         4 5)))
          (return-from window-proc 0))
         (:wm-size
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
            (setf %event% (glop::make-instance (if (zerop w-param)
                                                   'glop:map-in-event
                                                   'glop:map-out-event)))))
         (:wm-set-focus
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
