(in-package #:glop-bridge)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                NSColor                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSColorBlackColor" ns-black-color) :pointer)
(defcfun ("NSColorBlueColor" ns-blue-color) :pointer)
(defcfun ("NSColorBrownColor" ns-brown-color) :pointer)
(defcfun ("NSColorCyanColor" ns-cyan-color) :pointer)
(defcfun ("NSColorDarkGrayColor" ns-dark-gray-color) :pointer)
(defcfun ("NSColorGrayColor" ns-gray-color) :pointer)
(defcfun ("NSColorGreenColor" ns-green-color) :pointer)
(defcfun ("NSColorLightGrayColor" ns-light-gray-color) :pointer)
(defcfun ("NSColorMagentaColor" ns-magenta-color) :pointer)
(defcfun ("NSColorOrangeColor" ns-orange-color) :pointer)
(defcfun ("NSColorPurpleColor" ns-purple-color) :pointer)
(defcfun ("NSColorRedColor" ns-red-color) :pointer)
(defcfun ("NSColorWhiteColor" ns-white-color) :pointer)
(defcfun ("NSColorYellowColor" ns-yellow-color) :pointer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                NSEvent                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcenum ns-event-type
  (:left-mouse-down 1)
  (:left-mouse-up 2)
  (:right-mouse-down 3)
  (:right-mouse-up 4)
  (:mouse-moved 5)
  (:left-mouse-dragged 6)
  (:right-mouse-dragged 7)
  (:mouse-entered 8)
  (:mouse-exited 9)
  (:key-down 10)
  (:key-up 11)
  (:flags-changed 12)
  (:app-kit-defined 13)
  (:system-defined 14)
  (:application-defined 15)
  (:periodic 16)
  (:cursor-update 17)
  (:scroll-wheel 22)
  (:tablet-point 23)
  (:tablet-proximity 24)
  (:other-mouse-down 25)
  (:other-mouse-up 26)
  (:other-mouse-dragged 27)
  (:gesture 29)
  (:magnify 30)
  (:swipe 31)
  (:rotate 18)
  (:begin-gesture 19)
  (:end-gesture 20))

(defcfun ("NSEventGetType" ns-event-type) ns-event-type
  (event :pointer))

(defcenum ns-key-code
  :a
  :s
  :d
  :f
  :h
  :g
  :z
  :x
  :c
  :v
  (:b 11)
  :q
  :w
  :e
  :r
  :y
  :t
  :1
  :2
  :3
  :4
  :6
  :5
  :equal
  :9
  :7
  :minus
  :8
  :0
  :bracket-right
  :o
  :u
  :bracket-left
  :i
  :p
  :return
  :l
  :j
  :quote
  :k
  (:semicolon 41)
  :backslash
  :comma
  :forwardslash
  :n
  :m
  :decimal
  :tab
  :space
  :grave
  (:backspace 51)
  (:escape 53)
  :super-r
  :super-l
  :shift-l
  :caps-lock
  :alt-l
  :ctrl-l
  :shift-r
  :alt-r
  :ctrl-r
  (:function 63)
  :f17
  :kp-decimal
  (:kp-multiply 67)
  (:kp-add 69)
  (:kp-divide 75)
  :kp-enter
  (:kp-subtract 78)
  :f18
  :f19
  :kp-equal
  :kp-0
  :kp-1
  :kp-2
  :kp-3
  :kp-4
  :kp-5
  :kp-6
  :kp-7
  :f20
  :kp-8
  :kp-9
  (:f5 96)
  :f6
  :f7
  :f3
  :f8
  :f9
  (:f11 103)
  (:f13 105)
  :f16
  :f14
  (:f10 109)
  (:f12 111)
  (:f15 113)
  :insert 
  :home
  :page-up
  :delete
  :f4
  :end
  :f2
  :page-down
  :f1
  :left
  :right
  :down
  :up)

(defcfun ("NSEventKeyCode" ns-event-key-code) :uint16
  (event :pointer))

(defun keysym (code)
  (let* ((key (foreign-enum-keyword 'ns-key-code code :errorp nil)))
    (if key key :unknown)))

(defbitfield ns-modifier-flags
  (:caps-lock #x10000)
  (:shift #x20000)
  (:control #x40000)
  (:alt #x80000)
  (:special #x100000)
  (:num-lock #x110000)
  (:help #x120000)
  (:function #x140000)
  (:device-independant #xffff0000))

(defcfun ("NSEventModifierFlags" ns-event-modifier-flags) ns-modifier-flags
  (event :pointer))

(defcfun ("NSEventWindow" ns-event-window) :pointer
  (event :pointer))

(defcfun ("NSEventLocationInWindow" ns-event-location-in-window) ns-point
  (event :pointer))

(defcfun ("NSEventMouseLocation" ns-event-mouse-location) ns-point)

(defcfun ("NSEventButtonNumber" ns-event-button-number) ns-integer
  (event :pointer))

(defcfun ("NSEventDeltaX" ns-event-delta-x) cg-float
  (event :pointer))

(defcfun ("NSEventDeltaY" ns-event-delta-y) cg-float
  (event :pointer))

(defcfun ("NSEventCharacters" ns-event-characters) ns-string
  (event :pointer))

(defcfun ("GlopSendNoticeEvent" glop-send-notice-event) :void
  (window :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             NSApplication                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSApplicationSharedApplication" ns-application-shared-application)
    :void)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                NSWindow                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defbitfield ns-window-style
  (:borderless 0)
  (:titled #x1)
  (:closable #x2)
  (:miniaturizable #x4)
  (:resizable #x8)
  (:textured-background #x80))

(defcenum ns-window-level
  (:normal #x0)
  (:floating #x3)
  (:submenu #x3)
  (:torn-off-menu #x3)
  (:main-menu #x18)
  (:status #x19)
  (:modal-panel #x8)
  (:pop-up-menu #x65)
  (:screen-saver #x3e8)
  (:dock #x14))

(defcfun ("NSWindowAllocInit" ns-window-alloc-init) :pointer
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun ("NSWindowSetTitle" ns-window-set-title) :void
  (window :pointer)
  (title ns-string))

(defcfun ("NSWindowSetBackgroundColor" ns-window-set-background-color) :void
  (window :pointer)
  (color :pointer))

(defcfun ("NSWindowMakeKeyAndOrderFront" ns-window-make-key-and-order-front)
    :void
  (window :pointer))

(defcfun ("NSWindowNextEvent" ns-window-next-event) :pointer
  (window :pointer))

(defcfun ("NSWindowSetReleasedWhenClosed" ns-window-set-released-when-closed)
    :void
  (window :pointer)
  (state :boolean))

(defcfun ("NSWindowClose" ns-window-close) :void
  (window :pointer))

(defcfun ("NSWindowOrderOut" ns-window-order-out) :void
  (window :pointer)
  (sender :pointer))

(defcfun ("NSWindowSetContentView" ns-window-set-content-view) :void
  (window :pointer)
  (view :pointer))

(defcfun ("NSWindowSetDelegate" ns-window-set-delegate) :void
  (window :pointer)
  (delegate :pointer))

(defcfun ("NSWindowSetNextResponder" ns-window-set-next-responder) :void
  (window :pointer)
  (delegate :pointer))

(defcfun ("NSWindowSetAcceptsMouseMovedEvents"
          ns-window-set-accepts-mouse-moved-events)
    :void
  (window :pointer)
  (accept-events :boolean))

(defcfun ("NSWindowDiscardRemainingEvents" ns-window-discard-remaining-events)
    :void
  (window :pointer))

(defcfun ("NSWindowContentView" ns-window-content-view) :pointer
  (window :pointer))

(defcfun ("NSWindowSetFrame" ns-window-set-frame) :void
  (window :pointer)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun ("NSWindowSetStyleMask" ns-window-set-style-mask) :void
  (window :pointer)
  (style ns-window-style))

(defcfun ("NSWindowSetLevel" ns-window-set-level) :void
  (window :pointer)
  (level ns-window-level))

(defcfun ("NSFrameMethod" ns-window-frame) ns-rect
  (window :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 NSView                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSFrameMethod" ns-view-frame) ns-rect
  (view :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 NSMenu                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSMenuAllocInit" ns-menu-alloc-init) :pointer
  (title ns-string))

(defcfun ("NSMenuAddItem" ns-menu-add-item) :void
  (menu :pointer)
  (item :pointer))

(defcfun ("NSMenuAddItemWithTitle" ns-menu-add-item-with-title) :void
  (menu :pointer)
  (title ns-string)
  (selector :pointer)
  (key-equiv ns-string))

(defcfun ("NSMenuItemAllocInit" ns-menu-item-alloc-init) :void
  (title ns-string)
  (selector :pointer)
  (key-equiv ns-string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      NSOpenGLPixelFormatAttribute                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcenum ns-opengl-pixel-format-attribute
  (:all-renderers 1)
  (:double-buffer 5)
  (:stereo 6)
  (:aux-buffers 7)
  (:color-size 8)
  (:alpha-size 11)
  (:depth-size 12)
  (:stencil-size 13)
  (:accum-size 14)
  (:minimum-policy 51)
  (:maximum-policy 52)
  (:off-screen 53)
  (:full-screen 54)
  (:sample-buffers 55)
  (:samples 56)
  (:aux-depth-stencil 57)
  (:color-float 58)
  (:multisample 59)
  (:supersample 60)
  (:sample-alpha 61)
  (:renderer-id 70)
  (:single-renderer 71)
  (:no-recovery 72)
  (:accelerated 73)
  (:closest-policy 74)
  (:robust 75)
  (:backing-store 76)
  (:mp-safe 78)
  (:window 80)
  (:multi-screen 81)
  (:compliant 83)
  (:screen-mask 84)
  (:pixel-buffer 90)
  (:remote-pixel-buffer 91)
  (:allow-offline-renderers 96)
  (:accelerated-compute 97)
  (:virtual-screen-count 128))

(defun list-to-pixel-format-attribs (list)
  (let ((pixel-format (foreign-alloc :uint32 :count (1+ (length list)))))
    (loop for arg in list
          for i upfrom 0
          do (setf (mem-aref pixel-format :uint32 i)
                   (if (typep arg 'keyword)
                       (foreign-enum-value 'ns-opengl-pixel-format-attribute
                                           arg)
                       arg))
          finally (setf (mem-aref pixel-format :uint32 (1+ i)) 0))
    pixel-format))

(defmacro with-pixel-format-attribs (pixel-format-var list &body body)
  `(let ((,pixel-format-var (list-to-pixel-format-attribs ,list)))
     (unwind-protect (progn ,@body)
       (foreign-free ,pixel-format-var))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           NSOpenGLPixelFormat                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSOpenGLPixelFormatInit" %ns-opengl-pixel-format-init) :pointer
  (attribs :pointer))

(defun ns-opengl-pixel-format-init (list)
  (with-pixel-format-attribs attribs list
    (%ns-opengl-pixel-format-init attribs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             NSOpenGLContext                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSOpenGLContextInit" ns-opengl-context-init) :pointer
  (format :pointer))

(defcfun ("NSOpenGLContextMakeCurrentContext"
          ns-opengl-context-make-current-context)
    :void
  (context :pointer))

(defcfun ("NSOpenGLContextSetView" ns-opengl-context-set-view) :void
  (context :pointer)
  (view :pointer))

(defcfun ("NSOpenGLContextSetFullScreen" ns-opengl-context-set-full-screen)
    :void
  (context :pointer))

(defcfun ("NSOpenGLContextClearDrawable" ns-opengl-context-clear-drawable) :void
  (context :pointer))

(defcfun ("NSOpenGLContextFlushBuffer" ns-opengl-context-flush-buffer) :void
  (context :pointer))

(defcfun ("NSOpenGLContextUpdate" ns-opengl-context-update) :void
  (context :pointer))