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
;;;                             NSApplication                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSApplicationSharedApplication" ns-application-shared-application)
    :void)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                NSWindow                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSWindowAllocInit" ns-window-alloc-init) :pointer
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun ("NSWindowSetBackgroundColor" ns-window-set-background-color) :void
  (window :pointer)
  (color :pointer))

(defcfun ("NSWindowMakeKeyAndOrderFront" ns-window-make-key-and-order-front)
    :void
  (window :pointer))

(defcfun ("NSWindowNextEvent" ns-window-next-event) :pointer
  (window :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 NSMenu                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSMenuAllocInit" ns-menu-alloc-init) :pointer)