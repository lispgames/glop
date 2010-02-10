(in-package #:glop-xlib)

(defcfun ("XkbSetDetectableAutoRepeat" xkb-set-detectable-auto-repeat) :boolean
  (display-ptr :pointer)
  (detectable :boolean)
  (supported-return :pointer))
