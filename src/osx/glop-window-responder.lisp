(in-package :glop-bridge)

(defcfun ("GlopWindowResponderInit" glop-window-responder-init) :pointer
  (callback :pointer))