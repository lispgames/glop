(in-package :glop-bridge)

(defcfun ("GlopViewInit" glop-view-init) :pointer
  (event-callback :pointer))