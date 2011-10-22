(in-package :glop-bridge)

(defcenum glop-notice-type
  :window-close
  :window-resize)

(defcstruct glop-notice-struct
  (type glop-notice-type)
  (source :pointer))

(define-foreign-type glop-notice-type ()
  ()
  (:actual-type glop-notice-struct)
  (:simple-parser glop-notice))

(defmethod translate-from-foreign (value (type glop-notice-type))
  (with-foreign-slots ((type source) value glop-notice-struct)
    (list :type type
          :source source)))

(defcfun ("GlopViewInit" glop-view-init) :pointer
  (event-callback :pointer)
  (notice-callback :pointer))