(in-package #:glop)

;; (defstruct base-window
;;   width
;;   height
;;   title
;;   context)

;; (defgeneric create-window (title width height &key (fullscreen nil))
;;   (:documentation "Creates a new GL window."))

;; (defgeneric destroy-window (window)
;;   (:documentation "Destroy the provided GL window."))

;; (defgeneric dispatch-events (window)
;;   (:documentation "Dispatch pending window events."))


(defun test ()
  ;; Create a window
  (let ((win (create-system-window "GLOP Test Window" 800 600)))
    ;; Create GL context
    (let ((context (create-gl-context win)))
      (set-gl-context context win)
      (gl:clear-color 0.3 0.3 1.0 0)
      (gl:shade-model :flat)
      (loop while t
         do (progn (gl:clear :color-buffer)
                   (gl:flush)
                   (dispatch-system-window-events win :blocking nil)
                   (swap-gl-buffers win))))
    (destroy-system-window win)))