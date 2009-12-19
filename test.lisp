(defpackage :glop-test
  (:use #:cl)
  (:export #:test-manual-create #:test-with-window))

(in-package #:glop-test)

(defmethod glop:on-key (window state key)
  (format t "Key: ~S~%" key)
  (when (eql key #\Escape)
    (glop:push-close-event window)))

(defmethod glop:on-button (window state button)
  (format t "Button: ~S~%" button))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (format t "Mouse motion !!~%"))

(defmethod glop:on-resize (window)
  (format t "Resize !!~%"))

(defmethod glop:on-draw (window)
  (format t "Draw !!~%"))

(defmethod glop:on-close (window)
  (format t "Close !!~%"))


(defun test-manual-create ()
  (let ((win (glop:create-window "Glop test window" 800 600)))
    (format t "Created window: ~S~%" win)
    (gl:clear-color 0.3 0.3 1.0 0)
    (loop while (glop:dispatch-events win :blocking nil) do
         (gl:clear :color-buffer)
         (gl:flush)
         (glop:swap-buffers win))
    (glop:destroy-window win)))


(defun test-with-window ()
  (glop:with-window (win "Glop test window" 800 600)
    (format t "Created window: ~S~%" win)
    (gl:clear-color 0.3 0.3 1.0 0)
    (loop while (glop:dispatch-events win :blocking nil) do
         (gl:clear :color-buffer)
         (gl:flush)
         (glop:swap-buffers win))))
