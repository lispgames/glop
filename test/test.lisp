;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage :glop-test
  (:use #:cl)
  (:export #:test-manual-create #:test-multiple-contexts #:test-with-window #:test-manual-events
           #:test-gl-hello #:test-gl-hello-fullscreen #:test-gl-hello-gl3 #:test-multiple-windows))

(in-package #:glop-test)

(defmethod glop:on-key (window state key)
  (case state
    (:press (format t "Key pressed: ~S~%" key))
    (:release (format t "Key released: ~S~%" key)))
  (when (eql key #\Escape)
    (glop:push-close-event window)))

(defmethod glop:on-button (window state button)
  (declare (ignore window))
  (case state
    (:press (format t "Button pressed: ~S~%" button))
    (:release (format t "Button released: ~S~%" button))))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (declare (ignore window x y dx dy))
  (format t "Mouse motion !!~%"))

(defmethod glop:on-resize (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 w h)
  (format t "Resize: ~Sx~S !!~%" w h))

(defmethod glop:on-draw (window)
  (declare (ignore window))
  (format t "Draw !!~%"))

(defmethod glop:on-close (window)
  (declare (ignore window))
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

;; XXX: doesn't seem to work on win32...
(defun test-multiple-contexts ()
  (let ((win (glop:create-window "Glop test window" 800 600))
        (time (get-internal-real-time)))
    (format t "Created window: ~S~%" win)
    (gl:clear-color 0.3 0.3 1.0 0)
    (let ((ctx1 (glop::window-gl-context win))
          (ctx2 (glop:create-gl-context win :make-current t)))
      (gl:clear-color 1.0 0.3 0.3 0)
      (loop while (glop:dispatch-events win :blocking nil) do
           (gl:clear :color-buffer)
           (gl:flush)
           (glop:swap-buffers win)
         when (> (- (get-internal-real-time) time) (* 2.0 internal-time-units-per-second))
         do (progn (setf time (get-internal-real-time))
                   (if (eql (glop::window-gl-context win) ctx1)
                       (glop:attach-gl-context win ctx2)
                       (glop:attach-gl-context win ctx1)))))
    (glop:destroy-window win)))

(defun test-with-window ()
  (glop:with-window (win "Glop test window" 800 600)
    (format t "Created window: ~S~%" win)
    (gl:clear-color 0.3 0.3 1.0 0)
    (loop while (glop:dispatch-events win :blocking nil) do
         (gl:clear :color-buffer)
         (gl:flush)
         (glop:swap-buffers win))))


;; Note that priority is for event handling here and rendering is done when there's no more pending
;; events. This is done so we can avoid artificial delay in event processing dur to events
;; accumulating on the system side (at least on X11) while rendering.
(defun test-manual-events ()
  (let ((win (glop:create-window "Glop test window" 800 600)))
    (format t "Created window: ~S~%" win)
    (gl:clear-color 0.3 0.3 1.0 0)
    (loop for evt = (glop:next-event win :blocking nil)
         with running = t
         while running
         if evt
         do (case (glop:event-type evt)
              (:key-press
               (when (eql (glop:event-key evt) #\Escape)
                 (glop:push-close-event win)))
              (:close (setf running nil))
              (t (format t "Unhandled event: ~S~%" (glop:event-type evt))))
         else do (gl:clear :color-buffer)
                 (gl:flush)
                 (glop:swap-buffers win))
    (glop:destroy-window win)))


(defun test-gl-hello ()
  (glop:with-window (win "Glop test window" 800 600)
    (format t "Created window: ~S~%" win)
    ;; GL init
    (gl:clear-color 0.3 0.3 1.0 0)
    ;; setup view
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -1 1)
    ;; idle loop, we draw here anyway
    (loop while (glop:dispatch-events win :blocking nil) do
         ;; rendering
         (gl:clear :color-buffer)
         (gl:color 1 1 1)
         (gl:with-primitive :polygon
           (gl:vertex 0.25 0.25 0)
           (gl:vertex 0.75 0.25 0)
           (gl:vertex 0.75 0.75 0)
           (gl:vertex 0.25 0.75 0))
         (gl:flush)
         (glop:swap-buffers win))))

(defun test-gl-hello-fullscreen ()
  (defmethod glop:on-key :after (window state key)
    (format t "Key: ~S~%" key)
    (when (and (eql key #\f) (eql state :press))
      (glop:toggle-fullscreen window)))
  (glop:with-window (win "Glop test window" 800 600 :fullscreen t)
    (format t "Created window: ~S~%" win)
    ;; GL init
    (gl:clear-color 0.3 0.3 1.0 0)
    ;; setup view
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -1 1)
    ;; idle loop, we draw here anyway
    (loop while (glop:dispatch-events win :blocking nil) do
         ;; rendering
         (gl:clear :color-buffer)
         (gl:color 1 1 1)
         (gl:with-primitive :polygon
           (gl:vertex 0.25 0.25 0)
           (gl:vertex 0.75 0.25 0)
           (gl:vertex 0.75 0.75 0)
           (gl:vertex 0.25 0.75 0))
         (gl:flush)
         (glop:swap-buffers win))))

(defun test-gl-hello-gl3 (&optional (major 3) (minor 1))
  (glop:with-window (win "Glop test window" 800 600 :major major :minor minor)
    (format t "Created window: ~S~%" win)
    (format t "GL Context version: ~a~%" (gl:get-string :version))
    ;; GL init
    (gl:clear-color 0.3 0.3 1.0 0)
    ;; setup view
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -1 1)
    ;; idle loop, we draw here anyway
    (loop while (glop:dispatch-events win :blocking nil) do
         ;; rendering
         (gl:clear :color-buffer)
         (gl:color 1 1 1)
         (gl:with-primitive :polygon
           (gl:vertex 0.25 0.25 0)
           (gl:vertex 0.75 0.25 0)
           (gl:vertex 0.75 0.75 0)
           (gl:vertex 0.25 0.75 0))
         (gl:flush)
         (glop:swap-buffers win))))

(defun test-multiple-windows ()
  (let* ((window-1 (glop:create-window "window 1" 800 600))
         (window-2 (glop:create-window "window 2" 800 600))
         (windows (list window-1 window-2)))
    (when (and window-1 window-2)
      ;; setup first window
      (glop:set-gl-window window-1)
      (gl:clear-color 0.3 0.3 1.0 0)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 1 0 1 -1 1)
      ;; setup second window
      (glop:set-gl-window window-2)
      (gl:clear-color 1.0 0.3 0.3 0)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 1 0 1 -1 1)
      (loop while windows do
         (dolist (win windows)
           (if (glop:dispatch-events win :blocking nil)
               (cond
                 ((eq win window-1) ;; render in first window
                  (glop:set-gl-window window-1)
                  (gl:clear :color-buffer)
                  (gl:color 1 1 1)
                  (gl:with-primitive :polygon
                    (gl:vertex 0.25 0.25 0)
                    (gl:vertex 0.75 0.25 0)
                    (gl:vertex 0.75 0.75 0)
                    (gl:vertex 0.25 0.75 0))
                  (gl:flush)
                  (glop:swap-buffers window-1))
                 ((eq win window-2) ;; render in second window
                  (glop:set-gl-window window-2)
                  (gl:clear :color-buffer)
                  (gl:color 1 1 1)
                  (gl:with-primitive :polygon
                    (gl:vertex 0.25 0.25 0)
                    (gl:vertex 0.75 0.25 0)
                    (gl:vertex 0.75 0.75 0)
                    (gl:vertex 0.25 0.75 0))
                  (gl:flush)
                  (glop:swap-buffers window-2)))
               (cond
                 ((eq win window-1) (setf windows (remove window-1 windows))
                  (glop:destroy-window window-1))
                 ((eq win window-2) (setf windows (remove window-2 windows))
                  (glop:destroy-window window-2)))))))))