;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage :glop-test
  (:use #:cl)
  (:export #:test-manual-create #:test-multiple-contexts #:test-with-window #:test-manual-events
           #:test-gl-hello #:test-gl-hello-fullscreen #:test-gl-hello-gl3 #:test-multiple-windows
           #:test-on-event #:test-subclassing
           #+(and unix (not darwin))#:test-custom-event-loop))

(in-package #:glop-test)

(defmethod glop:on-key (window pressed keycode keysym text)
  (format t "Key ~:[released~;pressed~]: ~D (~S ~S)~%" pressed keycode keysym text)
  (format t "Key pressed: ~S~%" (glop:key-pressed keycode))
  (when (and (not pressed) (eq keysym :escape))
    (glop:push-close-event window))
  (case keysym
    (:h (glop:hide-cursor window))
    (:j (glop:show-cursor window))
    (:left (decf (glop:window-x window)))
    (:right (incf (glop:window-x window)))
    (:up (decf (glop:window-y window)))
    (:down (incf (glop:window-y window)))
    (:page-up (progn (incf (glop:window-width window) 10)
                     (incf (glop:window-height window) 10)))
    (:page-down (progn (decf (glop:window-width window) 10)
                       (decf (glop:window-height window) 10))))
  (when (and (not pressed) (eq keysym :f))
    (glop:toggle-fullscreen window))
  (when (and (not pressed) (eq keysym :g))
    (glop:set-fullscreen window)))

(defmethod glop:on-button (window pressed button)
  (declare (ignore window))
  (format t "Button ~:[released~;pressed~]: ~S~%" pressed button))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (declare (ignore window x y dx dy))
  (format t "Mouse motion~%"))

(defmethod glop:on-resize (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 w h)
  (format t "Resize: ~Sx~S~%" w h))

(defmethod glop:on-draw (window)
  (declare (ignore window))
  (format t "Draw~%"))

(defmethod glop:on-close (window)
  (declare (ignore window))
  (format t "Close~%"))

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
;; events. This is done so we can avoid artificial delay in event processing due to events
;; accumulating on the system side (at least on X11) while rendering.
(defun test-manual-events ()
  (let ((win (glop:create-window "Glop test window" 800 600)))
    (format t "Created window: ~S~%" win)
    (gl:clear-color 0.3 0.3 1.0 0)
    (loop for evt = (glop:next-event win :blocking nil)
         with running = t
         while running
         if evt
         do (typecase evt
              (glop:key-press-event
               (when (eq (glop:keysym evt) :escape)
                 (glop:push-close-event win)))
              (glop:close-event (setf running nil))
              (t (format t "Unhandled event: ~A~%" evt)))
         else do (gl:clear :color-buffer)
                 (gl:flush)
                 (glop:swap-buffers win))
    (glop:destroy-window win)))

;; How to completely replace glop's event management on X11
;; Note that this uses non-exported stuff from glop-xlib
;; This is just provided as an example and you should use your own
;; platform specific event code
;; Necessary data (window handle etc) are probably not exported for the moment
#+(and unix (not darwin))
(defun test-custom-event-loop ()
  (let ((win (glop:create-window "Glop test window" 800 600)))
    (format t "Created window: ~S~%" win)
    (gl:clear-color 0.3 0.3 1.0 0)
    (loop with running = t
       with dpy = (glop::x11-window-display win)
       for x-evt = nil
       while running
       do (when (glop-xlib::x-pending-p dpy)
            (setf x-evt (cffi:foreign-alloc 'glop-xlib::x-event))
            (glop-xlib::%x-next-event dpy x-evt))
       if x-evt
       do (let ((evt (glop-xlib::process-event win dpy x-evt)))
            (typecase evt
              (glop:key-press-event
               (when (eq (glop:keysym evt) :escape)
                 (setf running nil)))
              (glop:close-event
               (setf running nil))
              (t (format t "Unhandled event: ~A~%" evt)))
            (cffi:foreign-free x-evt))
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
               (progn
                 (glop:set-gl-window win)
                 (gl:clear :color-buffer)
                 (gl:color 1 1 1)
                 (gl:with-primitive :polygon
                  (gl:vertex 0.25 0.25 0)
                  (gl:vertex 0.75 0.25 0)
                  (gl:vertex 0.75 0.75 0)
                  (gl:vertex 0.25 0.75 0))
                (gl:flush)
                (glop:swap-buffers win))
               (progn
                 (setf windows (remove win windows))
                 (glop:destroy-window win))))))))

;; on-event based dispatching test
(defmethod glop:on-event (window (event glop:key-event))
  (format t "Key ~:[released~;pressed~]: ~A~%" (glop:pressed event) (glop:keysym event))
  (when (eq (glop:keysym event) :escape)
      (glop:push-close-event window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :f))
    (glop:toggle-fullscreen window)))

(defmethod glop:on-event (window (event glop:button-event))
  (declare (ignore window))
  (format t "Button ~:[released~;pressed~]: ~S~%" (glop:pressed event)
                                                  (glop:button event)))

(defmethod glop:on-event (window (event glop:mouse-motion-event))
  (declare (ignore window event))
  (format t "Mouse motion~%"))

(defmethod glop:on-event (window (event glop:resize-event))
  (declare (ignore window))
   (gl:viewport 0 0 (glop:width event) (glop:height event))
   (format t "Resize: ~Sx~S~%" (glop:width event) (glop:height event)))

(defmethod glop:on-event (window (event glop:expose-event))
  (declare (ignore window event))
  (format t "Expose~%"))

(defmethod glop:on-event (window (event glop:close-event))
  (declare (ignore window event))
  (format t "Close~%"))

(defun test-on-event ()
  (glop:with-window (win "Glop test window" 800 600)
    (format t "Created window: ~S~%" win)
    ;; GL init
    (gl:clear-color 0.3 0.3 1.0 0)
    ;; setup view
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -1 1)
    ;; idle loop, we draw here anyway
    (loop while (glop:dispatch-events win :blocking nil :on-foo nil) do
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


;; window subclassing test
(defclass my-window (glop:window)
  ((data :initform "This is my own window class !!!" :accessor my-window-data)))

(defmethod glop:on-event ((window my-window) (event glop:key-event))
  (format t "Window data: ~S~%" (my-window-data window))
  (format t "Key ~:[released~;pressed~]: ~A~%" (glop:pressed event) (glop:keysym event))
  (when (eq (glop:keysym event) :escape)
      (glop:push-close-event window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :f))
    (glop:toggle-fullscreen window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :g))
    (glop:set-fullscreen window)))

(defmethod glop:on-event ((window my-window) (event glop:button-event))
  (format t "Window data: ~S~%" (my-window-data window))
  (format t "Button ~:[released~;pressed~]: ~S~%" (glop:pressed event)
                                                  (glop:button event)))

(defmethod glop:on-event ((window my-window) (event glop:mouse-motion-event))
  (declare (ignore event))
  (format t "Window data: ~S~%" (my-window-data window))
  (format t "Mouse motion~%"))

(defmethod glop:on-event ((window my-window) (event glop:resize-event))
  (format t "Window data: ~S~%" (my-window-data window))
  (gl:viewport 0 0 (glop:width event) (glop:height event))
  (format t "Resize: ~Sx~S~%" (glop:width event) (glop:height event)))

(defmethod glop:on-event ((window my-window) (event glop:expose-event))
  (declare (ignore event))
  (format t "Window data: ~S~%" (my-window-data window))
  (format t "Expose~%"))

(defmethod glop:on-event ((window my-window) (event glop:close-event))
  (declare (ignore event))
  (format t "Window data: ~S~%" (my-window-data window))
  (format t "Close~%"))

(defun test-subclassing ()
  (glop:with-window (win "Glop test window" 800 600 :win-class 'my-window)
    (format t "Created window: ~S~%" win)
    ;; GL init
    (gl:clear-color 0.3 0.3 1.0 0)
    ;; setup view
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -1 1)
    ;; idle loop, we draw here anyway
    (loop while (glop:dispatch-events win :blocking nil :on-foo nil) do
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


;; xinput2 testi

(defclass xi2-window (glop:window)
  ())

(defmethod glop:on-event ((window xi2-window) (event glop:mouse-motion-event))
  (declare (ignore window event))
  )

(defmethod glop:on-event ((window xi2-window) (event glop::extended-mouse-motion-event))
  (declare (ignore window event))
  )

(defmethod glop:on-event ((window xi2-window) (event glop:resize-event))
  (declare (ignore window))
   (gl:viewport 0 0 (glop:width event) (glop:height event))
   (format t "Resize: ~Sx~S~%" (glop:width event) (glop:height event)))

(defmacro with-continue-restart (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue"  )))

(defun test-xinput-2 ()
  (glop:with-window (win "Glop test window" 800 600 :win-class 'xi2-window)
    (format t "~&Created window: ~S~%" win)
    (format t "XInput-p : ~s~%"
            (multiple-value-list
             (glop-xlib::x-query-extension (glop::x11-window-display win)
                                           "XInputExtension")))
    (format t "XInput version : ~s~%"
            (multiple-value-list
             (glop-xlib::xi-query-version (glop::x11-window-display win)
                                          2 0)))
    (when (glop-xlib::xi-query-version (glop::x11-window-display win)
                                       2 0)
      (format t "select events = ~s~%"
              (glop-xlib::xi-select-events (glop::x11-window-display win)
                                           (glop::x11-window-id win)
                                           :all-devices
                                           :xi-button-press
                                           :xi-motion
                                           :xi-key-press
                                           :xi-hierarchy-changed)))
    (with-continue-restart
     (glop-xlib::xi-query-device (glop::x11-window-display win)
                                 glop-xlib::+xi-all-devices+))

    (gl:clear-color 0.3 0.3 1.0 0)
    (loop while (glop:dispatch-events win :blocking nil :on-foo nil)
       do
         (with-continue-restart
           (sleep 0.005)
           (gl:clear :color-buffer)
           (gl:flush)
           (glop:swap-buffers win)))
    (format t "done~%")))

#++
(test-xinput-2)


