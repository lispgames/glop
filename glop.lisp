(in-package #:glop)

(defstruct window
  width
  height
  title
  gl-context)

(defgeneric create-gl-context (window &key make-current)
  (:documentation "Creates a new OpenGL context for the provided window and optionally
    make it current."))

(defgeneric destroy-gl-context (ctx)
  (:documentation "Detach and release the provided OpenGL context."))

(defgeneric attach-gl-context (window ctx)
  (:documentation "Makes CTX the current OpenGL context and attach it to WINDOW."))

(defgeneric detach-gl-context (ctx)
  (:documentation "Make the provided OpenGL context no longer current."))

(defgeneric create-window (title width height &key double-buffer accum alpha depth)
  (:documentation "Creates a new GL window using the provided visual attributes if possible."))

(defgeneric destroy-window (window)
  (:documentation "Destroy the provided GL window."))

(defgeneric show-window (window)
  (:documentation "Make WINDOW visible."))

(defgeneric hide-window (window)
  (:documentation "Make WINDOW not visible."))

(defgeneric set-window-title (window title)
  (:documentation "Set WINDOW title to TITLE."))

(defgeneric swap-buffers (window)
  (:documentation "Swaps GL buffers."))

(defstruct event
  type
  key     ;; for :key-press :key-release
  button  ;; for :button-press :button-release
  x y     ;; mouse position
  dx dy
)

(defgeneric next-event (window &key blocking)
  (:documentation "Returns next available event for manual processing.
   If :blocking is true wait until an event occur."))

;; method based event handling
(defun dispatch-events (window &key blocking)
  "Dispatch window events to corresponding methods.
   When :blocking is non-nil calls event handling func that will block
   until an event occur.
   Returns NIL on :CLOSE event type, T otherwise."
  (let ((evt (next-event window :blocking blocking)))
    (when evt
      (format t "Dispatching event: ~S~%" evt)
      (case (event-type evt)
        (:key-press (on-key window :press (event-key evt)))
        (:key-release (on-key window :release (event-key evt)))
        (:button-press (on-button window :press (event-button evt)))
        (:button-release (on-button window :release (event-button evt)))
        (:mouse-motion (on-mouse-motion window (event-x evt) (event-y evt)
                                        (event-dx evt) (event-dy evt)))
        (:resize (on-resize window))
        (:draw (on-draw window))
        (:close (on-close window)
                (return-from dispatch-events nil))))
    t))


(defgeneric on-key (window state key)
  (:method ((win window) state key)
    (format t "Key: ~S~%" key)))

(defgeneric on-button (window state button)
  (:method ((win window) state button)
    (format t "Button: ~S~%" button)))

(defgeneric on-mouse-motion (window x y dx dy)
  (:method ((win window) x y dx dy)
    (format t "Mouse motion !!~%")))

(defgeneric on-resize (window)
  (:method ((win window))
    (format t "Resize !!~%")))

(defgeneric on-draw (window)
  (:method ((win window))
    (format t "Draw !!~%")))

(defgeneric on-close (window)
  (:method ((win window))
    (format t "Close !!~%")))

;; main loop anyone?
(defmacro run-loop (window &body idle-forms)
  (let ((blocking (unless idle-forms t))
        (res (gensym)))
    `(loop with ,res = (dispatch-events window :blocking ,blocking)
        while ,res
        do ,(if idle-forms
                  `(progn ,@idle-forms)
                  t))))

;; Test func

(defmethod on-key (window state (key (eql #\Escape)))
  (destroy-window window))

(defun test ()
  (let ((win (create-window "GLOP Test Window" 800 600))
        (running t))
    (format t "Created window: ~S~%" win)
    (gl:clear-color 0.3 0.3 1.0 0)
    (loop while (dispatch-events win :blocking nil) do
         (gl:clear :color-buffer)
         (gl:flush)
         (swap-buffers win))
    (destroy-window win)))
