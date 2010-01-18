;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(in-package #:glop)

;; Default implementation
(defdfun gl-get-proc-address (proc-name)
  "Get foreign pointer to the GL extension designed by PROC-NAME."
  (declare (ignore proc-name))
  (error 'not-implemented))

(defdfun create-gl-context (window &key make-current major minor)
  "Creates a new OpenGL context of the specified version for the provided window
   and optionally make it current. If major and minor are NIL old style context creation is
   used."
  (declare (ignore window make-current major minor))
  (error 'not-implemented))

(defdfun destroy-gl-context (ctx)
  "Detach and release the provided OpenGL context."
  (declare (ignore ctx))
  (error 'not-implemented))

(defdfun attach-gl-context (window ctx)
  "Makes CTX the current OpenGL context and attach it to WINDOW."
  (declare (ignore window ctx))
  (error 'not-implemented))

(defdfun detach-gl-context (ctx)
  "Make the provided OpenGL context no longer current."
  (declare (ignore ctx))
  (error 'not-implemented))

(defdfun create-window (title width height &key major minor
                                                double-buffer
                                                stereo
                                                red-size
                                                green-size
                                                blue-size
                                                alpha-size
                                                depth-size
                                                accum-buffer
                                                accum-red-size
                                                accum-green-size
                                                accum-blue-size
                                                stencil-buffer
                                                stencil-size)
  "Creates a new GL window using the provided visual attributes.
   Major and minor arguments specify the context version to use, when NIL
   (default value) old style gl context is created, otherwise framebuffer config
    based context creation is used."
  (declare (ignore title width height major minor double-buffer stereo red-size
                   green-size blue-size alpha-size depth-size accum-buffer accum-red-size
                   accum-green-size accum-blue-size stencil-buffer stencil-size))
  (error 'not-implemented))

(defdfun destroy-window (window)
  "Destroy the provided GL window."
  (declare (ignore window))
  (error 'not-implemented))

(defdfun set-fullscreen (window &optional (state (not (window-fullscreen win))))
  "Attempt to set WINDOW to fullscreen state STATE using the video mode closest to current geometry."
  (declare (ignore window))
  (error 'not-implemented))

(defdfun show-window (window)
  "Make WINDOW visible."
  (declare (ignore window))
  (error 'not-implemented))

(defdfun hide-window (window)
  "Make WINDOW not visible."
  (declare (ignore window))
  (error 'not-implemented))

(defdfun set-window-title (window title)
  "Set WINDOW title to TITLE."
  (declare (ignore window title))
  (error 'not-implemented))

(defdfun swap-buffers (window)
  "Swaps GL buffers."
  (declare (ignore window))
  (error 'not-implemented))

;;; Events handling
(defclass event () ()
  (:documentation "Common ancestor for all events."))

(defclass key-event (event)
  ((keycode :initarg :keycode :reader event-keycode)
   (keysym :initarg :keysym :reader event-keysym)
   (string :initarg :string :reader event-string)
   (pressed :initarg :pressed :reader event-pressed))
  (:documentation "Keyboard key press or release."))

(defclass key-press-event (key-event)
  ((pressed :initform t))
  (:documentation "Keyboard key press."))

(defclass key-release-event (key-event)
  ((pressed :initform nil))
  (:documentation "Keyboard key release."))

(defclass button-event (event)
  ((button :initarg :button :reader event-button)
   (pressed :initarg :pressed :reader event-pressed))
  (:documentation "Mouse button press or release."))

(defclass button-press-event (button-event)
  ((pressed :initform t))
  (:documentation "Mouse button press."))

(defclass button-release-event (button-event)
  ((pressed :initform nil))
  (:documentation "Mouse button release."))

(defclass mouse-motion-event (event)
  ((x :initarg :x :reader event-x)
   (y :initarg :y :reader event-y)
   (dx :initarg :dx :reader event-dx)
   (dy :initarg :dy :reader event-dy))
  (:documentation "Mouse motion."))

(defclass expose-event (event)
  ((width :initarg :width :reader event-width)
   (height :initarg :height :reader event-height))
  (:documentation "Window expose."))

(defclass configure-event (event)
  ((width :initarg :width :reader event-width)
   (height :initarg :height :reader event-height))
  (:documentation "Window reconfiguration."))

(defclass map-event (event)
  ((mapped :initarg :mapped :reader event-mapped))
  (:documentation "Window mapped or unmapped."))

(defclass map-in-event (map-event)
  ((mapped :initform t))
  (:documentation "Window mapped in."))

(defclass map-out-event (map-event)
  ((mapped :initform nil))
  (:documentation "Window unmapped."))

(defclass close-event (event) ()
  (:documentation "Window closed."))

(defun push-event (window evt)
  "Push an artificial event into the event processing system.
Note that this has no effect on the underlying window system."
  (setf (window-pushed-event window) evt))

(defun push-close-event (window)
  "Push an artificial :close event into the event processing system."
  (push-event window (make-instance 'close-event)))

(defdfun next-event (window &key blocking)
  "Returns next available event for manual processing.
If :blocking is true, wait for an event."
  (let ((pushed-evt (window-pushed-event window)))
    (if pushed-evt
        (progn (setf (window-pushed-event window) nil)
               pushed-evt)
        (%next-event window :blocking blocking))))

(defdfun %next-event (window &key blocking)
  "Real next-event implementation."
  (declare (ignore window blocking))
  (error 'not-implemented))

;; method based event handling
(defmacro dispatch-events (window &key blocking (on-foo t))
  "Process all pending system events and call corresponding methods.
When :blocking is non-nil calls event handling func that will block
until an event occurs.
Returns NIL on :CLOSE event, T otherwise."
  (let ((evt (gensym)))
  `(block dispatch-events
     (loop for ,evt = (next-event ,window :blocking ,blocking)
      while ,evt
      do ,(if on-foo
              `(typecase ,evt
                 (key-press-event (on-key ,window t (event-keycode ,evt) (event-keysym ,evt) (event-string ,evt)))
                 (key-release-event (on-key ,window nil (event-keycode ,evt) (event-keysym ,evt) (event-string ,evt)))
                 (button-press-event (on-button ,window t (event-button ,evt)))
                 (button-release-event (on-button ,window nil (event-button ,evt)))
                 (mouse-motion-event (on-mouse-motion ,window (event-x ,evt) (event-y ,evt)
                                                      (event-dx ,evt) (event-dy ,evt)))
                 (configure-event (on-resize ,window (event-width ,evt) (event-height ,evt)))
                 (expose-event (on-draw ,window))
                 (close-event (on-close ,window)
                              (return-from dispatch-events nil))
                 (t (format t "Unhandled event type: ~S~%" (type-of ,evt))))
              `(progn (on-event ,window ,evt)
                      (when (eql (type-of ,evt) 'close-event)
                        (return-from dispatch-events nil))))
      finally (return t)))))


;; implement this genfun when calling dispatch-events with :on-foo NIL
(defgeneric on-event (window event))

(defmethod on-event (window event)
  (declare (ignore window))
  (format t "Unhandled event: ~S~%" event))

;; implemented those when calling dispatch-events with :on-foo T
(defgeneric on-key (window pressed keycode keysym string))
(defgeneric on-button (window pressed button))
(defgeneric on-mouse-motion (window x y dx dy))
(defgeneric on-resize (window w h))
(defgeneric on-draw (window))
(defgeneric on-close (window))

;; main loop anyone?
(defmacro with-idle-forms (window &body idle-forms)
  (let ((blocking (unless idle-forms t))
        (res (gensym)))
    `(loop with ,res = (dispatch-events ,window :blocking ,blocking)
        while ,res
        do ,(if idle-forms
                  `(progn ,@idle-forms)
                  t))))

(defmacro with-window ((win-sym title width height &rest attribs) &body body)
  "Creates a window and binds it to WIN-SYM.  The window is detroyed when body exits."
  `(let ((,win-sym (apply #'create-window ,title ,width ,height
                          (list ,@attribs))))
     (when ,win-sym
       (unwind-protect (progn ,@body)
         (destroy-window ,win-sym)))))

;; multiple windows management
(defun set-gl-window (window)
  "Make WINDOW current for GL rendering."
  (attach-gl-context window (window-gl-context window)))
