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

(defdfun toggle-fullscreen (window)
  "Switch display resolution to WINDOW dimensions and make WINDOW fullscreen."
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
  ((keycode :initargs :keycode :reader key)
   (text :initargs :character :reader text)
   (pressed :initargs :pressed :reader pressed))
  (:documentation "Keyboard key press or release."))

(defclass key-press-event (key-event)
  ((pressed :initform t))
  (:documentation "Keyboard key press."))

(defclass key-release-event (key-event)
  ((pressed :initform nil))
  (:documentation "Keyboard key release."))

(defclass button-event (event)
  ((button :initargs :button :reader button)
   (pressed :initargs :pressed :reader pressed))
  (:documentation "Mouse button press or release."))

(defclass button-press-event (button-event)
  ((pressed :initform t))
  (:documentation "Mouse button press."))

(defclass button-release-event (button-event)
  ((pressed :initform nil))
  (:documentation "Mouse button release."))

(defclass motion-event (event)
  ((x-pos :initargs :x-pos :reader x-pos)
   (y-pos :initargs :y-pos :reader y-pos)
   (x-delta :initargs :x-delta :reader x-delta)
   (y-delta :initargs :y-delta :reader y-delta))
  (:documentation "Mouse motion."))

(defclass expose-event (event)
  ((width :initargs :width :reader width)
   (height :initargs :height :reader height))
  (:documentation "Window expose."))

(defclass resize-event (event)
  ((width :initargs :width :reader width)
   (height :initargs :height :reader height))
  (:documentation "Window reconfiguration."))

(defclass map-event (event)
  ((mapped :initargs :mapped :reader mapped))
  (:documentation "Window mapped or unmapped."))

(defclass map-in-event (map-change-event)
  ((mapped :initform t))
  (:documentation "Window mapped in."))

(defclass map-out-event (map-change-event)
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
(defgeneric on-event (window event))

(defun dispatch-events (window &key blocking)
  "Process all pending system events and call corresponding methods.
When :blocking is non-nil calls event handling func that will block
until an event occurs.
Returns NIL on :CLOSE event, T otherwise."
  (loop for evt = (next-event window :blocking blocking)
    while evt
    do (on-event window evt)
    finally (return t)))


;; main loop anyone?
(defmacro with-idle-forms (window &body idle-forms)
  (let ((blocking (unless idle-forms t))
        (res (gensym)))
    `(loop with ,res = (dispatch-events ,window :blocking ,blocking)
        while ,res
        do ,(if idle-forms
                  `(progn ,@idle-forms)
                  t))))

(defmacro with-window ((win-sym title width height &key major minor fullscreen) &body body)
  "Creates a window and binds it to WIN-SYM.  The window is detroyed when body exits."
  `(let ((,win-sym (create-window ,title ,width ,height
                                  :major ,major :minor ,minor :fullscreen ,fullscreen)))
     (when ,win-sym
       (unwind-protect (progn ,@body)
         (destroy-window ,win-sym)))))

;; multiple windows management
(defun set-gl-window (window)
  "Make WINDOW current for GL rendering."
  (attach-gl-context window (window-gl-context window)))
