;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(in-package #:glop)

;; Default implementation
(defdfun gl-get-proc-address (proc-name)
  "Get foreign pointer to the GL extension designed by PROC-NAME."
  (error 'not-implemented))

(defdfun create-gl-context (window &key make-current major minor)
  "Creates a new OpenGL context of the specified version for the provided window
   and optionally make it current. If major and minor are NIL old style context creation is
   used."
  (error 'not-implemented))

(defdfun destroy-gl-context (ctx)
  "Detach and release the provided OpenGL context."
  (error 'not-implemented))

(defdfun attach-gl-context (window ctx)
  "Makes CTX the current OpenGL context and attach it to WINDOW."
  (error 'not-implemented))

(defdfun detach-gl-context (ctx)
  "Make the provided OpenGL context no longer current."
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
  (error 'not-implemented))

(defdfun destroy-window (window)
  "Destroy the provided GL window."
  (error 'not-implemented))

(defdfun toggle-fullscreen (window)
  "Switch display resolution to WINDOW dimensions and make WINDOW fullscreen."
  (error 'not-implemented))

(defdfun show-window (window)
  "Make WINDOW visible."
  (error 'not-implemented))

(defdfun hide-window (window)
  "Make WINDOW not visible."
  (error 'not-implemented))

(defdfun set-window-title (window title)
  "Set WINDOW title to TITLE."
  (error 'not-implemented))

(defdfun swap-buffers (window)
  "Swaps GL buffers."
  (error 'not-implemented))

;;; Events handling
(defstruct event
  type
  width height ;; for :configure :expose :show
  key          ;; for :key-press :key-release
  button       ;; for :button-press :button-release
  x y          ;; mouse position (all events)
  dx dy        ;; for :mouse-motion
)

(defun push-event (window evt)
  "Artificially push an event into the event processing system.
   The pushed event is internal to glaw and never makes it to the underlying
   system."
  (setf (window-pushed-event window) evt))

(defun push-close-event (window)
  "Push an artificial :close event into the event processing system."
  (push-event window (make-event :type :close)))

(defdfun next-event (window &key blocking)
  "Returns next available event for manual processing.
   If :blocking is true wait until an event occur."
  (let ((pushed-evt (window-pushed-event window)))
    (if pushed-evt
        (progn (setf (window-pushed-event window) nil)
               pushed-evt)
        (%next-event window :blocking blocking))))

(defdfun %next-event (window &key blocking)
  "Real next-event implementation."
  (error 'not-implemented))

;; method based event handling
(defun dispatch-events (window &key blocking)
  "Process all pending system events and call corresponding methods.
   When :blocking is non-nil calls event handling func that will block
   until an event occur.
   Returns NIL on :CLOSE event, T otherwise."
  (loop for evt = (next-event window :blocking blocking)
    while evt
    do  (case (event-type evt)
          (:key-press (on-key window :press (event-key evt)))
          (:key-release (on-key window :release (event-key evt)))
          (:button-press (on-button window :press (event-button evt)))
          (:button-release (on-button window :release (event-button evt)))
          (:mouse-motion (on-mouse-motion window (event-x evt) (event-y evt)
                                          (event-dx evt) (event-dy evt)))
          (:configure (on-resize window (event-width evt) (event-height evt)))
          (:expose (on-draw window))
          (:close (on-close window)
                  (return-from dispatch-events nil))
          (t (format t "Unhandled event type: ~S~%" (event-type evt))))
    finally (return t)))


(defgeneric on-key (window state key))
(defgeneric on-button (window state button))
(defgeneric on-mouse-motion (window x y dx dy))
(defgeneric on-resize (window w h))
(defgeneric on-draw (window))
(defgeneric on-close (window))

;;; Some helper macros

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
  `(let ((,win-sym (create-window ,title ,width ,height
                                  :major ,major :minor ,minor :fullscreen ,fullscreen)))
     (when ,win-sym
       (unwind-protect (progn ,@body)
         (destroy-window ,win-sym)))))
