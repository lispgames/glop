(in-package #:glop)

(defstruct (video-mode
	     (:constructor make-video-mode (width height depth)))
  (width 0 :type integer)
  (height 0 :type integer)
  (depth 0 :type integer))

;; base window structure
;; all implementations should inherit from it
(defstruct window
  width
  height
  title
  gl-context
  pushed-event
  (fullscreen nil)
  (previous-video-mode nil))

;;; Keyboard stuff
(defvar *ignore-auto-repeat* nil
  "When set to NIL, holding a key press will generate a sequence of key-press events.
Otherwise, only one key-press event will be triggered.")

(defvar %key-states% (make-array #xffff :initial-element nil))

(defun key-pressed (keycode)
  (aref %key-states% keycode))

(defun %set-key-pressed (keycode value)
  (setf (aref %key-states% keycode) value))

(defsetf key-pressed %set-key-pressed)

;; Helper macros from bordeaux-threads
;; http://common-lisp.net/project/bordeaux-threads/
(defmacro defdfun (name args doc &body body)
  `(progn
     ,(unless (fboundp name)
       `(defun ,name ,args ,@body))
     (setf (documentation ',name 'function)
           (or (documentation ',name 'function) ,doc))))

(defmacro defdmacro (name args doc &body body)
  `(progn
     ,(unless (fboundp name)
       `(defmacro ,name ,args ,@body))
     (setf (documentation ',name 'function)
           (or (documentation ',name 'function) ,doc))))

;;; Execute BODY with floating-point traps disabled. This seems to be
;;; necessary on (at least) Linux/x86-64 where SIGFPEs are signalled
;;; when creating making a GLX context active.
#+(and sbcl x86-64)
(defmacro without-fp-traps (&body body)
 `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
 ,@body))

;;; Do nothing on Lisps that don't need traps disabled.
#-(and sbcl x86-64)
(defmacro without-fp-traps (&body body)
 `(progn ,@body))

;; Glop's conditions
(define-condition glop-error (error)
  () (:documentation "Any glop specific error should inherit this."))

(define-condition not-implemented (glop-error)
  () (:documentation "Unimplemented."))

;; misc.
(defun load-libraries ()
  #+unix(progn (cffi:define-foreign-library xlib
                   (t (:default "libX11")))
               (cffi:use-foreign-library xlib)
               (cffi:define-foreign-library opengl
                   (t (:default "libGL")))
               (cffi:use-foreign-library opengl))
  #+win32(progn (cffi:define-foreign-library user32
                    (t (:default "user32")))
                (cffi:use-foreign-library user32)
                (cffi:define-foreign-library kernel32
                    (t (:default "kernel32")))
                (cffi:use-foreign-library kernel32)
                (cffi:define-foreign-library opengl
                    (t (:default "opengl32")))
                (cffi:use-foreign-library opengl)
                (cffi:define-foreign-library gdi32
                    (t (:default "gdi32")))
                (cffi:use-foreign-library gdi32)))


