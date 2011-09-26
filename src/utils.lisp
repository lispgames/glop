(in-package #:glop)

(defstruct video-mode
  (width 0 :type integer)
  (height 0 :type integer)
  (depth 0 :type integer)
  (rate 0 :type double-float))

;; platform specific windows
;; XXX: this may move to platform specific directories

#+(or win32 windows)
(defclass win32-window ()
  ((module-handle :initarg :module-handle :accessor win32-window-module-handle)
   (class-name :accessor win32-window-class-name)
   (pixel-format :accessor win32-window-pixel-format)
   (dc :accessor win32-window-dc)
   (id :accessor win32-window-id)))

#+(or win32 windows)
(defstruct (win32-video-mode (:include video-mode)))

#+(and unix (not darwin))
(defclass x11-window ()
  ((display :initarg :display :accessor x11-window-display)
   (screen :initarg :screen :accessor x11-window-screen)
   (id :accessor x11-window-id)
   (visual-infos :accessor x11-window-visual-infos)
   (fb-config :accessor x11-window-fb-config)))

#+(and unix (not darwin))
(defstruct (x11-video-mode (:include video-mode))
  (index -1 :type integer))

;; #+darwin
;; (defclass osx-window (window)
;;   ())

#+darwin
(defstruct (osx-video-mode (:include video-mode))
  mode)

;; base window structure
;; you may inherit your own window class from this
(defclass window (#+(and unix (not darwin)) x11-window
                  #+(or win32 windows) win32-window)
  ((x :initform 0 :initarg :x :accessor window-x)
   (y :initform 0 :initarg :y :accessor window-y)
   (width :initform 100 :initarg :width :accessor window-width)
   (height :initform 100 :initarg :height :accessor window-height)
   (title :initform "glop" :initarg :title :accessor window-title)
   (gl-context :accessor window-gl-context)
   (pushed-event :initform nil :accessor window-pushed-event)
   (fullscreen :initform nil :accessor window-fullscreen)
   (previous-video-mode :accessor window-previous-video-mode)))

(defun %update-geometry (win x y width height)
  (setf (slot-value win 'x) x
        (slot-value win 'y) y
        (slot-value win 'width) width
        (slot-value win 'height) height))

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
  #+(and unix (not darwin))
  (progn (cffi:define-foreign-library xlib
           (t (:default "libX11")))
         (cffi:use-foreign-library xlib)
         (cffi:define-foreign-library opengl
           (t (:or (:default "libGL")
                   "libGL.so.1"
                   "libGL.so.2")))
         (cffi:use-foreign-library opengl))
  #+(or win32 windows)
  (progn (cffi:define-foreign-library user32
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


