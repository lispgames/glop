;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(in-package #:glop-win32)


(cffi:define-foreign-library dwm
  (:windows "Dwmapi.dll"))

(cffi:use-foreign-library dwm)

(cffi:defcfun ("DwmFlush" dwm-flush) :int)

(cffi:defcfun ("DwmIsCompositionEnabled" %dwm-is-composition-enabled) :int32
  (enabled (:pointer bool)))

(defun dwm-is-composition-enabled ()
  (with-foreign-object (p 'bool)
    (let ((hr (%dwm-is-composition-enabled p)))
      (if (zerop hr)
          (not (zerop (mem-ref p 'bool)))
          (error "dwm-is-composition-enabled failed 0x~x~%" hr)))))
