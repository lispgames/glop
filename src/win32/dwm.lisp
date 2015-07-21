;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(in-package #:glop-win32)


(cffi:define-foreign-library dwm
  (:windows "Dwmapi.dll"))

(cffi:use-foreign-library dwm)

(cffi:defcfun ("DwmFlush" dwm-flush) :int)
