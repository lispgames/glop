;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage :glop-xlib
  (:use #:cl #:cffi)
  (:export #:visual-info #:bool #:drawable
           #:x-open-display #:x-create-window #:x-default-root-window
           #:x-store-name #:x-flush #:x-map-raised #:x-unmap-window
           #:x-destroy-window #:x-close-display #:x-next-event
           #:x-free #:x-intern-atom #:x-set-wm-protocols
           #:%set-fullscreen #:closest-mode
           #:x-set-geometry
           #:x-create-null-cursor #:x-define-cursor #:x-undefine-cursor
           #:x-free-cursor
           #:current-mode #:set-mode #:supported-modes
           #:xkb-set-detectable-auto-repeat))
