;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage :glop-xlib
  (:use #:cl #:cffi)
  (:export #:visual-info #:bool #:drawable
           #:x-open-display #:x-create-window #:x-default-root-window
           #:default-screen
           #:x-store-name #:x-flush #:x-map-raised #:x-unmap-window
           #:x-destroy-window #:x-close-display #:x-next-event
           #:x-free #:x-intern-atom #:x-set-wm-protocols
           #:%set-fullscreen #:closest-mode
           #:x-set-geometry
           #:x-create-null-cursor #:x-define-cursor #:x-undefine-cursor
           #:x-free-cursor
           #:current-mode #:set-mode #:supported-modes
           #:xkb-set-detectable-auto-repeat
           #:add-connection-watch
           #:remove-connection-watch
           #:process-internal-connection))

(defpackage :glop-glx
  (:use #:cl #:cffi #:glop-xlib)
  (:export #:glx-get-proc-address #:correct-context? #:glx-destroy-context
           #:glx-create-specific-context #:glx-create-context
           #:glx-get-version
           #:glx-make-current #:glx-release-context #:glx-choose-fb-config
           #:glx-get-visual-from-fb-config #:glx-choose-visual
           #:glx-wait-gl #:glx-swap-buffers))
