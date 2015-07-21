;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage :glop-win32
  (:use #:cl #:cffi)
  (:export #:handle #:hdc #:bool
           #:get-last-error #:get-module-handle #:create-and-register-class
           #:create-window-ex #:get-dc #:choose-pixel-format #:set-foreground-window
           #:set-focus #:update-window #:show-window #:set-window-text
           #:set-geometry
           #:destroy-window #:unregister-class #:swap-buffers #:next-event
           #:set-video-mode #:show-cursor
           #:%event%
           #:adjust-window-rect-ex
           #:client-to-screen))

(defpackage :glop-wgl
  (:use #:cl #:cffi #:glop-win32)
  (:export #:wgl-get-proc-address #:wgl-create-context #:wgl-create-specific-context
           #:wgl-delete-context  #:wgl-make-current #:correct-context?))
