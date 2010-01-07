;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage :glop
  (:use #:cl)
  (:export
   ;; GL
   #:create-gl-context #:destroy-gl-context #:attach-gl-context #:detach-gl-context
   #:gl-get-proc-address
   ;; window
   #:create-window #:destroy-window #:show-window #:hide-window #:set-window-title
   #:swap-buffers
   #:window-width #:window-height #:window-gl-context #:set-fullscreen
   ;; events
   #:on-event #:next-event #:push-event #:push-close-event
   #:event #:key-event #:key-press-event #:key-release-event
   #:button-event #:button-press-event #:button-release-event
   #:mouse-motion-event #:expose-event #:configure-event
   #:map-event #:map-in-event #:map-out-event #:close-event
   #:event-pressed #:event-mapped #:event-width #:event-height #:event-key #:event-button
   #:event-x #:event-y #:event-dx #:event-dy
   ;; events methods
   #:dispatch-events
   #:on-key #:on-button #:on-mouse-motion #:on-resize #:on-draw #:on-close
   ;; helper macros
   #:with-window #:with-idle-forms
   ;; multiple windows
   #:set-gl-window))


