(defpackage :glop
  (:use #:cl)
  (:export
   ;; window
   #:create-window #:destroy-window
   ;; GL
   #:create-gl-context #:destroy-gl-context #:set-gl-context #:swap-gl-buffers))

(in-package #:glop)
