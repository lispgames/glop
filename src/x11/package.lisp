(defpackage :glop-xlib
  (:use #:cl #:cffi)
  (:export #:visual-info #:bool #:drawable
           #:x-open-display #:x-create-window #:x-default-root-window
           #:x-store-name #:x-flush #:x-map-raised #:x-unmap-window
           #:x-destroy-window #:x-close-display #:x-next-event
           #:x-free #:set-fullscreen #:closest-mode
           #:current-mode #:set-mode #:supported-modes))
