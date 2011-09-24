(defpackage #:glop-bridge
  (:use #:cl #:cffi)
  (:export #:main-display-id
           #:copy-display-mode
           #:copy-all-display-modes
           #:mode-width
           #:mode-height
           #:mode-rate
           #:mode-pixel-encoding
           #:ns-array-count
           #:ns-array-object-at-index
           #:ns-string-to-lisp
           #:ns-application-shared-application))