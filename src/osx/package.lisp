(defpackage #:glop-core-foundation
  (:use #:cl #:cffi)
  (:nicknames #:glop-cf)
  (:export #:string-length
           #:string-c-string
           #:string-lisp-string
           #:array-count
           #:array-values
           #:lisp-array-values))

(defpackage #:glop-quartz
  (:use :cl :cffi :glop-cf)
  (:export #:main-display-id
           #:copy-display-mode
           #:copy-all-display-modes
           #:mode-width
           #:mode-height
           #:mode-rate
           #:mode-pixel-encoding))
