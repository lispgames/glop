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
           #:ns-application-shared-application
           #:ns-black-color
           #:ns-blue-color
           #:ns-brown-color
           #:ns-cyan-color
           #:ns-dark-gray-color
           #:ns-gray-color
           #:ns-green-color
           #:ns-light-gray-color
           #:ns-magenta-color
           #:ns-orange-color
           #:ns-purple-color
           #:ns-white-color
           #:ns-yellow-color
           #:ns-window-alloc-init
           #:ns-window-set-background-color
           #:ns-window-make-key-and-order-front
           #:ns-autorelease-pool-alloc-init
           #:ns-autorelease-pool-release
           #:with-ns-autorelease-pool
           #:ns-string-c-string-using-encoding
           #:ns-string-to-lisp-string
           #:custom-app-shared-application
           #:custom-app-run-iteration
           #:custom-app-run))