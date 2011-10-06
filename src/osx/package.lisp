(defpackage #:glop-bridge
  (:use #:cl #:cffi)
  (:export #:*ns-app*
           #:cf-bundle-get-bundle-with-identifier
           #:cf-bundle-get-function-pointer-for-name
           #:copy-all-display-modes
           #:copy-display-mode
           #:glop-app-next-event
           #:glop-app-run
           #:glop-app-run-iteration
           #:glop-app-send-event
           #:glop-app-set-main-menu
           #:glop-app-shared-application
           #:glop-app-update-windows
           #:list-to-pixel-format
           #:main-display-id
           #:mode-height
           #:mode-pixel-encoding
           #:mode-rate
           #:mode-width
           #:ns-application-shared-application
           #:ns-array-count
           #:ns-array-object-at-index
           #:ns-autorelease
           #:ns-autorelease-pool-alloc-init
           #:ns-black-color
           #:ns-blue-color
           #:ns-brown-color
           #:ns-cyan-color
           #:ns-dark-gray-color
           #:ns-gray-color
           #:ns-green-color
           #:ns-light-gray-color
           #:ns-magenta-color
           #:ns-menu-add-item
           #:ns-menu-add-item-with-title
           #:ns-menu-alloc-init
           #:ns-menu-item-alloc-init
           #:ns-orange-color
           #:ns-purple-color
           #:ns-red-color
           #:ns-release
           #:ns-retain
           #:ns-selector-from-string
           #:ns-string-alloc-init-with-c-string
           #:ns-string-c-string-using-encoding
           #:ns-string-to-lisp
           #:ns-string-to-lisp-string
           #:ns-white-color
           #:ns-window-alloc-init
           #:ns-window-close
           #:ns-window-make-key-and-order-front
           #:ns-window-order-out
           #:ns-window-set-background-color
           #:ns-window-set-released-when-closed
           #:ns-window-set-title
           #:ns-yellow-color
           #:set-front-current-process
           #:set-front-process
           #:transform-current-process-type
           #:transform-process-type
           #:with-ns-autorelease-pool
           #:with-ns-strings
           #:glop-gl-view-init
           #:ns-window-set-content-view
           #:ns-opengl-pixel-format-init
           #:glop-gl-view-opengl-context
           #:glop-gl-view-set-opengl-context
           #:glop-gl-view-clear-gl-context
           #:ns-opengl-context-clear-drawable
           #:ns-event-type
           #:glop-window-responder-init
           #:ns-window-set-delegate
           #:ns-window-set-next-responder
           #:glop-gl-view-set-next-responder
           #:ns-window-set-accepts-mouse-moved-events
           #:ns-event-window
           #:ns-event-location-in-window
           #:ns-event-delta-x
           #:ns-event-delta-y
           #:ns-event-button-number))