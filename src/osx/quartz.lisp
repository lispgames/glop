(in-package #:glop-bridge)

(define-foreign-library application-services
  (t (:framework "ApplicationServices")))
(use-foreign-library application-services)

(defctype display-id :uint32)
(defctype size-t :uint32)
(defcfun ("CGMainDisplayID" main-display-id) display-id)
(defcfun ("CGDisplayCopyDisplayMode" copy-display-mode) :pointer
  (id display-id))
(defcfun ("CGDisplayCopyAllDisplayModes" copy-all-display-modes) :pointer
  (id display-id)
  (options :pointer))
(defcfun ("CGDisplayModeGetWidth" mode-width) size-t
  (mode :pointer))
(defcfun ("CGDisplayModeGetHeight" mode-height) size-t
  (mode :pointer))
(defcfun ("CGDisplayModeGetRefreshRate" mode-rate) :double
  (mode :pointer))
(defcfun ("CGDisplayModeCopyPixelEncoding" mode-pixel-encoding) ns-string
  (mode :pointer))
