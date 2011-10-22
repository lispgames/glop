(in-package #:glop-bridge)

(define-foreign-library application-services
  (t (:framework "ApplicationServices")))
(use-foreign-library application-services)

(define-foreign-type display-mode-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser display-mode))

(declaim (inline display-to-video-mode))
(defun translate-to-video-mode (mode)
  (glop::make-osx-video-mode
    :width (mode-width mode)
    :height (mode-height mode)
    :rate (mode-rate mode)
    :depth (length (mode-pixel-encoding mode))
    :mode mode))

(defmethod translate-from-foreign (mode (type display-mode-type))
  (declare (ignore type))
  (translate-to-video-mode mode))

;;; Not needed, have autorelease pools which take care of this. Must test.
;; (defmethod free-translated-object (mode (type display-mode-type) param)
;;   (declare (ignore param))
;;   (display-mode-release mode))


(defcenum cg-error-code
  (:success 0)
  (:failure 1000)
  :illegal-argument
  :invalid-connection
  :invalid-context
  :cannot-complete
  :name-too-long
  :not-implemented
  :range-check
  :type-check
  :no-current-point
  :invalid-operation
  :none-available
  (:application-requires-newer-system 1015)
  :application-not-permitted-to-execute
  (:application-incorrect-executable-format-found 1023)
  :application-is-launching
  :application-already-running
  :application-can-only-be-run-in-one-session-at-a-time
  :classic-application-must-be-launched-by-classic
  :fork-failed
  :retry-registration)

(define-condition cg-error (error)
  ((code :initform (error "Must specify code.")
           :initarg :code
           :reader code))
  (:report (lambda (condition stream)
             (format stream "CGError detected with code ~s."
                     (code condition)))))

(define-foreign-type cg-error-type ()
  ()
  (:actual-type cg-error-code)
  (:simple-parser cg-error))

(defmethod translate-from-foreign (value (type cg-error-type))
  (let ((code (foreign-enum-keyword 'cg-error-code value)))
    (unless (eq code :success)
      (error 'cg-error :code code))
    code))

(defctype display-id :uint32)
(defctype size-t :uint32)
(defcfun ("CGMainDisplayID" main-display-id) display-id)
(defcfun ("CGDisplayCopyDisplayMode" copy-display-mode) display-mode
  (id display-id))
(defcfun ("CGDisplayCopyAllDisplayModes" copy-all-display-modes) :pointer
  (id display-id)
  (options :pointer))
(defcfun ("CGDisplayModeRelease" display-mode-release) :void
  (mode display-mode))
(defcfun ("CGDisplayModeGetWidth" mode-width) size-t
  (mode display-mode))
(defcfun ("CGDisplayModeGetHeight" mode-height) size-t
  (mode display-mode))
(defcfun ("CGDisplayModeGetRefreshRate" mode-rate) :double
  (mode display-mode))
(defcfun ("CGDisplayModeCopyPixelEncoding" mode-pixel-encoding) ns-string
  (mode display-mode))
(defcfun ("CGCaptureAllDisplays" capture-all-displays) cg-error)
(defcfun ("CGReleaseAllDisplays" release-all-displays) cg-error)
(defcfun ("CGDisplaySetDisplayMode" set-display-mode) cg-error
  (id display-id)
  (mode display-mode)
  (options :pointer))