(in-package #:glop)

(defun display-to-video-mode (mode)
  (make-osx-video-mode
    :width (glop-quartz:mode-width mode)
    :height (glop-quartz:mode-height mode)
    :rate (glop-quartz:mode-rate mode)
    :depth (length (glop-quartz:mode-pixel-encoding mode))
    :mode mode))

(defmethod current-video-mode ()
  (display-to-video-mode 
   (glop-quartz:copy-display-mode (glop-quartz:main-display-id))))

(defmethod list-video-modes ()
  (let ((display-modes (glop-quartz:copy-all-display-modes
                        (glop-quartz:main-display-id) (cffi:null-pointer))))
    ;; (map 'list #'display-to-video-mode
    ;;      (glop-cf:lisp-array-values display-modes))
    (glop-cf:lisp-array-values display-modes)
    ))