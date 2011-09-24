(in-package #:glop)

(defun display-to-video-mode (mode)
  (make-osx-video-mode
    :width (glop-bridge:mode-width mode)
    :height (glop-bridge:mode-height mode)
    :rate (glop-bridge:mode-rate mode)
    :depth (length (glop-bridge:mode-pixel-encoding mode))
    :mode mode))

(defmethod current-video-mode ()
  (display-to-video-mode 
   (glop-bridge:copy-display-mode (glop-bridge:main-display-id))))

(defmethod list-video-modes ()
  (let ((display-modes (glop-bridge:copy-all-display-modes
                        (glop-bridge:main-display-id) (cffi:null-pointer))))
    (loop for i below (glop-bridge:ns-array-count display-modes)
          collect (display-to-video-mode
                    (glop-bridge:ns-array-object-at-index
                       display-modes i)))))