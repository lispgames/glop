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

(defmethod open-window (window title width height
                        &key (x 0) (y 0) (rgba t) (double-buffer t) stereo
                        (red-size 4) (green-size 4) (blue-size 4) (alpha-size 4)
                        (depth-size 16) accum-buffer (accum-red-size 0)
                        (accum-green-size 0) (accum-blue-size 0) stencil-buffer
                        (stencil-size 0))
  (declare (ignorable
            x y rgba double-buffer stereo red-size green-size blue-size
            alpha-size depth-size accum-buffer accum-red-size
            accum-green-size accum-blue-size stencil-buffer
            stencil-size))
  (glop-bridge:with-ns-autorelease-pool
    (let ((window (glop-bridge:ns-window-alloc-init x y width height)))
      (glop-bridge:ns-window-set-background-color
        window (glop-bridge:ns-blue-color))
      (glop-bridge:ns-window-make-key-and-order-front window))))


(defun run ()
  (glop-bridge:with-ns-autorelease-pool
    (let ((app (glop-bridge:custom-app-shared-application))
          (window (glop-bridge:ns-window-alloc-init 100 100 640 480)))
      (glop-bridge:ns-window-set-background-color
        window (glop-bridge:ns-blue-color))
      (glop-bridge:ns-window-make-key-and-order-front window)
      (glop-bridge:custom-app-run app))))