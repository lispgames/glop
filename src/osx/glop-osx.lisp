(in-package #:glop)

(defparameter *autorelease-pool* nil)
(defparameter *opengl-bundle* nil)
(defparameter *ns-app* nil)
(defparameter *main-menu* nil)

(defun release-global-autorelease-pool ()
  (when *autorelease-pool*
    (glop-bridge:ns-release *autorelease-pool*)
    (setf *autorelease-pool* nil)))

(defun init-global-autorelease-pool ()
  (release-global-autorelease-pool)
  (setf *autorelease-pool* (glop-bridge:ns-autorelease-pool-alloc-init)))

(defun init-opengl-bundle ()
  (unless *opengl-bundle*
    (glop-bridge:with-ns-autorelease-pool
      (let* ((identifier (glop-bridge:ns-autorelease
                          (glop-bridge:ns-string-alloc-init-with-c-string
                           "com.apple.opengl" :iso-latin-1)))
             (bundle (glop-bridge:cf-bundle-get-bundle-with-identifier
                      identifier)))
        (when (cffi:null-pointer-p bundle)
          (error "Couldn't find the OpenGL bundle."))
        (setf *opengl-bundle* (glop-bridge:ns-retain bundle))))))

(defun release-opengl-bundle ()
  (when *opengl-bundle*
    (glop-bridge:ns-release *opengl-bundle*)
    (setf *opengl-bundle* nil)))

(defun init-ns-app ()
  (setf *ns-app* (glop-bridge:glop-app-shared-application)))

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

(defmethod open-window ((window osx-window) title width height
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
  (unless *ns-app* (init-ns-app))
  (unless *autorelease-pool* (init-global-autorelease-pool))
  (let ((window (glop-bridge:ns-window-alloc-init x y width height)))
    (glop-bridge:ns-window-make-key-and-order-front window)))

(defmethod create-gl-context ((window osx-window)
                              &key make-current major minor forward-compat
                              debug profile)
  (declare (ignorable make-current major minor forward-compat debug profile)))

(defun %next-event (win &key blocking)
  (declare (ignore win blocking)))


(defun gl-get-proc-address (proc-name)
  (init-opengl-bundle)
  (let ((name (glop-bridge:ns-string-alloc-init-with-c-string
                proc-name :iso-latin-1)))
    (unwind-protect (glop-bridge:cf-bundle-get-function-pointer-for-name
                      *opengl-bundle* name)
      (glop-bridge:ns-release name))))
  

(defun run ()
  (glop-bridge:with-ns-autorelease-pool
    (let ((app (glop-bridge:glop-app-shared-application))
          (window (glop-bridge:ns-window-alloc-init 100 100 640 480)))
      (glop-bridge:ns-window-set-background-color
        window (glop-bridge:ns-blue-color))
      (glop-bridge:ns-window-make-key-and-order-front window)
      (glop-bridge:glop-app-run app))))