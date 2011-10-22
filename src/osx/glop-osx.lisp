(in-package #:glop)

(defparameter *autorelease-pool* nil)
(defparameter *opengl-bundle* nil)
(defparameter *main-menu* nil)
(defparameter *event-stacks* (make-hash-table))
(declaim (special *native-video-mode*))


(defun event-stack (ns-window)
  (gethash (cffi:pointer-address ns-window) *event-stacks*))

(defsetf event-stack (ns-window) (value)
  `(setf (gethash (cffi:pointer-address ,ns-window) *event-stacks*)
         ,value))

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
  (glop-bridge:transform-current-process-type
    :transform-to-foreground-application)
  (glop-bridge:glop-app-shared-application))

(defmethod current-video-mode ()
  (glop-bridge:copy-display-mode (glop-bridge:main-display-id)))

(defun invert-screen-y (y)
  (- (video-mode-height (current-video-mode)) y))

(defmethod list-video-modes ()
  (let ((display-modes (glop-bridge:copy-all-display-modes
                        (glop-bridge:main-display-id) (cffi:null-pointer))))
    (loop for i below (glop-bridge:ns-array-count display-modes)
          collect (glop-bridge:translate-to-video-mode
                    (glop-bridge:ns-array-object-at-index
                       display-modes i)))))

(cffi:defcallback push-event-to-stack :void ((ns-event :pointer))
  (let* ((event-type (glop-bridge:ns-event-type ns-event))
         (event
          (case event-type
            ((:key-down :key-up)
             (let ((keycode (glop-bridge:ns-event-key-code ns-event))
                   (pressed (eq event-type :key-down)))
               (unless (and *ignore-auto-repeat*
                            (eq (key-pressed keycode) pressed))
                 (setf (key-pressed keycode) pressed)
                 (make-instance (if pressed 'key-press-event 'key-release-event)
                   :pressed pressed
                   :keycode keycode
                   :keysym (glop-bridge:keysym keycode)
                   :text (glop-bridge:ns-event-characters ns-event)))))
            (:flags-changed
             (let* ((keycode (glop-bridge:ns-event-key-code ns-event))
                    (pressed (not (key-pressed keycode))))
               (setf (key-pressed keycode) pressed)
               (make-instance (if pressed 'key-press-event 'key-release-event)
                 :pressed pressed
                 :keysym (glop-bridge:keysym keycode)
                 :keycode keycode
                 :text "")))
            ((:mouse-moved :left-mouse-dragged :right-mouse-dragged
              :other-mouse-dragged)
             (destructuring-bind (x y)
                 (glop-bridge:ns-event-location-in-window ns-event)
               (make-instance 'mouse-motion-event
                 :x x
                 :y y
                 :dx (truncate (glop-bridge:ns-event-delta-x ns-event))
                 :dy (truncate (glop-bridge:ns-event-delta-y ns-event)))))
            ((:left-mouse-down :right-mouse-down :other-mouse-down)
             (make-instance 'button-press-event
               :button (glop-bridge:ns-event-button-number ns-event)
               :pressed t))
            ((:left-mouse-up :right-mouse-up :other-mouse-up)
             (make-instance 'button-release-event
               :button (glop-bridge:ns-event-button-number ns-event)
               :pressed nil)))))
    (when event
      (push event (event-stack (glop-bridge:ns-event-window ns-event))))))

(cffi:defcallback push-notification-to-event-stack :void
    ((notice glop-bridge:glop-notice))
  (destructuring-bind (&key type source) notice
    (let ((event
           (case type
             (:window-close (make-instance 'close-event))
             (:resize
              (let ((rect (glop-bridge:ns-view-frame
                            (glop-bridge:ns-window-content-view source))))
                (make-instance 'resize-event
                               :height (glop-bridge:rect-height rect)
                               :width (glop-bridge:rect-width rect)))))))
      (when event
        (push event (event-stack source))))))

(defmethod open-window ((window osx-window) title width height
                        &key (x 0) (y 0) (rgba t) (double-buffer t) stereo
                        (red-size 4) (green-size 4) (blue-size 4) (alpha-size 4)
                        (depth-size 16) accum-buffer (accum-red-size 0)
                        (accum-green-size 0) (accum-blue-size 0) stencil-buffer
                        (stencil-size 0))
  (declare (ignore rgba accum-buffer stencil-buffer))
  (when (cffi:null-pointer-p glop-bridge:*ns-app*) (init-ns-app))
  (unless *autorelease-pool* (init-global-autorelease-pool))
  (let* ((color-size (+ red-size green-size blue-size alpha-size))
         (accum-size (+ accum-red-size accum-blue-size accum-green-size))
         (pf-list (list :full-screen
                        :screen-mask
                        :accelerated
                        :no-recovery
                        :depth-size depth-size
                        :color-size color-size)))
    (unless (zerop stencil-size)
      (push stencil-size pf-list)
      (push :stencil-size pf-list))
    (unless (zerop accum-size)
      (push accum-size pf-list)
      (push :accum-size pf-list))
    (when double-buffer (push :double-buffer pf-list))
    (when stereo (push :stereo pf-list))
    (setf (pixel-format-list window) pf-list))
  (with-accessors ((ns-window ns-window) (gl-view gl-view)) window
    (setf gl-view
          (glop-bridge:glop-view-init
            (cffi:callback push-event-to-stack)
            (cffi:callback push-notification-to-event-stack))
          ns-window
          (glop-bridge:ns-window-alloc-init x (invert-screen-y y) width height)
          (event-stack ns-window) '())
    (glop-bridge:ns-window-discard-remaining-events ns-window)
    (glop-bridge:ns-window-set-accepts-mouse-moved-events ns-window t)
    (glop-bridge:ns-window-set-content-view ns-window gl-view)
    (glop-bridge:ns-window-set-delegate ns-window gl-view)
    (glop-bridge:ns-window-set-title ns-window title))
  (%update-geometry window x y width height))

(defmethod set-window-title ((window osx-window) title)
  (glop-bridge:ns-window-set-title (ns-window window) title))

(defmethod set-geometry ((window osx-window) x y width height)
  (with-accessors ((ns-window ns-window)) window
    (glop-bridge:ns-window-set-frame ns-window x (invert-screen-y y)
                                     width height)
    (%update-geometry window x y width height)))

(defmethod show-window ((window osx-window))
  (glop-bridge:set-front-current-process)
  (glop-bridge:ns-window-make-key-and-order-front (ns-window window)))

(defmethod hide-window ((window osx-window))
  (glop-bridge:ns-window-order-out (ns-window window) glop-bridge:*ns-app*))

(defmethod close-window ((window osx-window))
  (with-accessors ((ns-window ns-window)) window
    (when (cffi:null-pointer-p ns-window)
      (return-from close-window))
    (set-fullscreen window nil)
    (remhash (cffi:pointer-address (ns-window window)) *event-stacks*)
    (glop-bridge:ns-window-close ns-window)
    (setf ns-window (cffi:null-pointer)) t))

(defmethod attach-gl-context ((window osx-window) ctx)
  (with-accessors ((gl-view gl-view)) window
    (glop-bridge:ns-opengl-context-set-view ctx gl-view)))

(defmethod detach-gl-context (ctx)
  (glop-bridge:ns-opengl-context-clear-drawable ctx))

(defmethod create-gl-context ((window osx-window)
                              &key make-current major minor forward-compat
                              debug profile)
  (declare (ignorable make-current major minor forward-compat debug profile))
  (with-accessors ((width window-width) (height window-height)
                   (pixel-format-list pixel-format-list)
                   (ns-window ns-window) (gl-view gl-view)
                   (gl-context window-gl-context)) window
    (glop-bridge:with-ns-autorelease-pool
      (let ((pixel-format (glop-bridge:ns-autorelease
                            (glop-bridge:ns-opengl-pixel-format-init
                              pixel-format-list))))
        (setf gl-context (glop-bridge:ns-opengl-context-init pixel-format))
        (glop-bridge:ns-opengl-context-make-current-context gl-context)
        (attach-gl-context window gl-context)
        (glop-bridge:ns-opengl-context-set-view gl-context gl-view)))))

(defmethod destroy-gl-context (context)
  (detach-gl-context context)
  (glop-bridge:ns-release context))

(defmethod swap-buffers ((window osx-window))
  (glop-bridge:ns-opengl-context-flush-buffer (window-gl-context window)))

(defmethod set-fullscreen ((window osx-window)
                           &optional (state (not (window-fullscreen window))))
  (declare (ignorable window state))
  (with-accessors ((gl-context window-gl-context)
                   (gl-view gl-view)) window
    (if state
        (let ((fullscreen-mode
               (closest-video-mode (current-video-mode)
                                   (list-video-modes)
                                   (window-width window)
                                   (window-height window))))
          (glop-bridge:capture-all-displays)
          (glop-bridge:set-display-mode
            (glop-bridge:main-display-id)
            (osx-video-mode-mode fullscreen-mode)
            (cffi:null-pointer))
          (glop-bridge:ns-opengl-context-clear-drawable gl-context)
          (glop-bridge:ns-opengl-context-set-full-screen gl-context)
          (glop-bridge:ns-retain gl-context)
          (setf (window-fullscreen window) t))
        (progn
          (glop-bridge:set-display-mode
            (glop-bridge:main-display-id)
            (osx-video-mode-mode *native-video-mode*)
            (cffi:null-pointer))
          (glop-bridge:ns-opengl-context-clear-drawable gl-context)
          (glop-bridge:release-all-displays)
          (glop-bridge:ns-opengl-context-set-view gl-context gl-view)
          (setf (window-fullscreen window) nil)))))

(defun %next-event (win &key blocking)
  (loop
     for ns-window = (ns-window win)
     for event = (glop-bridge:glop-app-next-event glop-bridge:*ns-app* blocking)
     for found = (cffi:pointer-eq ns-window (glop-bridge:ns-event-window event))
     do (progn (glop-bridge:glop-app-send-event glop-bridge:*ns-app* event)
               (glop-bridge:glop-app-update-windows))
     while (and blocking (or (not found) (null (event-stack ns-window))))
     finally (when found
               (return (pop (event-stack ns-window))))))

(defun gl-get-proc-address (proc-name)
  (init-opengl-bundle)
  (let ((name (glop-bridge:ns-string-alloc-init-with-c-string
                proc-name :iso-latin-1)))
    (unwind-protect (glop-bridge:cf-bundle-get-function-pointer-for-name
                      *opengl-bundle* name)
      (glop-bridge:ns-release name))))

(defparameter *native-video-mode* (current-video-mode))