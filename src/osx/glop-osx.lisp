(in-package #:glop)

(defparameter *autorelease-pool* nil)
(defparameter *opengl-bundle* nil)
(defparameter *main-menu* nil)
(defparameter *event-stacks* (make-hash-table))
(defparameter *active-modifiers* '())

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

(defun init-main-menu ()
  (when (cffi:null-pointer-p glop-bridge:*ns-app*) (init-ns-app))
  (unless *autorelease-pool* (init-global-autorelease-pool))
  (setf *main-menu* (glop-bridge:ns-menu-alloc-init "GLOP"))
  (glop-bridge:glop-app-set-main-menu glop-bridge:*ns-app* *main-menu*))

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

(cffi:defcallback push-event-to-stack :void ((ns-event :pointer))
  (let* ((event-type (glop-bridge:ns-event-type ns-event))
         (event
          (case event-type
            ((:key-down :key-up)
             (let ((pressed (eq event-type :key-down)))
               (make-instance (if pressed 'key-press-event 'key-release-event)
                 :pressed pressed
                 :keysym (glop-bridge:ns-event-key-code ns-event)
                 :keycode 0)))
            (:flags-changed
             (let* ((keysym (glop-bridge:ns-event-key-code ns-event))
                    (pressed (null (find keysym *active-modifiers*))))
               (if pressed
                   (push keysym *active-modifiers*)
                   (setf *active-modifiers* (delete keysym *active-modifiers*)))
               (make-instance (if pressed 'key-press-event 'key-release-event)
                 :pressed pressed
                 :keysym keysym
                 :keycode 0)))
            (:mouse-moved
             (destructuring-bind (x y)
                 (glop-bridge:ns-event-location-in-window ns-event)
               (make-instance 'mouse-motion-event
                 :x x
                 :y y
                 :dx (glop-bridge:ns-event-delta-x ns-event)
                 :dy (glop-bridge:ns-event-delta-y ns-event))))
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
         (pf-list (list :depth-size depth-size
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
  (with-accessors ((ns-window ns-window)) window
    (let ((responder (glop-bridge:glop-window-responder-init
                      (cffi:callback push-event-to-stack))))
      (setf ns-window
            (glop-bridge:ns-window-alloc-init
              x (- (+ (video-mode-height (current-video-mode)) height) y)
              width height)
            (event-stack ns-window) '())
      (glop-bridge:ns-window-discard-remaining-events ns-window)
      (glop-bridge:ns-window-set-delegate ns-window responder)
      (glop-bridge:ns-window-set-accepts-mouse-moved-events ns-window t)
      (glop-bridge:ns-window-set-next-responder ns-window responder)
      (glop-bridge:ns-window-set-title ns-window title))))

(defmethod set-window-title ((window osx-window) title)
  (glop-bridge:ns-window-set-title (ns-window window) title))

(defmethod show-window ((window osx-window))
  (glop-bridge:set-front-current-process)
  (glop-bridge:ns-window-make-key-and-order-front (ns-window window)))

(defmethod hide-window ((window osx-window))
  (glop-bridge:ns-window-order-out (ns-window window) glop-bridge:*ns-app*))

(defmethod close-window ((window osx-window))
  (with-accessors ((ns-window ns-window)) window
    (remhash (cffi:pointer-address (ns-window window)) *event-stacks*)
    (glop-bridge:ns-window-close ns-window)))

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
        (setf gl-view (glop-bridge:glop-gl-view-init 0 0 width height
                                                     pixel-format)
              gl-context (glop-bridge:glop-gl-view-opengl-context gl-view))))
    (glop-bridge:ns-window-set-content-view ns-window gl-view)))

(defmethod destroy-gl-context (context)
  (glop-bridge:ns-opengl-context-clear-drawable context)
  (glop-bridge:ns-release context))

(defmethod swap-buffers ((window osx-window))
  (glop-bridge:ns-opengl-context-flush-buffer (gl-context window)))

(defmethod set-fullscreen ((window osx-window) &optional state)
  (declare (ignorable window state)))

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

(defun run ()
  (glop-bridge:with-ns-autorelease-pool
    (let ((app (glop-bridge:glop-app-shared-application))
          (window (glop-bridge:ns-window-alloc-init 100 100 640 480)))
      (glop-bridge:ns-window-set-background-color
        window (glop-bridge:ns-blue-color))
      (glop-bridge:ns-window-make-key-and-order-front window)
      (glop-bridge:glop-app-run app))))