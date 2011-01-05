;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; GLOP implementation
(in-package #:glop)

(defun gl-get-proc-address (proc-name)
  (glop-glx:glx-get-proc-address proc-name))

(defstruct glx-context
  ctx           ;; GL context ptr
  display       ;; X display ptr
  )

(defmethod create-gl-context ((win x11-window) &key (make-current t) major minor
                                                    forward-compat debug
                                                    profile)
  (let ((ctx (make-glx-context :display (x11-window-display win))))
    (setf (glx-context-ctx ctx)
          (if (and major minor)
              (let ((attrs (list :major-version major :minor-version minor)))
                (when profile
                  (case profile
                    (:core (push :core-profile-bit attrs))
                    (:compat (push :compatibility-profile-bit attrs)))
                  (push :profile-mask attrs))
                (when (or forward-compat debug)
                  (let ((flags '()))
                    (when forward-compat (push :forward-compatible-bit flags))
                    (when debug (push :debug-bit flags))
                    (push flags attrs)
                    (push :flags attrs)))
                (glop-glx:glx-create-specific-context (x11-window-display win)
                                                      (x11-window-fb-config win)
                                                      attrs))
              (glop-glx:glx-create-context (x11-window-display win)
                                           (x11-window-visual-infos win))))
    (when make-current
      (attach-gl-context win ctx))
    (when (and major minor)
      (glop-glx:correct-context? major minor))
    ctx))

(defmethod destroy-gl-context ((ctx glx-context))
  (detach-gl-context ctx)
  (glop-glx:glx-destroy-context (glx-context-display ctx)
                                (glx-context-ctx ctx)))

(defmethod attach-gl-context ((win x11-window) (ctx glx-context))
  (setf (window-gl-context win) ctx)
  (glop-glx:glx-make-current (glx-context-display ctx)
                             (x11-window-id win)
                             (glx-context-ctx ctx)))

(defmethod detach-gl-context ((ctx glx-context))
  (glop-glx:glx-release-context (glx-context-display ctx)))

(defmethod open-window ((win x11-window) title width height &key (x 0) (y 0)
                        (rgba t)
                        (double-buffer t)
                        stereo
                        (red-size 4)
                        (green-size 4)
                        (blue-size 4)
                        (alpha-size 4)
                        (depth-size 16)
                        accum-buffer
                        (accum-red-size 0)
                        (accum-green-size 0)
                        (accum-blue-size 0)
                        stencil-buffer
                        (stencil-size 0))
  (without-fp-traps
      (let ((attribs (list :rgba rgba
                           :red-size red-size
                           :green-size green-size
                           :blue-size blue-size
                           :alpha-size alpha-size
                           :depth-size depth-size
                           :double-buffer double-buffer
                           :stereo stereo)))
        (when accum-buffer
          (push accum-red-size attribs)
          (push :accum-red-size attribs)
          (push accum-green-size attribs)
          (push :accum-green-size attribs)
          (push accum-blue-size attribs)
          (push :accum-blue-size attribs))
        (when stencil-buffer
          (push stencil-size attribs)
          (push :stencil-size attribs))
        (with-accessors ((display x11-window-display)
                         (screen x11-window-screen)
                         (id x11-window-id)
                         (visual-infos x11-window-visual-infos)
                         (fb-config x11-window-fb-config)
                         (win-title window-title))
            win
          (setf display (glop-xlib:x-open-display)
                screen 0)
          ;; FIXME: something like defglextfun from cl-opengl would be nice for extensions
          ;; FIXME: all the fb config stuff should be checked as extensions too
          (if (cffi::null-pointer-p (gl-get-proc-address "glXCreateContextAttribsARB"))
              (setf visual-infos (glop-glx:glx-choose-visual display screen attribs))
              (setf fb-config (glop-glx:glx-choose-fb-config display screen attribs)
                    visual-infos (glop-glx:glx-get-visual-from-fb-config display fb-config)))
          (setf id (glop-xlib:x-create-window display
                                              (glop-xlib:x-default-root-window display)
                                              x y width height visual-infos))
          (cffi:with-foreign-object (array :unsigned-long)
            (setf (cffi:mem-aref array :unsigned-long)
                  (glop-xlib:x-intern-atom display "WM_DELETE_WINDOW" nil))
            (glop-xlib:x-set-wm-protocols display id array 1))
          (%update-geometry win x y width height)
          (glop-xlib:x-store-name display id title)
          (setf win-title title)
          (glop-xlib:xkb-set-detectable-auto-repeat display t (cffi:null-pointer))
          (glop-xlib:x-flush (x11-window-display win))
          win))))

(defmethod close-window ((win x11-window))
  (with-accessors ((display x11-window-display)
                   (id x11-window-id)
                   (context window-gl-context))
      win
    (glop-xlib:x-destroy-window display id)
    (glop-xlib:x-close-display display)))

(defmethod set-fullscreen ((win x11-window) &optional (state (not (window-fullscreen win))))
  (with-accessors ((display x11-window-display)
                   (screen x11-window-screen)
                   (id x11-window-id)
                   (previous-video-mode window-previous-video-mode)
                   (win-width window-width)
                   (win-height window-height)
                   (fullscreen window-fullscreen))
      win
    (unless (eq state fullscreen)
      (if state
          (progn
            (setf previous-video-mode
                  (multiple-value-bind (width height depth)
                      (glop-xlib:current-mode display screen)
                    (make-video-mode width height depth)))
            (glop-xlib:set-mode display screen
                                (glop-xlib:closest-mode display screen
                                                        win-width win-height 0) 0)
            (glop-xlib:%set-fullscreen id display t)
            (setf fullscreen t))
          (progn
            (with-accessors ((height video-mode-height)
                             (width video-mode-width))
                previous-video-mode
              (glop-xlib:%set-fullscreen id display nil)
              (glop-xlib:set-mode display screen
                                  (glop-xlib:closest-mode display screen
                                                                          width height 0) 0))
            (setf fullscreen nil))))))

(defmethod set-geometry ((win x11-window) x y width height)
  (glop-xlib:x-set-geometry (x11-window-display win) (x11-window-id win) x y width height)
  (%update-geometry win x y width height))

(defmethod show-window ((win x11-window))
  (glop-xlib:x-map-raised (x11-window-display win) (x11-window-id win)))

(defmethod hide-window ((win x11-window))
  (glop-xlib:x-unmap-window (x11-window-display win) (x11-window-id win)))

(defmethod set-window-title ((win x11-window) title)
  (setf (slot-value win 'title) title)
  (glop-xlib:x-store-name (x11-window-display win) (x11-window-id win) title))

(defmethod swap-buffers ((win x11-window))
  (glop-glx:glx-wait-gl)
  (glop-glx:glx-swap-buffers (x11-window-display win) (x11-window-id win)))

(defun %next-event (win &key blocking)
  (glop-xlib:x-next-event win (x11-window-display win) blocking))

