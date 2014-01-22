;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;; GLOP implementation
(in-package #:glop)

(defun gl-get-proc-address (proc-name)
  (glop-wgl:wgl-get-proc-address proc-name))

(defmethod list-video-modes ()
  (glop-win32::list-video-modes))

(defmethod set-video-mode ((mode video-mode))
  (glop-win32::set-video-mode mode))

(defmethod current-video-mode ()
  (glop-win32::current-video-mode))

(defstruct wgl-context
  ctx)

(defmethod create-gl-context ((win win32-window) &key (make-current t) major minor
                                                          forward-compat debug
                                                          profile)

  (let ((ctx (make-wgl-context)))
        (setf (wgl-context-ctx ctx)
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
                                (glop-wgl:wgl-create-specific-context (win32-window-dc win) attrs))
                          (glop-wgl:wgl-create-context (win32-window-dc win))))
        (unless (wgl-context-ctx ctx)
          (format t "Error creating GL context: ~S~%" (glop-win32:get-last-error)))
        (when make-current
          (attach-gl-context win ctx))
        (when (and major minor)
          (glop-wgl:correct-context? major minor))
        ctx))

(defmethod destroy-gl-context ((ctx wgl-context))
  (detach-gl-context ctx)
  (glop-wgl:wgl-delete-context (wgl-context-ctx ctx)))

(defmethod attach-gl-context ((win win32-window) (ctx wgl-context))
  (setf (window-gl-context win) ctx)
  (glop-wgl:wgl-make-current (win32-window-dc win) (wgl-context-ctx ctx)))

(defmethod detach-gl-context ((ctx wgl-context))
  (glop-wgl::wgl-make-current (cffi:null-pointer) (cffi:null-pointer)))

(defmethod open-window ((win win32-window) title width height &key (x 0) (y 0)
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
  (setf (win32-window-module-handle win)
                (glop-win32:get-module-handle (cffi:null-pointer)))
  ;; register window class
  (glop-win32:create-and-register-class (win32-window-module-handle win) "GLOP-OpenGL")
  (setf (win32-window-class-name win) "GLOP-OpenGL")
  ;; create the window
  (let ((wnd (glop-win32:create-window-ex '(:ws-ex-app-window :ws-ex-window-edge)
                                        "GLOP-OpenGL"
                                        title
                                        '(:ws-overlapped-window :ws-clip-siblings :ws-clip-children)
                                        x y width height (cffi:null-pointer) (cffi:null-pointer)
                                        (win32-window-module-handle win) (cffi:null-pointer))))
    (unless wnd
      (error "Can't create window (error ~S)~%" (glop-win32:get-last-error)))
    (setf (win32-window-id win) wnd))
  (%update-geometry win x y width height)
  (setf (win32-window-dc win)
        (glop-win32:get-dc (win32-window-id win)))
  (setf (win32-window-pixel-format win) (glop-win32:choose-pixel-format
                                         (win32-window-dc win)
                                         :rgba rgba
                                         :double-buffer double-buffer
                                         :stereo stereo
                                         :red-size red-size
                                         :green-size green-size
                                         :blue-size blue-size
                                         :alpha-size alpha-size
                                         :depth-size depth-size
                                         :accum-buffer accum-buffer
                                         :accum-red-size accum-red-size
                                         :accum-green-size accum-green-size
                                         :accum-blue-size accum-blue-size
                                         :stencil-buffer stencil-buffer
                                         :stencil-size stencil-size))
  (glop-win32:set-foreground-window (win32-window-id win))
  (glop-win32:update-window (win32-window-id win))
  win)

(defmethod close-window ((win win32-window))
  (glop-win32:destroy-window (win32-window-id win))
  (glop-win32:unregister-class (win32-window-class-name win)
                                                           (win32-window-module-handle win)))

(defmethod set-fullscreen ((win win32-window) &optional (state (not (window-fullscreen win))))
  (with-accessors ((id win32-window-id)
                                   (fullscreen window-fullscreen))
          win
        (unless (eq state fullscreen)
          (if state
                  (progn (glop-win32::%set-fullscreen (win32-window-id win) t)
                                 (setf fullscreen t))
                  (progn (glop-win32::%set-fullscreen (win32-window-id win) nil)
                                 (setf fullscreen nil))))
        (glop-win32:update-window (win32-window-id win))
        (show-window win)))


(defmethod set-geometry ((win win32-window) x y width height)
  (glop-win32:set-geometry (win32-window-id win) x y width height)
  (%update-geometry win x y width height))

(defmethod show-window ((win win32-window))
  (glop-win32:show-window (win32-window-id win) :sw-show)
  (glop-win32:set-focus (win32-window-id win)))

(defmethod hide-window ((win win32-window))
  (glop-win32::show-window (win32-window-id win) :sw-hide))

(defmethod set-window-title ((win win32-window) title)
  (setf (slot-value win 'title) title)
  (glop-win32:set-window-text (win32-window-id win) title))

(defmethod swap-buffers ((win win32-window))
  (glop-win32:swap-buffers (win32-window-dc win)))

(defmethod show-cursor ((win win32-window))
  (glop-win32:show-cursor 1))

(defmethod hide-cursor ((win win32-window))
  (glop-win32:show-cursor 0))

(defun %next-event (win &key blocking)
  (let ((evt (glop-win32:next-event win (win32-window-id win) blocking)))
        (setf glop-win32:%event% nil)
        evt))
