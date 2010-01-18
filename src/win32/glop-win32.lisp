;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;; GLOP implementation
(in-package #:glop)

(defun gl-get-proc-address (proc-name)
  (glop-wgl:wgl-get-proc-address proc-name))

(defstruct (win32-window (:include window))
  module-handle
  class-name
  pixel-format
  dc
  id)

(defstruct wgl-context
  ctx)

(defun create-gl-context (win &key (make-current t) major minor)
  (let ((ctx (make-wgl-context)))
    (let ((wgl-ctx (glop-wgl:wgl-create-context (win32-window-dc win))))
      (unless wgl-ctx
        (format t "Error creating GL context: ~S~%" (glop-win32:get-last-error)))
      (setf (wgl-context-ctx ctx) wgl-ctx))
    (when make-current
      (attach-gl-context win ctx))
    ctx))

(defun destroy-gl-context (ctx)
  (detach-gl-context ctx)
  (glop-wgl:wgl-delete-context (wgl-context-ctx ctx)))

(defun attach-gl-context (win ctx)
  (glop-wgl:wgl-make-current (win32-window-dc win) (wgl-context-ctx ctx)))

(defun detach-gl-context (ctx)
  (glop-wgl::wgl-make-current (cffi:null-pointer) (cffi:null-pointer)))

(defun create-window (title width height &key major minor fullscreen
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
                                              stencil-buffer (stencil-size 0))
  (when (or (and major minor) fullscreen)
      (error 'not-implemented))
  (let ((win (make-win32-window
              :module-handle (glop-win32:get-module-handle (cffi:null-pointer)))))
    ;; create window class
    (glop-win32:create-and-register-class (win32-window-module-handle win) "OpenGL")
    (setf (win32-window-class-name win) "OpenGL")
    (let ((wnd (glop-win32:create-window-ex '(:ws-ex-app-window :ws-ex-window-edge)
                                  "OpenGL"
                                  title
                                  '(:ws-overlapped-window :ws-clip-siblings :ws-clip-children)
                                  0 0 width height (cffi:null-pointer) (cffi:null-pointer)
                                  (win32-window-module-handle win) (cffi:null-pointer))))
      (unless wnd
        (error "Can't create window (error ~S)~%" (glop-win32:get-last-error)))
      (setf (win32-window-id win) wnd))
    (setf (win32-window-width win) width)
    (setf (win32-window-height win) height)
    (setf (win32-window-dc win) (glop-win32:get-dc (win32-window-id win)))
    ;; choose pixel format
    ;; XXX: kwargs passing is ugly here and we need something else...
    (setf (win32-window-pixel-format win) (glop-win32:choose-pixel-format
                                           (win32-window-dc win)
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
    ;; create GL context and make it current
    (setf (window-gl-context win) (create-gl-context win :make-current t))
    ;; show window
    (glop-win32:set-foreground-window (win32-window-id win))
    (glop-win32:update-window (win32-window-id win))
    (show-window win)
    ;; return created window
    win))

(defun show-window (win)
  (glop-win32:show-window (win32-window-id win) :sw-show)
  (glop-win32:set-focus (win32-window-id win)))

(defun hide-window (win)
  (glop-win32::show-window (win32-window-id win) :sw-hide))

(defun set-window-title (win title)
  (setf (slot-value win 'title) title)
  (glop-win32:set-window-text (win32-window-id win) title))

(defun destroy-window (win)
  (glop-win32:destroy-window (win32-window-id win))
  (glop-win32:unregister-class (win32-window-class-name win)
                                 (win32-window-module-handle win)))

(defun swap-buffers (win)
  (glop-win32:swap-buffers (win32-window-dc win)))

(defun %next-event (win &key blocking)
  (let ((evt (glop-win32:next-event (win32-window-id win) blocking)))
    (setf glop-win32:%event% nil)
    evt))