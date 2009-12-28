;; GLOP implementation
(in-package #:glop)

;;; Execute BODY with floating-point traps disabled. This seems to be
;;; necessary on (at least) Linux/x86-64 where SIGFPEs are signalled
;;; when creating making a GLX context active.
#+(and sbcl x86-64)
(defmacro without-fp-traps (&body body)
 `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
 ,@body))

;;; Do nothing on Lisps that don't need traps disabled.
#-(and sbcl x86-64)
(defmacro without-fp-traps (&body body)
 `(progn ,@body))

(setf gl-get-proc-address #'glop-glx:glx-get-proc-address)

(defstruct (x11-window (:include window))
  display      ;; X display ptr
  screen       ;; X screen number
  id           ;; X window ID
  visual-infos ;; X visual format of the window
  fb-config    ;; X framebuffer config
  )

(defstruct glx-context
  ctx           ;; GL context ptr
  display       ;; X display ptr
  )

(defmethod create-gl-context ((win x11-window) &key (make-current t) major minor)
    (let ((ctx (make-glx-context :display (x11-window-display win))))
      (setf (glx-context-ctx ctx)
            (if (and major minor)
                (glop-glx:glx-create-specific-context (x11-window-display win)
                                                       (x11-window-fb-config win)
                                                       `(:major-version ,major :minor-version ,minor))
                (glop-glx:glx-create-context (x11-window-display win)
                                              (x11-window-visual-infos win))))
      (when make-current
        (attach-gl-context win ctx))
      (when (and major minor)
        (glop-glx:correct-context? major minor))
      ctx))

(defmethod destroy-gl-context (ctx)
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

(defmethod create-window (title width height &key major minor
                                                  (double-buffer t)
                                                  stereo
                                                  (red-size 0)
                                                  (green-size 0)
                                                  (blue-size 0)
                                                  (alpha-size 0)
                                                  (depth-size 0)
                                                  accum-buffer
                                                  (accum-red-size 0)
                                                  (accum-green-size 0)
                                                  (accum-blue-size 0)
                                                  stencil-buffer (stencil-size 0))
  (without-fp-traps
    (let ((win (make-x11-window :display (glop-xlib::x-open-display "")
                                :screen 0)))
      ;;GLX attributes
      (let ((attribs (list :rgba t
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
        ;; if major *and* minor are specified use fb config code path
        ;; otherwise just use old style visual selection and context creation
        (if (and major minor)
            ;;create fb-config and visual
            (setf (x11-window-fb-config win)
                  (glop-glx:glx-choose-fb-config (x11-window-display win)
                                                  (x11-window-screen win) attribs)
                  (x11-window-visual-infos win)
                  (glop-glx:glx-get-visual-from-fb-config (x11-window-display win)
                                                            (x11-window-fb-config win)))
            ;; create old style visual
            (setf (x11-window-visual-infos win)
                  (glop-glx:glx-choose-visual (x11-window-display win)
                                               (x11-window-screen win)
                                               attribs))))
      ;; create window
      (setf (x11-window-id win) (glop-xlib:x-create-window
                                 (x11-window-display win)
                                 (glop-xlib:x-default-root-window (x11-window-display win))
                                 width height (x11-window-visual-infos win)))
      (setf (window-width win) width)
      (setf (window-height win) height)
      ;; set title
      (glop-xlib:x-store-name (x11-window-display win) (x11-window-id win) title)
      (setf (slot-value win 'title) title)
      ;; create a GL context and make it current same as for the visual regarding to major/minor
      ;; values
      (setf (window-gl-context win) (create-gl-context win :major major :minor minor
                                                           :make-current t))
      ;; show created window
      (show-window win)
      (glop-xlib:x-flush (x11-window-display win))
      ;; return created window
      win)))

(defmethod show-window ((win x11-window))
  (glop-xlib:x-map-raised (x11-window-display win) (x11-window-id win)))

(defmethod hide-window ((win x11-window))
  (glop-xlib:x-unmap-window (x11-window-display win) (x11-window-id win)))

(defmethod set-window-title ((win x11-window) title)
  (setf (slot-value win 'title) title)
  (glop-xlib:x-store-name (x11-window-display win) (x11-window-id win) title))

(defmethod destroy-window ((win x11-window))
  (glop-xlib:x-destroy-window (x11-window-display win) (x11-window-id win))
  (glop-xlib:x-close-display (x11-window-display win)))

(defmethod swap-buffers ((win x11-window))
  (glop-glx:glx-wait-gl)
  (glop-glx:glx-swap-buffers (x11-window-display win) (x11-window-id win)))

(defmethod next-event ((win x11-window) &key blocking)
  (glop-xlib:x-next-event (x11-window-display win) blocking))

