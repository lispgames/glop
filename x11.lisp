(defpackage :glop-x11
  (:use #:cl #:cffi)
  (:export))

(in-package #:glop-x11)

;; X11 bindings
(define-foreign-library xlib
  (t (:default "libX11")))
(use-foreign-library xlib)

(defctype xid :unsigned-long)
;; X
(defctype window xid)
(defctype drawable xid)
(defctype colormap xid)
(defctype pixmap xid)
(defctype cursor xid)
(defctype gcontext xid)
(defctype keysym xid)

(defctype bool :int)

(defcstruct _xdisplay)
(defctype display _xdisplay)

(defctype visualid :unsigned-long)

(defcstruct visual-info
    (visual :pointer) (visual-id visualid) (screen :int)
    (depth :int) (class :int)
    (red-mask :ulong) (green-mask :ulong) (blue-mask :ulong)
    (colormap-size :int) (bpp :int))

(defcstruct set-window-attributes
  (bg-pixmap pixmap) (bg-pixel :unsigned-long)
  (border-pixmap pixmap) (border-pixel :unsigned-long)
  (bit-gravity :int) (win-gravity :int) (backing-store :int)
  (backing-planes :unsigned-long) (backing-pixel :unsigned-long)
  (save-under bool) (event-mask :long) (do-not-propagate-mask :long)
  (override-redirect bool) (cmap colormap) (curs cursor))

(defcfun ("XOpenDisplay" x-open-display) :pointer
  (display-name :string))

(defcfun("XCloseDisplay" x-close-display) :pointer
  (display-ptr :pointer))

(defmacro with-display ((display-sym display-name) &body body)
  `(let ((,display-sym (%x-open-display ,display-name)))
     (unwind-protect
          (progn ,@body)
       (%x-close-display ,display-sym))))

(defcfun ("XDefaultRootWindow" %x-default-root-window) window
  (display-ptr :pointer))
 
(defcenum (x-alloc :int)
  (:alloc-none)
  (:alloc-all))

(defcfun ("XCreateColormap" %x-create-color-map) colormap
  (display-ptr :pointer) (win window) (visual-ptr :pointer) (alloc x-alloc))

(defcenum (x-window-class :int)
  (:copy-from-parent 0)
  (:input-output 1)
  (:input-only 2))

(defbitfield x-window-attributes-flags
  (:cw-back-pixmap       #x0001)
  :cw-back-pixel
  :cw-border-pixmap
  :cw-border-pixel
  :cw-bit-gravity
  :cw-win-gravity
  :cw-backing-store
  :cw-backing-planes
  :cw-backing-pixel
  :cw-override-redirect
  :cw-save-under
  :cw-event-mask
  :cw-dont-propagate
  :cw-colormap
  :cw-cursor)

(defbitfield x-event-mask-flags
  (:no-event-mask       #x0000)
  (:key-press-mask      #x0001)
  :key-release-mask
  :button-press-mask
  :button-release-mask
  :enter-window-mask
  :leave-window-mask
  :pointer-motion-mask
  :pointer-motion-hint-mask
  :button1-motion-mask
  :button2-motion-mask
  :button3-motion-mask
  :button4-motion-mask
  :button5-motion-mask
  :button-motion-mask
  :keymap-state-mask
  :exposure-mask
  :visibility-change-mask
  :structure-notify-mask
  :resize-redirect-mask
  :substructure-notify-mask
  :substructure-redirect-mask
  :focus-change-mask
  :property-change-mask
  :colormap-change-mask
  :owner-grab-buttons-mask)

(defcenum x-event-name
  (:key-press 2)
  :key-release
  :button-press
  :button-release
  :motion-notify
  :enter-notify
  :leave-notify
  :focus-in
  :focus-out
  :keymap-notify
  :expose
  :graphics-expose
  :no-expose
  :visibility-notify
  :create-notify
  :destroy-notify
  :unmap-notify
  :map-notify
  :map-request
  :reparent-notify
  :configure-notify
  :configure-request
  :gravity-notify
  :resize-request
  :circulate-notify
  :circulate-request
  :property-notify
  :selection-clear
  :selection-request
  :selection-notify
  :colormap-notify
  :client-message
  :mapping-notify
  :generic-event
  (:last-event 36))

(defctype x-time :unsigned-long)

(defcstruct x-key-event
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display-ptr :pointer)
  (win window)
  (root window)
  (subwin window)
  (time x-time)
  (x :int) (y :int)
  (x-root :int) (y-root :int)
  (state :unsigned-int)
  (keycode :unsigned-int)
  (same-screen bool))

(defctype x-key-pressed-event x-key-event)
(defctype x-key-released-event x-key-event)

(defcstruct x-button-event
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display-ptr :pointer)
  (win window)
  (root window)
  (subwin window)
  (time x-time)
  (x :int) (y :int)
  (x-root :int) (y-root :int)
  (state :unsigned-int)
  (button :unsigned-int)
  (same-screen bool))

(defctype x-button-pressed-event x-button-event)
(defctype x-button-released-event x-button-event)

(defcstruct x-motion-event
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display-ptr :pointer)
  (win window)
  (root window)
  (subwin window)
  (time x-time)
  (x :int) (y :int)
  (x-root :int) (y-root :int)
  (state :unsigned-int)
  (is-hint :char)
  (same-screen bool))

(defctype x-pointer-moved-event x-motion-event)

;; TODO: add missing events

(defcunion x-event
  (type x-event-name)
  (pad :long :count 24))

(defcfun ("XCreateWindow" %x-create-window) window
  (display-ptr :pointer) (parent window) (x :int) (y :int) (width :int) (height :int)
  (border-width :int) (depth :int) (win-class x-window-class) (visual :pointer)
  (value-mask x-window-attributes-flags) (attributes set-window-attributes))

(defun x-create-window (dpy parent width height visual-infos)
  (let ((root-win (%x-default-root-window dpy)))
    (with-foreign-slots ((visual-id visual depth) visual-infos visual-info)
      (let ((colormap (%x-create-color-map dpy root-win visual :alloc-none)))
        (with-foreign-object (win-attrs 'set-window-attributes)
          (with-foreign-slots ((cmap event-mask) win-attrs set-window-attributes)
            (setf cmap colormap
                  event-mask (foreign-bitfield-value 'x-event-mask-flags
                                '(:exposure-mask
                                  :key-press-mask :key-release-mask
                                  :button-press-mask :button-release-mask
                                  :structure-notify-mask
                                  :visibility-change-mask
                                  :pointer-motion-mask)))
            (format t "ColorMap: ~S~%" cmap)
            (format t "EventMask: ~S~%" event-mask)
            (%x-create-window dpy parent 0 0 width height 0
                              depth :input-output visual
                              '(:cw-colormap :cw-event-mask)
                              win-attrs)))))))


(defcfun ("XDestroyWindow" x-destroy-window) :int
  (display-ptr :pointer) (win window))

(defcfun ("XMapWindow" %x-map-window) :int
  (display-ptr :pointer) (win window))

(defcfun ("XMapRaised" %x-map-raised) :int
  (display-ptr :pointer) (win window))

(defcfun ("XStoreName" %x-store-name) :int
  (display-ptr :pointer) (win window) (name :string))

(defcfun ("XNextEvent" x-next-event) :int
  (display-ptr :pointer) (evt x-event))

(defcfun ("XPending" %x-pending) :int
  (display-ptr :pointer))

(defun x-pending-p (display-ptr)
  (not (zerop (%x-pending display-ptr))))

(defcstruct x-compose-status
    (compose-ptr :pointer) (chars-matched :int))

(defcfun ("XLookupString" %x-lookup-string) :int
  (evt x-key-event) (buffer-return :pointer) (bytes-buffer :int)
  (keysym-return :pointer) (status-in-out :pointer))

;; Only define interesting keysym and just use iso-8859-1 for the remaining ones
;; too many keysyms exists and I don't want to write so many things right now
;; TODO: add keypad support but I don't have it on the laptop here
(defcenum x-keysym-value
  ;; cursor control & motion
  (:key-left #xff51)
  :key-up
  :key-right
  :key-down
  :key-prior
  :key-page-up
  :key-next
  :key-page-down
  :key-end
  :key-begin
  ;; function keys
  (:key-f1 #xffbe)
  :key-f2
  :key-f3
  :key-f4
  :key-f5
  :key-f6
  :key-f7
  :key-f8
  :key-f9
  :key-f10
  :key-f11)

(defun x-lookup-key (key-event)
  "Returns either a char or an x-keysym-value keyword."
  (with-foreign-objects ((buffer :char #x20) (keysym 'keysym) (compose 'x-compose-status))
    (%x-lookup-string key-event buffer #x20 keysym compose)
    ;; do we have an interesting keysym?
    (let ((sym (foreign-enum-keyword 'x-keysym-value (mem-ref keysym 'keysym) :errorp nil)))
      (or sym (code-char (mem-aref buffer :char 0))))))


(defun process-event (evt)
  (with-foreign-slots ((type) evt x-event)
    (case type
      (:key-press
       (with-foreign-slots ((keycode) evt x-key-pressed-event)
         (format t "Pressed key: ~S~%" (x-lookup-key evt))))
      (:key-release
       (with-foreign-slots ((keycode) evt x-key-pressed-event)
         (format t "Pressed key: ~S~%" (x-lookup-key evt))))
      (:button-press
       (with-foreign-slots ((button) evt x-button-pressed-event)
         (format t "Pressed button: ~S~%" button)))
      (:button-release
       (with-foreign-slots ((button) evt x-button-pressed-event)
         (format t "Pressed button: ~S~%" button)))
      (:motion-notify
       (format t "Motion !!~%"))
      (:expose
       (format t "Expose !! ~%"))
      (:configure-notify
       (format t "Configure !! ~%"))
      (:map-notify
       (format t "Map !! ~%"))
      (:unmap-notify
       (format t "Unmap !! ~%"))
      (:client-message
       (format t "Client message !! ~%"))
      (t (format t "Unhandled event: ~S~%" type)))))

(defun dispatch-events (dpy)
  (with-foreign-object (evt 'x-event)
      (x-next-event dpy evt)
      (process-event evt)))

(defun dispatch-events-no-block (dpy)
  (when (x-pending-p dpy)
    (dispatch-events dpy)))

(defctype x-status :int)

(defcfun ("XGetGeometry" %x-get-geometry) x-status
  (display-ptr :pointer) (d drawable) (root-return :pointer)
  (x-return :pointer) (y-return :pointer) (width-return :pointer)
  (height-return :pointer) (border-width-return :pointer)
  (depth-return :pointer))

(defun x-get-geometry (dpy win)
  (with-foreign-objects ((root 'window) (x :int) (y :int)
                         (width :unsigned-int) (height :unsigned-int)
                         (border-width :unsigned-int) (depth :unsigned-int))
    (%x-get-geometry dpy win root x y width height border-width depth)
    (values (mem-ref root 'window) (mem-ref x :int) (mem-ref y :int)
            (mem-ref width :unsigned-int) (mem-ref height :unsigned-int)
            (mem-ref border-width :unsigned-int) (mem-ref depth :unsigned-int))))

;; GLX

(define-foreign-library opengl
  (t (:default "libGL")))
(use-foreign-library opengl)

(defcenum (glx-attributes :int)
  (:use-gl 1)
  (:buffer-size)
  (:level)
  (:rgba)
  (:double-buffer)
  (:stereo )
  (:aux-buffers)
  (:red-size)
  (:green-size)
  (:blue-size)
  (:alpha-size)
  (:depth-size)
  (:stencil-size)
  (:accum-red-size)
  (:accum-green-size)
  (:accum-blue-size)
  (:accum-alpha-size))

(defcenum (glx-config-errors :unsigned-int)
  (:bad-screen 1)
  (:bad-attribute)
  (:no-extension)
  (:bad-visual)
  (:bad-context)
  (:bad-value)
  (:bad-enum))

(defcfun ("glXChooseVisual" %glx-choose-visual) visual-info
  (display-ptr :pointer) (screen :int) (attribs :pointer))

(defun glx-choose-visual (dpy screen attribs)
  (with-foreign-object (atts :int (1+ (length attribs)))
    (loop for i below (length attribs)
         for attr = (nth i attribs)
       do  (setf (mem-aref atts :int i)
                 (typecase attr
                   (keyword (foreign-enum-value 'glx-attributes attr))
                   (t attr))))
    (setf (mem-aref atts :int (length attribs)) 0)
    (%glx-choose-visual dpy screen atts)))

(defctype glx-context :pointer)

(defcfun ("glXCreateContext" %glx-create-context) glx-context
  (display-ptr :pointer) (visual-infos :pointer) (share-list glx-context)
  (redirect bool))

(defcfun ("glXDestroyContext" %glx-destroy-context) :void
  (display-ptr :pointer) (context glx-context))

(defcfun ("glXMakeCurrent" %glx-make-current) bool
  (display-ptr :pointer) (drawable drawable) (context glx-context))

(defcfun ("glXQueryVersion" %glx-query-version) bool
  (display-ptr :pointer) (major :pointer) (minor :pointer))

(defun glx-get-version (dpy)
  (with-foreign-objects ((major :int) (minor :int))
    (%glx-query-version dpy major minor)
    (values (mem-ref major :int) (mem-ref minor :int))))

(defcfun ("glXSwapBuffers" %glx-swap-buffers) :void
  (display-ptr :pointer) (drawable drawable))

;; GLOP functions
(in-package #:glop)

(defstruct system-window
  display
  screen
  id
  visual-infos)

(defstruct gl-context
  glx-context
  display)

(defun create-gl-context (window)
  (let ((ctx (make-gl-context
              :glx-context (glop-x11::%glx-create-context (system-window-display window)
                                                          (system-window-visual-infos window)
                                                          (cffi::null-pointer) 1)
              :display (system-window-display window))))
    ctx))

(defun set-gl-context (ctx win)
  (glop-x11::%glx-make-current (gl-context-display ctx)
                               (system-window-id win)
                               (gl-context-glx-context ctx)))

(defun destroy-gl-context (ctx)
  (glop-x11::%glx-destroy-context (gl-context-display ctx)
                                  (gl-context-glx-context ctx)))

(defun swap-gl-buffers (win)
  (glop-x11::%glx-swap-buffers (system-window-display win) (system-window-id win)))

(defun create-system-window (title width height)
  (let ((win (make-system-window :display (glop-x11::x-open-display "")
                          :screen 0))
        (visual-format '(:rgba
                         :red-size 4
                         :green-size 4
                         :blue-size 4
                         :alpha-size 4
                         :depth-size 24
                         :double-buffer)))
    (setf (system-window-visual-infos win) (glop-x11::glx-choose-visual (system-window-display win)
                                                                 (system-window-screen win)
                                                                 visual-format))
    (setf (system-window-id win) (glop-x11::x-create-window
                           (system-window-display win)
                           (glop-x11::%x-default-root-window (system-window-display win))
                           width height (system-window-visual-infos win)))
    (glop-x11::%x-store-name (system-window-display win) (system-window-id win) title)
    (glop-x11::%x-map-raised (system-window-display win) (system-window-id win))
    win))

(defun destroy-system-window (window)
  (glop-x11::x-destroy-window (system-window-display window) (system-window-id window))
  (glop-x11::x-close-display (system-window-display window)))


(defun dispatch-system-window-events (win &key (blocking nil))
  (if blocking
      (glop-x11::dispatch-events (system-window-display win))
      (glop-x11::dispatch-events-no-block (system-window-display win))))