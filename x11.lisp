(defpackage :glop-x11
  (:use #:cl #:cffi)
  (:export))

(in-package #:glop-x11)

(defctype xid :unsigned-long)
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

(defcenum (x-alloc :int)
  (:alloc-none)
  (:alloc-all))

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

(defctype x-atom :unsigned-long)

(defcunion x-client-message-event-data
  (b :char :count 20)
  (s :short :count 10)
  (l :long :count 5))

(defcstruct x-client-message-event
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display-ptr :pointer)
  (win window)
  (message-type x-atom)
  (format :int)
  (data x-client-message-event-data))

(defcstruct x-expose-event
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display-ptr :pointer)
  (win window)
  (x :int) (y :int)
  (width :int) (height :int)
  (count :int))

(defcstruct x-configure-event
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display-ptr :pointer)
  (event window)
  (win window)
  (x :int) (y :int)
  (width :int) (height :int)
  (border-width :int)
  (above window)
  (override-reirect bool))

(defcstruct x-map-event
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display-ptr :pointer)
  (event window)
  (win window)
  (override-redirect bool))

(defcstruct x-unmap-event
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display-ptr :pointer)
  (event window)
  (win window)
  (from-configure bool))

(defcunion x-event
  (type x-event-name)
  (pad :long :count 24))

(defcstruct x-compose-status
    (compose-ptr :pointer) (chars-matched :int))

;; Only define interesting keysym and just use iso-8859-1 for the remaining ones
;; too many keysyms exists and I don't want to write so many things right now.
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

(defctype x-status :int)

;; GLX
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

;; X11 bindings
(define-foreign-library xlib
  (t (:default "libX11")))
(use-foreign-library xlib)

(defcfun ("XOpenDisplay" x-open-display) :pointer
  (display-name :string))

(defcfun("XCloseDisplay" x-close-display) :pointer
  (display-ptr :pointer))

(defcfun ("XDefaultRootWindow" x-default-root-window) window
  (display-ptr :pointer))

(defcfun ("XCreateColormap" x-create-color-map) colormap
  (display-ptr :pointer) (win window) (visual-ptr :pointer) (alloc x-alloc))

(defcfun ("XGetAtomName" x-get-atom-name) :string
  (display-ptr :pointer) (atm x-atom))

(defcfun ("XFree" x-free) :int
  (data :pointer))

(defcfun ("XCreateWindow" %x-create-window) window
  (display-ptr :pointer) (parent window) (x :int) (y :int) (width :int) (height :int)
  (border-width :int) (depth :int) (win-class x-window-class) (visual :pointer)
  (value-mask x-window-attributes-flags) (attributes set-window-attributes))

(defun x-create-window (dpy parent width height visual-infos)
  (let ((root-win (x-default-root-window dpy)))
    (with-foreign-slots ((visual-id visual depth) visual-infos visual-info)
      (let ((colormap (x-create-color-map dpy root-win visual :alloc-none)))
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
            (%x-create-window dpy parent 0 0 width height 0
                              depth :input-output visual
                              '(:cw-colormap :cw-event-mask)
                              win-attrs)))))))


(defcfun ("XDestroyWindow" x-destroy-window) :int
  (display-ptr :pointer) (win window))

(defcfun ("XMapWindow" x-map-window) :int
  (display-ptr :pointer) (win window))

(defcfun ("XMapRaised" x-map-raised) :int
  (display-ptr :pointer) (win window))

(defcfun ("XUnmapWindow" x-unmap-window) :int
  (display-ptr :pointer) (win window))

(defcfun ("XStoreName" x-store-name) :int
  (display-ptr :pointer) (win window) (name :string))

(defcfun ("XNextEvent" %x-next-event) :int
  (display-ptr :pointer) (evt x-event))

(defcfun ("XPending" %x-pending) :int
  (display-ptr :pointer))

(defun x-pending-p (display-ptr)
  (not (zerop (%x-pending display-ptr))))

(defun x-next-event (dpy &optional blocking)
  (with-foreign-object (evt 'x-event)
    (if blocking
        (progn (%x-next-event dpy evt)
               (process-event evt))
        (progn (when (x-pending-p dpy)
                 (%x-next-event dpy evt)
                 (process-event evt))))))

(defun x-translate-mouse-button (button)
  (case button
    (1 :left-button)
    (2 :middle-button)
    (3 :right-button)
    (4 :wheel-up)
    (5 :wheel-down)))

(let ((last-x 0)
      (last-y 0))
  (defun process-event (evt)
    "Process an X11 event into a GLOP event."
    (with-foreign-slots ((type) evt x-event)
      (case type
        (:key-press
         (with-foreign-slots ((keycode) evt x-key-pressed-event)
           (glop::make-event :type :key-press
                             :key (x-lookup-key evt))))
        (:key-release
         (with-foreign-slots ((keycode) evt x-key-released-event)
           (glop::make-event :type :key-release
                             :key (x-lookup-key evt))))
        (:button-press
         (with-foreign-slots ((button) evt x-button-pressed-event)
           (glop::make-event :type :button-press
                             :button (x-translate-mouse-button button))))
        (:button-release
         (with-foreign-slots ((button) evt x-button-pressed-event)
           (glop::make-event :type :button-release
                             :button (x-translate-mouse-button button))))
        (:motion-notify
         (with-foreign-slots ((x y) evt x-motion-event)
           (let ((glop-evt (glop::make-event :type :mouse-motion
                                             :x x :y y :dx (- x last-x) :dy (- y last-y))))
             (setf last-x x last-y y)
             glop-evt)))
        (:expose
         (with-foreign-slots ((display-ptr win) evt x-expose-event)
           (multiple-value-bind (root x y width height border-width depth)
               (x-get-geometry display-ptr win)
             (glop::make-event :type :expose
                               :width width :height height))))
        (:configure-notify
         (with-foreign-slots ((width height) evt x-configure-event)
           (glop::make-event :type :configure
                             :width width :height height)))
        (:map-notify
         (glop::make-event :type :show))
        (:unmap-notify
         (glop::make-event :type :hide))
        (:client-message
         (with-foreign-slots ((display-ptr message-type data) evt x-client-message-event)
           (with-foreign-slots ((l) data x-client-message-event-data)
             (let ((atom-name (x-get-atom-name display-ptr (mem-ref l :long))))
               (when (string= atom-name "WM_DELETE_WINDOW")
                 (glop::make-event :type :close))))))
        (t (format t "Unhandled event: ~S~%" type))))))

(defcfun ("XLookupString" %x-lookup-string) :int
  (evt x-key-event) (buffer-return :pointer) (bytes-buffer :int)
  (keysym-return :pointer) (status-in-out :pointer))

(defun x-lookup-key (key-event)
  "Returns either a char or an x-keysym-value keyword."
  (with-foreign-objects ((buffer :char #x20) (keysym 'keysym) (compose 'x-compose-status))
    (%x-lookup-string key-event buffer #x20 keysym compose)
    ;; do we have an interesting keysym?
    (let ((sym (foreign-enum-keyword 'x-keysym-value (mem-ref keysym 'keysym) :errorp nil)))
      (or sym (code-char (mem-aref buffer :char 0))))))

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

(defcfun ("glXChooseVisual" %glx-choose-visual) visual-info
  (display-ptr :pointer) (screen :int) (attribs :pointer))

(defun glx-choose-visual (dpy screen &rest attribs)
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

(defun glx-create-context (dpy visual)
  (%glx-create-context dpy visual (null-pointer) 1))

(defcfun ("glXDestroyContext" glx-destroy-context) :void
  (display-ptr :pointer) (context glx-context))

(defcfun ("glXMakeCurrent" glx-make-current) bool
  (display-ptr :pointer) (drawable drawable) (context glx-context))

(defun glx-release-context (dpy)
  (glx-make-current dpy 0
                    (null-pointer)))

(defcfun ("glXQueryVersion" %glx-query-version) bool
  (display-ptr :pointer) (major :pointer) (minor :pointer))

(defun glx-get-version (dpy)
  (with-foreign-objects ((major :int) (minor :int))
    (%glx-query-version dpy major minor)
    (values (mem-ref major :int) (mem-ref minor :int))))

(defcfun ("glXSwapBuffers" glx-swap-buffers) :void
  (display-ptr :pointer) (drawable drawable))

;; GLOP implementation
(in-package #:glop)

(defstruct (x11-window (:include window))
  display      ;; X display ptr
  screen       ;; X screen number
  id           ;; X window ID
  visual-infos ;; X visual format of the window
)

(defstruct glx-context
  ctx           ;; GL context ptr
  display       ;; X display ptr
)

(defmethod create-gl-context ((win x11-window) &key (make-current t))
  (let ((ctx (make-glx-context
              :ctx (glop-x11::glx-create-context (x11-window-display win)
                                                 (x11-window-visual-infos win))
              :display (x11-window-display win))))
    (when make-current
      (attach-gl-context win ctx))
    ctx))

(defmethod destroy-gl-context (ctx)
  (detach-gl-context ctx)
  (glop-x11::glx-destroy-context (glx-context-display ctx)
                                 (glx-context-ctx ctx)))

(defmethod attach-gl-context ((win x11-window) (ctx glx-context))
  (setf (window-gl-context win) ctx)
  (glop-x11::glx-make-current (glx-context-display ctx)
                              (x11-window-id win)
                              (glx-context-ctx ctx)))

(defmethod detach-gl-context ((ctx glx-context))
  (glop-x11::glx-release-context (glx-context-display ctx)))

(defmethod create-window (title width height &key (double-buffer t) accum (alpha t) (depth 24))
  (let ((win (make-x11-window :display (glop-x11::x-open-display "")
                              :screen 0)))
    ;; create visual
    (setf (x11-window-visual-infos win)
          (glop-x11::glx-choose-visual (x11-window-display win)
                                       (x11-window-screen win)
                                       :rgba
                                       :red-size 4
                                       :green-size 4
                                       :blue-size 4
                                       :alpha-size (if alpha 4 0)
                                       :depth-size depth
                                       (if double-buffer :double-buffer :single-buffer)))
    ;; create window
    (setf (x11-window-id win) (glop-x11::x-create-window
                               (x11-window-display win)
                               (glop-x11::x-default-root-window (x11-window-display win))
                               width height (x11-window-visual-infos win)))
    (setf (window-width win) width)
    (setf (window-height win) height)
    ;; set title
    (glop-x11::x-store-name (x11-window-display win) (x11-window-id win) title)
    (setf (slot-value win 'title) title)
    ;; create a GL context and make it current
    (setf (window-gl-context win) (create-gl-context win :make-current t))
    ;; show created window
    (show-window win)
    ;; return created window
    win))

(defmethod show-window ((win x11-window))
  (glop-x11::x-map-raised (x11-window-display win) (x11-window-id win)))

(defmethod hide-window ((win x11-window))
  (glop-x11::x-unmap-window (x11-window-display win) (x11-window-id win)))

(defmethod set-window-title ((win x11-window) title)
  (setf (slot-value win 'title) title)
  (glop-x11::x-store-name (x11-window-display win) (x11-window-id win) title))

(defmethod destroy-window ((win x11-window))
  (glop-x11::x-destroy-window (x11-window-display win) (x11-window-id win))
  (glop-x11::x-close-display (x11-window-display win)))

(defmethod swap-buffers ((win x11-window))
  (glop-x11::glx-swap-buffers (x11-window-display win) (x11-window-id win)))

(defmethod next-event ((win x11-window) &key blocking)
  (glop-x11::x-next-event (x11-window-display win) blocking))

