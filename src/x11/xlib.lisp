;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;; Xlib bindings
(in-package #:glop-xlib)

(defctype xid :unsigned-long)
(defctype window xid)
(defctype font xid)
(defctype drawable xid)
(defctype colormap xid)
(defctype pixmap xid)
(defctype cursor xid)
(defctype gcontext xid)

(defcstruct _xdisplay)
(defctype display _xdisplay)

(defctype visualid :unsigned-long)

(defctype watch-proc :pointer)

(defcstruct xcolor
  (pixel :ulong)
  (red :ushort) (green :ushort) (blue :ushort)
  (flags :char) (pad :char))

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
  (save-under :boolean) (event-mask :long) (do-not-propagate-mask :long)
  (override-redirect :boolean) (cmap colormap) (curs cursor))

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
  (send-event :boolean)
  (display-ptr :pointer)
  (win window)
  (root window)
  (subwin window)
  (time x-time)
  (x :int) (y :int)
  (x-root :int) (y-root :int)
  (state :unsigned-int)
  (keycode :unsigned-int)
  (same-screen :boolean))

(defctype x-key-pressed-event x-key-event)
(defctype x-key-released-event x-key-event)

(defcstruct x-button-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (win window)
  (root window)
  (subwin window)
  (time x-time)
  (x :int) (y :int)
  (x-root :int) (y-root :int)
  (state :unsigned-int)
  (button :unsigned-int)
  (same-screen :boolean))

(defctype x-button-pressed-event x-button-event)
(defctype x-button-released-event x-button-event)

(defcstruct x-motion-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (win window)
  (root window)
  (subwin window)
  (time x-time)
  (x :int) (y :int)
  (x-root :int) (y-root :int)
  (state :unsigned-int)
  (is-hint :char)
  (same-screen :boolean))

(defctype x-pointer-moved-event x-motion-event)

(defctype x-atom :unsigned-long)

(defcunion x-client-message-event-data
  (b :char :count 20)
  (s :short :count 10)
  (l :long :count 5))

(defcstruct x-client-message-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (win window)
  (message-type x-atom)
  (format :int)
  (data x-client-message-event-data))

(defcstruct x-expose-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (win window)
  (x :int) (y :int)
  (width :int) (height :int)
  (count :int))

(defcstruct x-configure-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (event window)
  (win window)
  (x :int) (y :int)
  (width :int) (height :int)
  (border-width :int)
  (above window)
  (override-reirect :boolean))

(defcstruct x-map-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (event window)
  (win window)
  (override-redirect :boolean))

(defcstruct x-unmap-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (event window)
  (win window)
  (from-configure :boolean))

(defcenum visibility-state
  (:unobscured 0)
  (:partially-obscured 1)
  (:fully-obscured 2))

(defcstruct x-visibility-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (window window)
  (state visibility-state))

(defcstruct x-focus-change-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (window window)
  (mode :int)
  (detail :int))

(defcunion x-event
  (type :int)
  (pad :long :count 24))

(defcstruct x-compose-status
    (compose-ptr :pointer) (chars-matched :int))

(defctype x-status :int)

(defctype x-queued-mode :int)

;; X11 bindings
(define-foreign-library xlib
  (t (:default "libX11")))
(use-foreign-library xlib)

(defcfun ("XDisplayWidth" display-width) :int
  (display-ptr :pointer)
  (screen :int))

(defcfun ("XDisplayHeight" display-height) :int
  (display-ptr :pointer)
  (screen :int))

(defcfun ("XDefaultDepth" default-depth) :int
  (display-ptr :pointer)
  (screen :int))

(defcfun ("XRootWindow" root-window) :int
  (display-ptr :pointer)
  (screen :int))

(defcfun ("XSendEvent" x-send-event) :int
  (display-ptr :pointer)
  (win window)
  (propogate :boolean)
  (event-mask :long)
  (event-send :pointer))

(defcfun ("XInternAtom" x-intern-atom) x-atom
  (display-ptr :pointer)
  (atom-name :string)
  (only-if-exists :boolean))

(defcfun ("XGetVisualInfo" get-visual-info) :pointer
  (display-ptr :pointer)
  (vinfo-mask :int)
  (vinfo-template :pointer)
  (nitems-returned :pointer))

(defcfun ("XOpenDisplay" %x-open-display) :pointer
  (display-name :string))

(defcfun ("XCloseDisplay" x-close-display) :pointer
  (display-ptr :pointer))

(defcfun ("XDefaultRootWindow" x-default-root-window) window
  (display-ptr :pointer))

(defcfun ("XCreateColormap" x-create-color-map) colormap
  (display-ptr :pointer) (win window) (visual-ptr :pointer) (alloc x-alloc))

(defcfun ("XGetAtomName" x-get-atom-name) :string
  (display-ptr :pointer) (atm x-atom))

(defcfun ("XFree" x-free) :int
  (data :pointer))

(defcfun ("XFlush" x-flush) :int
  (display-ptr :pointer))

(defcfun ("XCreateWindow" %x-create-window) window
  (display-ptr :pointer) (parent window) (x :int) (y :int) (width :int) (height :int)
  (border-width :int) (depth :int) (win-class x-window-class) (visual :pointer)
  (value-mask x-window-attributes-flags) (attributes set-window-attributes))

(defun %set-fullscreen (window dpy be-fullscreen)
  (let ((wm-state (x-intern-atom dpy "_NET_WM_STATE" nil))
        (fullscreen (x-intern-atom dpy "_NET_WM_STATE_FULLSCREEN" nil)))
    (with-foreign-object (msg 'x-event)
      (with-foreign-slots ((type) msg x-event)
        (setf type (foreign-enum-value 'x-event-name :client-message))
        (with-foreign-slots ((win message-type format data) msg x-client-message-event) 
        (setf win window)
        (setf message-type wm-state)
        (setf format 32)
        (with-foreign-slots ((l) data x-client-message-event-data)
          (setf (mem-aref l :long 0) (if be-fullscreen 1 0))
          (setf (mem-aref l :long 1) fullscreen)
          (setf (mem-aref l :long 2) 0))))
      (x-send-event dpy (x-default-root-window dpy) nil (foreign-bitfield-value 'x-event-mask-flags '(:structure-notify-mask)) msg))))

(defun x-open-display (&optional (display-name (null-pointer)))
  (let ((display (%x-open-display display-name)))
    (when (null-pointer-p display)
      (error "Unable to open display"))
    display))

(defmacro with-current-display (dpy-sym &body body)
  `(let ((,dpy-sym (x-open-display)))
     (unwind-protect
          (progn ,@body)
       (x-close-display ,dpy-sym))))

(defun x-create-window (dpy parent x y width height visual-infos)
  (let ((root-win (x-default-root-window dpy)))
    (with-foreign-slots ((visual-id visual depth) visual-infos visual-info)
      (let ((colormap (x-create-color-map dpy root-win visual :alloc-none)))
        (with-foreign-object (win-attrs 'set-window-attributes)
          (with-foreign-slots ((cmap event-mask border-pixel) win-attrs set-window-attributes)
            (setf cmap colormap
                  event-mask (foreign-bitfield-value 'x-event-mask-flags
                                '(:exposure-mask
                                  :key-press-mask :key-release-mask
                                  :button-press-mask :button-release-mask
                                  :structure-notify-mask
                                  :visibility-change-mask
                                  :pointer-motion-mask))
                  border-pixel 0)
            (%x-create-window dpy parent x y  width height 0
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

(defcfun ("XSync" x-sync) :int
  (display-ptr :pointer) (discard :boolean))

(defcfun ("XNextEvent" %x-next-event) :int
  (display-ptr :pointer) (evt x-event))

(defcfun ("XEventsQueued" %x-events-queued) :int
  (display-ptr :pointer) (mode x-queued-mode))

(defcfun ("XPeekEvent" %x-peek-event) :int
  (display-ptr :pointer) (evt x-event))

(defcfun ("XPending" %x-pending) :int
  (display-ptr :pointer))

(defcfun ("XSetWMProtocols" x-set-wm-protocols) :int
  (display-ptr :pointer) (win window) (atoms :pointer) (count :int))

(defun x-pending-p (display-ptr)
  (not (zerop (%x-pending display-ptr))))

(defun x-next-event (win dpy &optional blocking)
  (x-sync dpy nil)
  (with-foreign-object (evt 'x-event)
    (if blocking
        (progn (%x-next-event dpy evt)
               (process-event win dpy evt))
        (progn (when (x-pending-p dpy)
                 (%x-next-event dpy evt)
                 (process-event win dpy evt))))))

(let ((last-x 0)
      (last-y 0))
  (defun process-event (win dpy evt)
    "Process an X11 event into a GLOP event."
    (with-foreign-slots ((type) evt x-event)
      (case (foreign-enum-keyword 'x-event-name type :errorp nil)
        (:key-press
         (with-foreign-slots ((keycode) evt x-key-event)
           (when (and glop:*ignore-auto-repeat* (glop:key-pressed keycode))
             (return-from process-event))
           (setf (glop:key-pressed keycode) t)
           (multiple-value-bind (text keysym) (x-lookup-string evt)
             (make-instance 'glop:key-press-event
                            :keycode keycode
                            :keysym keysym
                            :text text))))
        (:key-release
         (with-foreign-slots ((keycode win time) evt x-key-event)
           ;; ignore key release for key repeats
           ;; if a key is repeated we now get the same behavior as win32
           ;; i.e. multiple :key-press events without corresponding key release
           ;; the following is translated from glfw:x11_window.c:612
           (when (and (x-pending-p dpy) (%x-events-queued dpy 1)) ;; 1 is QueuedAfterReading
             (with-foreign-object (next-evt 'x-event)
               (%x-peek-event dpy next-evt)
               (when (and (eq :key-press
                              (foreign-enum-keyword 'x-event-name
                                   (foreign-slot-value next-evt 'x-key-event 'type)
                                   :errorp nil))
                          (eq win (foreign-slot-value next-evt 'x-key-event 'win))
                          (eq time (foreign-slot-value next-evt 'x-key-event 'time)))
                 (return-from process-event))))
           (setf (glop:key-pressed keycode) nil)
           (multiple-value-bind (text keysym) (x-lookup-string evt)
             (make-instance 'glop:key-release-event
                            :keycode keycode
                            :keysym keysym
                            :text text))))
        (:button-press
         (with-foreign-slots ((button) evt x-button-pressed-event)
           (make-instance 'glop:button-press-event
                          :button button)))
        (:button-release
         (with-foreign-slots ((button) evt x-button-pressed-event)
           (make-instance 'glop:button-release-event
                          :button button)))
        (:motion-notify
         (with-foreign-slots ((x y) evt x-motion-event)
           (let ((glop-evt (make-instance 'glop:mouse-motion-event
                                          :x x :y y
                                          :dx (- x last-x)
                                          :dy (- y last-y))))
             (setf last-x x last-y y)
             glop-evt)))
        (:expose
         (with-foreign-slots ((display-ptr win) evt x-expose-event)
           (multiple-value-bind (root x y width height border-width depth)
               (x-get-geometry display-ptr win)
             (declare (ignorable x y root border-width depth))
             (make-instance 'glop:expose-event
                            :width width :height height))))
        (:configure-notify
         (with-foreign-slots ((x y width height) evt x-configure-event)
           (glop::%update-geometry win x y width height)
           (make-instance 'glop:resize-event
                          :width width :height height)))
        (:map-notify
         (make-instance 'glop:visibility-unobscured-event))
        (:unmap-notify
         (make-instance 'glop:visibility-obscured-event))
        (:client-message
         (with-foreign-slots ((display-ptr message-type data) evt x-client-message-event)
           (with-foreign-slots ((l) data x-client-message-event-data)
             (let ((atom-name (x-get-atom-name display-ptr (mem-ref l :long))))
               (when (string= atom-name "WM_DELETE_WINDOW")
                 (make-instance 'glop:close-event))))))
        (:visibility-notify
         (with-foreign-slots ((state) evt x-visibility-event)
           (case state
             (:unobscured
              (make-instance 'glop:visibility-unobscured-event))
             (:partially-obscured
              (make-instance 'glop:visibility-unobscured-event :visible :partial))
             (:fully-obscured
              (make-instance 'glop:visibility-obscured-event)))))
        (:focus-in
         (make-instance 'glop:focus-in-event))
        (:focus-out
         (make-instance 'glop:focus-out-event))
        ;; unhandled event
        (t nil)))))

(defcfun ("XLookupString" %x-lookup-string) :int
  (evt x-key-event) (buffer-return :pointer) (bytes-buffer :int)
  (keysym-return :pointer) (status-in-out :pointer))

(defun x-lookup-string (key-event)
  "Returns the input string corresponding to a keypress."
  (with-foreign-objects ((buffer :char 32) (keysym 'x-keysym-value))
    #+ccl(loop for i below 32 do (setf (mem-aref buffer :char i) 0)) ;; buffer is not zeroed in cll
    (%x-lookup-string key-event buffer 32 keysym (null-pointer))
    (let ((string (foreign-string-to-lisp buffer)))
      (values (if (zerop (length string)) nil string)
              (mem-ref keysym 'x-keysym-value)))))

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

(defbitfield x-window-configure-flags
  :cw-x
  :cw-y
  :cw-width
  :cw-height
  :cw-border-width
  :cw-sibling
  :cw-stack-mode)

(defcstruct x-window-changes
  (x :int) (y :int)
  (width :int) (height :int)
  (border-width :int)
  (sibling window)
  (stack-mode :int))

(defcfun ("XConfigureWindow" %x-configure-window) :int
  (display-ptr :pointer) (win window) (value-mask x-window-configure-flags)
  (values x-window-changes))

(defun x-set-geometry (dpy win x y width height)
  (with-foreign-object (changes 'x-window-changes)
    (setf (foreign-slot-value changes 'x-window-changes 'x) x
          (foreign-slot-value changes 'x-window-changes 'y) y
          (foreign-slot-value changes 'x-window-changes 'width) width
          (foreign-slot-value changes 'x-window-changes 'height) height)
    (%x-configure-window dpy win (foreign-bitfield-value 'x-window-configure-flags
                                                         '(:cw-x :cw-y :cw-width :cw-height))
                         changes)))

(defcfun ("XCreatePixmapCursor" %x-create-pixmap-cursor) cursor
  (display-ptr :pointer) (src pixmap) (mask pixmap)
  (foreground-color xcolor) (background-color xcolor)
  (x :uint) (y :uint))

(defcfun ("XCreatePixmap" %x-create-pixmap) pixmap
  (display-ptr :pointer) (d drawable)
  (width :uint) (height :uint)
  (depth :uint))

(defcenum xgcvalues-function
  (:GXclear #x0)
  :GXand
  :GXandReverse
  :GXcopy
  :GXandInverted
  :GXnoop
  :GXxor
  :GXor
  :GXnor
  :GXequiv
  :GXinvert
  :GXorReverse
  :GXcopyInverted
  :GXorInverted
  :GXnand
  :GXset)

(defcstruct xgcvalues
  (function xgcvalues-function)
  (plane-mask :ulong) (foreground :ulong) (background :ulong)
  (line-width :int) (line-style :int)
  (cap-style :int) (join-style :int) (fill-style :int)
  (fill-rule :int) (arc-mode :int)
  (tile pixmap) (stipple pixmap)
  (ts-x-origin :int) (ts-y-origin :int)
  (font font) (subwindow-mode :int)
  (graphics-exposures :boolean)
  (clip-x-origin :int) (clip-y-origin :int)
  (clip-mask pixmap)
  (dash-offset :int) (dashes :char))

(defbitfield xgcvalues-flags
  (:gcv-function #x0001))

(defcfun ("XCreateGC" %x-create-gc) gcontext
  (display-ptr :pointer) (d drawable)
  (value-mask xgcvalues-flags)
  (xgc-values-ptr :pointer))

(defcfun ("XFillRectangle" %x-fill-rectangle) :int
  (display-ptr :pointer) (d drawable) (gc gcontext)
  (x :int) (y :int) (width :uint) (height :uint))

(defun x-create-null-cursor (dpy win)
  (with-foreign-objects ((xgc 'xgcvalues) (col 'xcolor))
    (setf (foreign-slot-value xgc 'xgcvalues 'function)
          (foreign-enum-value 'xgcvalues-function :GXclear)
          (foreign-slot-value col 'xcolor 'pixel)
          0
          (foreign-slot-value col 'xcolor 'red)
          0
          (foreign-slot-value col 'xcolor 'flags)
          4)
    (let* ((cursor-mask (%x-create-pixmap dpy win 1 1 1))
           (gc (%x-create-gc dpy cursor-mask '(:gcv-function) xgc)))
      (%x-fill-rectangle dpy cursor-mask gc 0 0 1 1)
      (let ((cursor (%x-create-pixmap-cursor dpy cursor-mask cursor-mask
                                             col col 0 0)))
        (x-free-pixmap dpy cursor-mask)
        (x-free-gc dpy gc)
        cursor))))

(defcfun ("XFreePixmap" x-free-pixmap) :int
  (display-ptr :pointer) (p pixmap))

(defcfun ("XFreeGC" x-free-gc) :int
  (display-ptr :pointer) (gc gcontext))

(defcfun ("XFreeCursor" x-free-cursor) :int
  (display-ptr :pointer) (cursor cursor))

(defcfun ("XDefineCursor" x-define-cursor) :int
  (display-ptr :pointer) (win window)
  (cursor cursor))

(defcfun ("XUndefineCursor" x-undefine-cursor) :int
  (display-ptr :pointer) (win window))

(defcfun ("XAddConnectionWatch" add-connection-watch) x-status
  (display :pointer)
  (procedure watch-proc)
  (client-data :pointer))

(defcfun ("XRemoveConnectionWatch" remove-connection-watch) x-status
  (display :pointer)
  (procedure watch-proc)
  (client-data :pointer))

(defcfun ("XProcessInternalConnection" process-internal-connection) :void
  (display :pointer)
  (fd :int))
