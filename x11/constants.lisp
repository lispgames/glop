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
  (:accum-alpha-size)
  (:sample-buffers 100000)
  (:samples 100001))

(defcenum (gl-enum :unsigned-int)
  (:version #x1F02))

(defcenum (glx-context-attributes :unsigned-int)
  (:major-version #x2091)
  (:minor-version #x2092)
  (:flags #x2094)
  (:core-profile-bit #x00000001)
  (:compatibility-profile-bit #x00000002)
  (:profile-mask #x9126)
  (:debug-bit #x00000001)
  (:forward-compatible-bit #x00000002))

(defcenum (glx-config-errors :unsigned-int)
  (:bad-screen 1)
  (:bad-attribute)
  (:no-extension)
  (:bad-visual)
  (:bad-context)
  (:bad-value)
  (:bad-enum))
