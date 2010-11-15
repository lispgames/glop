;;;; ffi definitions based on XInput2.h and XI2.h with following copyright:
;;/*
;; * Copyright © 2009 Red Hat, Inc.
;; *
;; * Permission is hereby granted, free of charge, to any person obtaining a
;; * copy of this software and associated documentation files (the "Software"),
;; * to deal in the Software without restriction, including without limitation
;; * the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; * and/or sell copies of the Software, and to permit persons to whom the
;; * Software is furnished to do so, subject to the following conditions:
;; *
;; * The above copyright notice and this permission notice (including the next
;; * paragraph) shall be included in all copies or substantial portions of the
;; * Software.
;; *
;; * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; * DEALINGS IN THE SOFTWARE.
;; *
;; */


;;; XInput2 bindings
(in-package #:glop-xlib)

(define-foreign-library xi
  (t (:default "libXi")))
(use-foreign-library xi)

(defcenum xi-property-event-flags
  (:xi-property-deleted                       0)
  (:xi-property-created                       1)
  (:xi-property-modified                      2)
  )


;;; /* enter/Leave and Focus In/Out modes */
(defcenum xi-notify-mode
  (:XI-Notify-Normal                          0)
  (:XI-Notify-Grab                            1)
  (:XI-Notify-Ungrab                          2)
  (:XI-Notify-While-Grabbed                   3)
  (:XI-Notify-Passive-Grab                    4)
  (:XI-Notify-Passive-Ungrab                  5))

;;; /* Enter/Leave and focus In/out detail */
(defcenum xi-notify-detail
  (:XI-Notify-Ancestor                        0)
  (:XI-Notify-Virtual                         1)
  (:XI-Notify-Inferior                        2)
  (:XI-Notify-Nonlinear                       3)
  (:XI-Notify-Nonlinear-Virtual               4)
  (:XI-Notify-Pointer                         5)
  (:XI-Notify-Pointer-Root                    6)
  (:XI-Notify-Detail-None                     7))

;;; /* Passive grab types */
(defcenum xi-passive-grab-type
  (:XI-Grabtype-Button                        0)
  (:XI-Grabtype-Keycode                       1)
  (:XI-Grabtype-Enter                         2)
  (:XI-Grabtype-Focus-In                      3))

;;; /* Passive grab modifier */
(defcenum xi-passive-grab-modifier
  (:XI-Any-Modifier                           #.(ash 1 31))
  (:XI-Any-Button                             0)
  (:XI-Any-Keycode                            0))

;;; /* XIAllowEvents event-modes */
(defcenum xi-allow-event-modes
  (:XI-Async-Device                           0)
  (:XI-Sync-Device                            1)
  (:XI-Replay-Device                          2)
  (:XI-Async-Paired-Device                    3)
  (:XI-Async-Pair                             4)
  (:XI-Sync-Pair                              5))

;;; /* DeviceChangedEvent change reasons */
(defcenum xi-device-changed-event-reason
  (:XI-Slave-Switch                           1)
  (:XI-Device-Change                          2))

;;; /* Hierarchy flags */
(defbitfield xi-hierarchy-flag 
  (:XI-Master-Added 1)
  (:XI-Master-Removed)
  (:XI-Slave-Added)
  (:XI-Slave-Removed)
  (:XI-Slave-Attached)
  (:XI-Slave-Detached)
  (:XI-Device-Enabled)
  (:XI-Device-Disabled))

;;; /* ChangeHierarchy constants */
(defcenum xi-change-hierarchy-flags
  (:XI-Add-Master                             1)
  (:XI-Remove-Master                          2)
  (:XI-Attach-Slave                           3)
  (:XI-Detach-Slave                           4)
  ;; aliases
  (:XI-Attach-To-Master                        1)
  (:XI-Floating                              2))

;;; /* Valuator modes */
(defcenum xi-valuator-modes
  (:XI-Mode-Relative                          0)
  (:XI-Mode-Absolute                          1))

;;; /* Device types */
(defcenum xi-device-types
  (:XI-Master-Pointer                         1)
  (:XI-Master-Keyboard                        2)
  (:XI-Slave-Pointer                          3)
  (:XI-Slave-Keyboard                         4)
  (:XI-Floating-Slave                         5))

;;; /* Device classes */
(defcenum xi-device-classes
  (:XI-Key-Class                              0)
  (:XI-Button-Class                           1)
  (:XI-Valuator-Class                         2))

(defbitfield xi-event-masks
  (:XI-Device-Changed 2)
  (:XI-Key-Press)
  (:XI-Key-Release)
  (:XI-Button-Press)
  (:XI-Button-Release)
  (:XI-Motion)
  (:XI-Enter)
  (:XI-Leave)
  (:XI-Focus-In)
  (:XI-Focus-Out)
  (:XI-Hierarchy-Changed)
  (:XI-Property-Event)
  (:XI-Raw-Key-Press)
  (:XI-Raw-Key-Release)
  (:XI-Raw-Button-Press)
  (:XI-Raw-Button-Release)
  (:XI-Raw-Motion))

(defcstruct xi-add-master-info
  (type :int)
  (name :string)
  (send-core :boolean)
  (enable :boolean))

(defcstruct xi-remove-master-info
  (type :int)
  (device-id :int)
  (return-mode :int)
  (return-pointer :int)
  (return-keyboard :int))

(defcstruct xi-attach-slave-info
  (type :int)
  (device-id :int)
  (new-master :int))

(defcstruct xi-detach-slave-info
  (type :int)
  (device-id :int))

(defcunion xi-any-hierarchy-change-info
  (type :int)
  (add xi-add-master-info)
  (remove xi-remove-master-info)
  (attach xi-attach-slave-info)
  (detach xi-detach-slave-info))

(defcstruct xi-modifier-state
  (base :int)
  (latched :int)
  (locked :int)
  (effective :int))

;;typedef XIModifierState XIGroupState;
(defctype xi-group-state xi-modifier-state)

(defcstruct xi-button-state
  (mask-len :int)
  (mask (:pointer :unsigned-char)))

(defcstruct xi-valuator-state
  (mask-len :int)
  (mask (:pointer :unsigned-char))
  (values (:pointer :double)))

(defcstruct xi-event-mask
  (device-id :int)
  (mask-len :int)
  (mask :pointer))

(defcstruct xi-any-class-info
  (type :int)
  (source-id :int))

(defcstruct xi-button-class-info
  (type :int)
  (source-id :int)
  (num-buttons :int)
  (labels (:pointer x-atom))
  (state xi-button-state))

(defcstruct xi-key-class-info
  (type :int)
  (source-id :int)
  (num-keycodes :int)
  (keycodes (:pointer :int)))

(defcstruct xi-valuator-class-info
  (type :int)
  (source-id :int)
  (number :int)
  (label x-atom)
  (min :double)
  (max :double)
  (value :double)
  (resolution :int)
  (mode :int))

(defcstruct xi-device-info
  (device-id :int)
  (name :string)
  (use :int)
  (attachment :int)
  (enabled :boolean)
  (num-classes :int)
  (classes (:pointer (:pointer xi-any-class-info))))

(defcstruct xi-grab-modifiers
  (modifiers :int)
  (status :int))

(defcstruct xi-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (extension :int)
  (evtype :int)
  (time x-time))

(defcstruct xi-hierarchy-info
  (device-id :int)
  (attachment :int)
  (use :int)
  (enabled :boolean)
  (flags :int))

(defcstruct xi-hierarchy-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (extension :int)
  (evtype :int)
  (time x-time)
  (flags :int)
  (num-info :int)
  (info (:pointer xi-hierarchy-info)))

(defcstruct xi-device-changed-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (extension :int)
  (evtype :int)
  (time x-time)
  (device-id :int)
  (source-id :int)
  (reason :int)
  (num-classes :int)
  (classes (:pointer (:pointer xi-any-class-info))))

(defcstruct xi-device-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (extension :int)
  (evtype :int)
  (time x-time)
  (device-id :int)
  (source-id :int)
  (detail :int)
  (root window)
  (event-window window)
  (child window)
  (x-root :double)
  (y-root :double)
  (x :double)
  (y :double)
  (flags :int)
  (buttons xi-button-state)
  (valuators xi-valuator-state)
  (mods xi-modifier-state)
  (group xi-modifier-state #++ xi-group-state))

(defcstruct xi-raw-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (extension :int)
  (evtype :int)
  (time x-time)
  (device-id :int)
  (source-id :int)
  (detail :int)
  (fags :int)
  (valuators xi-valuator-state)
  (raw-values (:pointer :double)))

(defcstruct xi-enter-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (extension :int)
  (evtype :int)
  (time x-time)
  (device-id :int)
  (source-id :int)
  (detail :int)
  (root window)
  (event window)
  (child window)
  (root-x :double)
  (root-y :double)
  (event-x :double)
  (event-y :double)
  (node :int)
  (focus :boolean)
  (same-screen :boolean)
  (buttons xi-button-state)
  (mods xi-modifier-state)
  (group xi-group-state))

;; typedef XIEnterEvent XILeaveEvent;
;; typedef XIEnterEvent XIFocusInEvent;
;; typedef XIEnterEvent XIFocusOutEvent;
(defctype xi-leave-event xi-enter-event)
(defctype xi-focus-in-event xi-enter-event)
(defctype xi-focus-out-event xi-enter-event)


(defcstruct xi-property-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display-ptr :pointer)
  (extension :int)
  (evtype :int)
  (time x-time)
  (device-id :int)
  (property x-atom)
  (what :int))

(defcfun ("XIQueryPointer" %xi-query-pointer) :boolean
  (display-ptr :pointer)
  (device-id :int)
  (win window)
  (root (:pointer window))
  (child (:pointer window))
  (root-x (:pointer :double))
  (root-y (:pointer :double))
  (win-x (:pointer :double))
  (win-y (:pointer :double))
  (buttons (:pointer xi-button-state))
  (mods (:pointer xi-modifier-state))
  (group (:pointer xi-group-state)))


(defcfun ("XIWarpPointer" xi-warp-pointer) :boolean
  (display-ptr :pointer)
  (device-id :int)
  (src-window window)
  (dst-window window)
  (src-x :double)
  (src-y :double)
  (src-width :unsigned-int)
  (src-height :unsigned-int)
  (dst-x :double)
  (dst-y :double))

(defcfun ("XIDefineCursor" xi-define-cursor) x-status
  (display-ptr :pointer)
  (device-id :int)
  (win window)
  (cursor cursor))

(defcfun ("XIUndefineCursor" xi-undefine-cursor) x-status
  (display-ptr :pointer)
  (device-id :int)
  (win window))

(defcfun ("XIChangeHierarchy" %xi-change-hierarchy) x-status
  (display-ptr :pointer)
  (changes (:pointer xi-any-hierarchy-change-info))
  (num-changes :int))

(defcfun ("XISetClientPointer" xi-set-client-pointer) x-status
  (display-ptr :pointer)
  (win window)
  (device-id :int))

(defcfun ("XIGetClientPointer" %xi-get-client-pointer) :boolean
  (display-ptr :pointer)
  (win window)
  (device-id (:pointer :int)))


(defcfun ("XISelectEvents" %xi-select-events) x-status
  (display-ptr :pointer)
  (window window)
  (masks (:pointer xi-event-mask))
  (num-masks :int))

;; fixme: use the enums instead of these constants
(defconstant +xi-all-devices+ 0)
(defconstant +xi-all-master-devices+ 1)

(defun xi-select-events (display window device-id &rest events)
  ;; fixme: this might need optimized if this gets called much
  ;; (compiler macros to handle constant event list at compile time, etc)
  (let* ((mask-val (foreign-bitfield-value 'xi-event-masks events))
         (octets (1+ (floor (integer-length mask-val) 8))))
    (with-foreign-objects ((event-mask 'xi-event-mask)
                           (mask-octets :unsigned-char octets))
      (loop for i below octets
         do (setf (mem-aref mask-octets :unsigned-char i)
                  ;; fixme: verify byte-order here
                  (ldb (byte 8 (* i 8)) mask-val)))
      (setf (foreign-slot-value event-mask 'xi-event-mask 'device-id)
            (cond ((numberp device-id) device-id)
                  ((eq device-id :all-devices) +xi-all-devices+)
                  ((eq device-id :all-master-devices) +xi-all-master-devices+)
                  (t (error "unknown device id ~s, expected number or :all-devices or :all-master-devices" device-id)))
            (foreign-slot-value event-mask 'xi-event-mask 'mask) mask-octets
            (foreign-slot-value event-mask 'xi-event-mask 'mask-len) octets)
      ;; todo: add version for specifying multiple masks/devices at once?
      ;; not sure if we would want 1 mask for multiple devices, or
      ;; separate mask for each device?
      (%xi-select-events display window event-mask 1))))

(defcfun ("XIGetSelectedEvents" %xi-get-selected-events) (:pointer xi-event-mask)
  (display-ptr :pointer)
  (win window)
  (num-masks-return (:pointer :int)))

(defcfun ("XIQueryVersion" %xi-query-version) x-status
  (display-ptr :pointer)
  (major-version-inout (:pointer :int))
  (minor-version-inout (:pointer :int)))

;; fixme: need to verify we actually loaded the lib properly, and return
;; failure of some sort if not
;; (or possibly add that as a separate function which should be checked
;;  by calling code before trying to use xi2 stuff?)
(defun xi-query-version (display major minor)
  (with-foreign-objects ((&major :int) (&minor :int))
    (setf (mem-ref &major :int) major
          (mem-ref &minor :int) minor)
    ;; possibly should return error code as well?
    (if (= (%xi-query-version display &major &minor) +status-success+)
        (values t (mem-ref &major :int) (mem-ref &minor :int))
        (values nil (mem-ref &major :int) (mem-ref &minor :int)))))

(defcfun ("XIQueryDevice" %xi-query-device) (:pointer xi-device-info)
  (display-ptr :pointer)
  (device-id :int)
  (num-devices-retuen (:pointer :int)))

(defcfun ("XISetFocus" xi-set-focus) x-status
  (display-ptr :pointer)
  (device-id :int)
  (focus window)
  (time x-time))

(defcfun ("XIGetFocus" %xi-get-focus) x-status
  (display-ptr :pointer)
  (device-id :int)
  (focus-return (:pointer window)))

(defcfun ("XIGrabDevice" %xi-grab-device) x-status
  (display-ptr :pointer)
  (device-id :int)
  (grab-window window)
  (time x-time)
  (cursor cursor)
  (grab-mode :int)
  (paired-device-mode :int)
  (owner-events :boolean)
  (mask (:pointer xi-event-mask)))

(defcfun ("XIUngrabDevice" xi-ungrab-device) x-status
  (display-ptr :pointer)
  (device-id :int)
  (time x-time))

(defcfun ("XIGrabButton" %xi-grab-button) :int
  (display-ptr :pointer)
  (device-id :int)
  (button :int)
  (grab-window window)
  (cursor cursor)
  (grab-mode :int)
  (paired-device-mode :int)
  (owner-events :int) ;; should this be boolean like XIGrabDevice?
  (mask (:pointer xi-event-mask))
  (num-modifiers :int)
  (modifiers-inout (:pointer xi-grab-modifiers)))

(defcfun ("XIGrabKeycode" %xi-grab-keycode) :int
  (display-ptr :pointer)
  (device-id :int)
  (keycode :int)
  (grab-window window)
  (grab-mode :int)
  (paired-device-mode :int)
  (owner-events :int) ;; should this be boolean like XIGrabDevice?
  (mask (:pointer xi-event-mask))
  (num-modifiers :int)
  (modifiers-inout (:pointer xi-grab-modifiers)))


(defcfun ("XIGrabEnter" %xi-grab-enter) :int
  (display-ptr :pointer)
  (device-id :int)
  (grab-window window)
  (cursor cursor)
  (grab-mode :int)
  (paired-device-mode :int)
  (owner-events :int) ;; should this be boolean like XIGrabDevice?
  (mask (:pointer xi-event-mask))
  (num-modifiers :int)
  (modifiers-inout (:pointer xi-grab-modifiers)))


(defcfun ("XIGrabFocusIn" %xi-grab-focus-in) :int
  (display-ptr :pointer)
  (device-id :int)
  (grab-window window)
  (grab-mode :int)
  (paired-device-mode :int)
  (owner-events :int) ;; should this be boolean like XIGrabDevice?
  (mask (:pointer xi-event-mask))
  (num-modifiers :int)
  (modifiers-inout (:pointer xi-grab-modifiers)))

(defcfun ("XIUngrabButton" %xi-ungrab-button) x-status
  (display-ptr :pointer)
  (device-id :int)
  (button :int)
  (grab-window window)
  (num-modifiers :int)
  (modifiers (:pointer xi-grab-modifiers)))

(defcfun ("XIUngrabKeycode" %xi-ungrab-keycode) x-status
  (display-ptr :pointer)
  (device-id :int)
  (keycode :int)
  (grab-window window)
  (num-modifiers :int)
  (modifiers (:pointer xi-grab-modifiers)))

(defcfun ("XIUngrabEnter" %xi-ungrab-enter) x-status
  (display-ptr :pointer)
  (device-id :int)
  (grab-window window)
  (num-modifiers :int)
  (modifiers (:pointer xi-grab-modifiers)))

(defcfun ("XIUngrabFocusIn" %xi-ungrab-focus-in) x-status
  (display-ptr :pointer)
  (device-id :int)
  (grab-window window)
  (num-modifiers :int)
  (modifiers (:pointer xi-grab-modifiers)))

(defcfun ("XIListProperties" %xi-list-properties) (:pointer x-atom)
  (display-ptr :pointer)
  (device-id :int)
  (num-properties-return (:pointer :int)))

(defcfun ("XIChangeProperty" %xi-change-property) :void
  (display-ptr :pointer)
  (device-id :int)
  (property x-atom)
  (type x-atom)
  (format :int)
  (mode :int)
  (data (:pointer :unsigned-char)) ;; is this a string of some sort?
  (num-items :int))

(defcfun ("XIDeleteProperty" xi-delete-property) :void
  (display-ptr :pointer)
  (device-id :int)
  (property x-atom))

(defcfun ("XIGetProperty" %xi-get-property) x-status
  (display-ptr :pointer)
  (device-id :int)
  (property x-atom)
  (offset :long)
  (length :long)
  (delete-property :boolean)
  (type x-atom)
  (type-return (:pointer x-atom))
  (format-return (:pointer :int))
  (num-items-return (:pointer :unsigned-long))
  (bytes-after-return (:pointer :unsigned-long))
  (data (:pointer (:pointer :unsigned-char))))

(defun parse-valuator-state (state)
  (with-foreign-slots ((mask-len mask values) state xi-valuator-state)
    (loop with index = 0
       for i below mask-len
       append (loop for j below 8
                 for bit = (logbitp j (mem-aref mask :unsigned-char i))
                 when bit
                 collect (list (+ j (* i 8))
                               (mem-aref values :double index))
                 and do (incf index)))))

;; process xinput 2 events
(defmethod %generic-event-dispatch ((extension-name (eql :x-input-2)) (event (eql 6)) data)
  (with-foreign-slots ((device-id source-id x y x-root y-root valuators) data xi-device-event)
    (format t "xinput2 motion event ~,3f,~,3f ~,3f,~,3fs~%" x y x-root y-root)
    (format t "device ~s/~s valuators:~s~%" source-id device-id (parse-valuator-state valuators))))

