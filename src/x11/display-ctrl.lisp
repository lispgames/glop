(in-package #:glop-xlib)

;; Xrandx bindings
(define-foreign-library xrandr
  (t (:default "libXrandr")))
(use-foreign-library xrandr)

(defcstruct xrr-screen-size
  (width :int)
  (height :int)
  (mwidth :int)
  (mheight :int))

(defctype screen-size xrr-screen-size)

(defcenum (rr-rotation)
  (:rotate-0 1)
  (:rotate-90 2)
  (:rotate-180 4)
  (:rotate-270 8))

(defcfun ("XRRSetScreenConfigAndRate" xrr-set-screen-config-and-rate) :int
  (display-ptr :pointer)
  (config :pointer)
  (root-window :int)
  (size-index :int)
  (rotation :int)
  (frequencies :short)
  (time :long))

(defcfun ("XRRSetScreenConfig" xrr-set-screen-config) :int
  (display-ptr :pointer)
  (config :pointer)
  (root-window :int)
  (size-index :int)
  (rotation :int)
  (time :long))

(defcfun ("XRRGetScreenInfo" xrr-get-screen-info) :pointer
  (display-ptr :pointer)
  (root-window :int))

(defcfun ("XRRFreeScreenConfigInfo" xrr-free-screen-config-info) :int
  (screen-info :pointer))

(defcfun ("XRRConfigSizes" xrr-config-sizes) :pointer
  (screen-info :pointer)
  (count (:pointer :int)))

(defcfun ("XRRRates" xrr-rates) :pointer
  (display-ptr :pointer)
  (screen :int)
  (size-index :int)
  (nrates :pointer))

(defcfun ("XRRConfigCurrentRate" xrr-config-current-rate) :short
  (config :pointer))

(defcfun ("XRRConfigCurrentConfiguration" xrr-config-current-configuration) :int
  (config :pointer)
  (rotation :pointer))

(defun current-mode (dpy screen)
  (with-foreign-object (rot :int)
    (let* ((sc (xrr-get-screen-info dpy (root-window dpy screen)))
           (rate (xrr-config-current-rate sc))
           (index (xrr-config-current-configuration sc rot)))
      (xrr-free-screen-config-info sc)
      (values (display-width dpy screen) (display-height dpy screen)
              (default-depth dpy screen) rate index))))

(defun set-mode (dpy screen mode-index &optional rate)
  (let* ((root (root-window dpy screen))
         (sc (xrr-get-screen-info dpy root)))
    (if rate
        (xrr-set-screen-config-and-rate dpy sc root mode-index
                               (foreign-enum-value 'rr-rotation :rotate-0) rate 0)
        (xrr-set-screen-config dpy sc root mode-index
                           (foreign-enum-value 'rr-rotation :rotate-0) 0))
    (xrr-free-screen-config-info sc)))

(defun supported-modes (dpy screen)
  (with-foreign-objects ((count :int) (gl :int) (rgba :int) (dummy 'visual-info))
    (let ((rtn-list (get-visual-info dpy 0 dummy count))
          (sc (xrr-get-screen-info dpy (root-window dpy screen)))
          (depth-list nil)
          (resolution-list nil))
      (unless (null-pointer-p rtn-list)
        ;; available depths
        (do
            ((index 0 (1+ index))
             (vi rtn-list (inc-pointer vi (foreign-type-size 'visual-info))))
            ((>= index (mem-aref count :int)))
          (glop-glx::glx-get-config dpy vi
                                    (foreign-enum-value 'glop-glx::glx-attributes :use-gl) gl)
          (when (and (= 1 (mem-aref gl :int))
                        (not (member (foreign-slot-value vi 'visual-info 'depth) depth-list)))
            (push (foreign-slot-value vi 'visual-info 'depth) depth-list)))
        ;; available resolutions
        (x-free rtn-list)
        (setf rtn-list (xrr-config-sizes sc count))
        (xrr-free-screen-config-info sc)
        (do
            ((index 0 (1+ index))
             (sizes rtn-list (inc-pointer sizes (foreign-type-size 'xrr-screen-size))))
            ((>= index (mem-aref count :int)))
          (with-foreign-slots ((width height) sizes screen-size)
            ;; all rates for this mode
            (with-foreign-object (nrates :short)
              (let ((rates (xrr-rates dpy screen index nrates)))
                (loop for n below (mem-ref nrates :short)
                  do (push (list width height (mem-aref rates :short n) index) resolution-list))))))
        (values depth-list resolution-list)))))
