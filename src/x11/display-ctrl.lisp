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

(defun get-current-display-mode (dpy screen)
  (values (display-width dpy screen) (display-height dpy screen) 
	  (default-depth dpy screen)))

(defun set-video-mode (dpy screen mode rate)
  (declare (ignore rate))
  (let* ((root (root-window dpy screen))
	 (sc (xrr-get-screen-info dpy root)))
    (xrr-set-screen-config dpy sc root mode 
			   (foreign-enum-value 'rr-rotation :rotate-0) 0)
    (xrr-free-screen-config-info sc)))

(defun get-closest-video-mode (dpy screen desired-width desired-height rate)
  (declare (ignore rate))
  (let ((sc (xrr-get-screen-info dpy (root-window dpy screen)))
	(size-list (null-pointer))
	(best-size -1)
	(best-match most-positive-fixnum))
    (with-foreign-object (size-count :int)
      (setf size-list (xrr-config-sizes sc size-count))
      (xrr-free-screen-config-info sc)
      (do
	  ((index 0 (1+ index))
	   (size size-list (inc-pointer size (foreign-type-size 'xrr-screen-size))))
	  ((>= index (mem-aref size-count :int)))
	(with-foreign-slots ((width height) size screen-size)
	  (let ((match (+ (* (- desired-width width) (- desired-width width))
			  (* (- desired-height height) (- desired-height height)))))
	    (when (< match best-match)
	      (setf best-match match)
	      (setf best-size index))))))
    (if (>= best-size 0)
	(with-foreign-slots 
	    ((width height) (inc-pointer size-list 
					 (* best-size (foreign-type-size 'xrr-screen-size)))
	     screen-size)
	  (values best-size width height))
	(values 0 (display-width dpy screen) (display-height dpy screen)))))

(defun get-available-display-modes (dpy screen)
  (with-foreign-objects ((count :int) (gl :int) (rgba :int) (dummy 'visual-info))
    (let ((rtn-list (get-visual-info dpy 0 dummy count))
	  (sc (xrr-get-screen-info dpy (root-window dpy screen)))
	  (depth-list nil)
	  (resolution-list nil))
      (unless (null-pointer-p rtn-list)
	(do
	    ((index 0 (1+ index))
	     (vi rtn-list (inc-pointer vi (foreign-type-size 'visual-info))))
	    ((>= index (mem-aref count :int)))
	  (glop-glx::glx-get-config dpy vi 
				    (foreign-enum-value 'glop-glx::glx-attributes :use-gl) gl)
	  (glop-glx::glx-get-config dpy vi 
				    (foreign-enum-value 'glop-glx::glx-attributes :rgba) rgba)
	  (when (and (= 1 (mem-aref gl :int) (mem-aref rgba :int))
			(not (member (foreign-slot-value vi 'visual-info 'depth) depth-list)))
	    (push (foreign-slot-value vi 'visual-info 'depth) depth-list)))
	(x-free rtn-list)
	(setf rtn-list (xrr-config-sizes sc count))
	(xrr-free-screen-config-info sc)
	(do
	    ((index 0 (1+ index))
	     (sizes rtn-list (inc-pointer sizes (foreign-type-size 'xrr-screen-size))))
	    ((>= index (mem-aref count :int)))
	  (with-foreign-slots ((width height) sizes screen-size)
	    (push `(,width ,height) resolution-list)))
	(values depth-list resolution-list)))))