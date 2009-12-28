(in-package #:glop-x11)

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

(defcfun ("XFlush" x-flush) :int
  (display-ptr :pointer))

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

(defcfun ("XSync" x-sync) :int
  (display-ptr :pointer) (discard bool))

(defcfun ("XNextEvent" %x-next-event) :int
  (display-ptr :pointer) (evt x-event))

(defcfun ("XPending" %x-pending) :int
  (display-ptr :pointer))

(defun x-pending-p (display-ptr)
  (not (zerop (%x-pending display-ptr))))

(defun x-next-event (dpy &optional blocking)
  (x-sync dpy 0)
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
           (glop::make-event :type :key-press
                             :key (x-lookup-key evt)))
        (:key-release
           (glop::make-event :type :key-release
                             :key (x-lookup-key evt)))
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
             (declare (ignorable x y root border-width depth))
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
        (t (format t "Unhandled X11 event: ~S~%" type))))))

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

(defctype fb-config :pointer)

(defcfun ("glGetString" get-string) :pointer
  (name :unsigned-int))

(defcfun ("glXWaitGL" glx-wait-gl) :void)

(defcfun ("glXChooseVisual" %glx-choose-visual) visual-info
  (display-ptr :pointer) (screen :int) (attribs :pointer))

(defcfun ("glXChooseFBConfig" %glx-choose-fb-config) (:pointer fb-config)
  (display-ptr :pointer)
  (screen :int)
  (attrib_list (:pointer :int))
  (nelements (:pointer :int)))

(defcfun ("glXGetFBConfigAttrib" %glx-get-fb-config-attrib) :int
  (display-ptr :pointer)
  (fb-config :pointer)
  (attribute :int)
  (value (:pointer :int)))

(defcfun ("glXGetVisualFromFBConfig" %glx-get-visual-from-fb-config) visual-info
  (display-ptr :pointer)
  (fb-config (:pointer fb-config)))

(defun glx-get-fb-config-attrib (dpy fb-config attrib)
  (with-foreign-object (value :int)
    (values (%glx-get-fb-config-attrib dpy fb-config (foreign-enum-value 'glx-attributes attrib) value) (mem-aref value :int))))

(defun glx-choose-fb-config (dpy screen attribs-list)
  (with-foreign-object (fb-config-count :int)
    (with-foreign-object (atts :int (1+ (length attribs-list)))
      (loop 
	for i below (length attribs-list)
	for attr in attribs-list do  
	  (setf (mem-aref atts :int i)
		(cond 
		  ((eq attr nil) 0)
		  ((eq attr t) 1)
		  (t (typecase attr
		       (keyword (foreign-enum-value 'glx-attributes attr))
		       (t attr))))))
      (setf (mem-aref atts :int (length attribs-list)) 0)
      (let ((fb-configs (%glx-choose-fb-config dpy screen atts fb-config-count)))
	(loop
	  for index below (mem-ref fb-config-count :int)
	  with vi = (null-pointer)
	  with best-samples = -1
	  with cur-samples = -1
	  with best-fbc = 0  do
	    (setf vi (%glx-get-visual-from-fb-config dpy (mem-aref fb-configs 'fb-config index)))
	    (unless (null-pointer-p vi)
	      (setf cur-samples 
		    (multiple-value-bind (rtn value) 
			(glx-get-fb-config-attrib dpy (mem-aref fb-configs 'fb-config index) :sample-buffers)
		      (declare (ignore rtn)) value))
	      (when (> cur-samples best-samples)
		(setf best-samples cur-samples)
		(setf best-fbc index)))
	    (x-free vi) 
	  finally (return (mem-aref fb-configs 'fb-config best-fbc)))))))

(defun glx-choose-visual (dpy screen attribs)
  (with-foreign-object (atts :int (1+ (length attribs)))
    (loop for i below (length attribs)
         for attr = (nth i attribs)
       do (unless (or (eq attr nil) (eq attr t))
		      (setf (mem-aref atts :int i)
			    (typecase attr
			      (keyword (foreign-enum-value 'glx-attributes attr))
			      (t attr)))))
    (setf (mem-aref atts :int (length attribs)) 0)
    (%glx-choose-visual dpy screen atts)))

(defctype glx-context :pointer)

(defcfun ("glXCreateContext" %glx-create-context) glx-context
  (display-ptr :pointer) (visual-infos :pointer) (share-list glx-context)
  (redirect bool))

(defun glx-create-context (dpy visual)
  (%glx-create-context dpy visual (null-pointer) 1))

(defmethod glx-create-specific-context (dpy fbc context-attribs)
  (with-foreign-object ( atts :int (1+ (length context-attribs)))
    (loop
      for i below (length context-attribs)
      for attr in context-attribs do
	(setf (mem-aref atts :int i)
	      (typecase attr
		(keyword (foreign-enum-value 'glx-context-attributes attr))
		(t attr))))
    (setf (mem-aref atts :int (length context-attribs)) 0)
    (let ((ptr (glop-x11::glx-get-proc-address "glXCreateContextAttribsARB")))      
      (when (null-pointer-p ptr)
	(error "glXCreateContextAttribsARB unavailable"))
      (cffi:foreign-funcall-pointer ptr ()
				    :pointer dpy 
				    :pointer fbc
				    :pointer (null-pointer) 
				    :int 1 
				    (:pointer :int) atts
				    :pointer))))

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

(defcfun ("glXGetProcAddress" glx-get-proc-address) :pointer
  (proc-name :string))

(defun parse-gl-version-string-values (string)
  ;; major version is integer value up to first #\.
  ;; minor version is integer from first #\. to a #\. or #\space
  (let ((dot (position #\. string)))
    (values
     (values (parse-integer string :end dot :junk-allowed t)) ; major
     (if dot ; minor
         (values (parse-integer string :start (1+ dot) :junk-allowed t))
         0))))

(defun correct-context? (major minor)
  (multiple-value-bind (maj min) 
      (parse-gl-version-string-values (foreign-string-to-lisp (get-string (foreign-enum-value 'gl-enum :version))))
    (unless (and (>= maj major) (>= min minor))
      (error "unable to create requested context"))))

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

(setf gl-get-proc-address #'glop-x11::glx-get-proc-address)

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

(defstruct (x11-window (:include window))
  display      ;; X display ptr
  screen       ;; X screen number
  id           ;; X window ID
  visual-infos ;; X visual format of the window
  fb-config ;; X framebuffer config
)

(defstruct glx-context
  ctx           ;; GL context ptr
  display       ;; X display ptr
)

(defmethod create-gl-context ((win x11-window) &key (make-current t) major minor)
    (let ((ctx (make-glx-context
                :ctx (if (and major minor)
			 (glop-x11::glx-create-specific-context (x11-window-display win) 
								(x11-window-fb-config win)
								`(:major-version ,major :minor-version ,minor))
                         (glop-x11::glx-create-context (x11-window-display win)
                                                       (x11-window-visual-infos win)))
                :display (x11-window-display win))))
      (when make-current
        (attach-gl-context win ctx))
      (when (and major minor)
	(glop-x11::correct-context? major minor))
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
  (declare (ignorable stencil-buffer stencil-size))
  (without-fp-traps
    (format t "opening display~%")
    (let ((win (make-x11-window :display (glop-x11::x-open-display "")
                                :screen 0)))
      (format t "Window structure: ~S~%" win)
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
	  (push :accum-red-size attribs)
	  (push accum-red-size attribs)
	  (push :accum-green-size attribs)
	  (push accum-green-size attribs)
	  (push :accum-blue-size attribs)
	  (push accum-blue-size attribs))
	;; if major *and* minor are specified use fb config code path
	;; otherwise just use old style visual selection and context creation
	(format t "Creating fb-config and visuals~%")
	(if (and major minor)
	    ;;create fb-config and visual
	    (progn
	      (setf (x11-window-fb-config win) (glop-x11::glx-choose-fb-config (x11-window-display win) (x11-window-screen win) attribs))
	      (setf (x11-window-visual-infos win) (glop-x11::%glx-get-visual-from-fb-config (x11-window-display win) (x11-window-fb-config win))))
	    ;; create old style visual
            (setf (x11-window-visual-infos win)
                  (glop-x11::glx-choose-visual (x11-window-display win)
                                               (x11-window-screen win)
                                               attribs))))
      (format t "Creating window ~%Window structure: ~S~%" win)
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
      (format t "Creating Contexts ~%Window structure: ~S~%" win)
      ;; create a GL context and make it current same as for the visual regarding to major/minor
      ;; values
      (setf (window-gl-context win) (create-gl-context win :major major :minor minor
                                                           :make-current t))
      ;; show created window
      (show-window win)
      (glop-x11::x-flush (x11-window-display win))
      ;; return created window
      win)))

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
  (glop-x11::glx-wait-gl)
  (glop-x11::glx-swap-buffers (x11-window-display win) (x11-window-id win)))

(defmethod next-event ((win x11-window) &key blocking)
  (glop-x11::x-next-event (x11-window-display win) blocking))

