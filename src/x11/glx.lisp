;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;; GLX bindings
(defpackage :glop-glx
  (:use #:cl #:cffi #:glop-xlib)
  (:export #:glx-get-proc-address #:correct-context? #:glx-destroy-context
           #:glx-create-specific-context #:glx-create-context
           #:glx-make-current #:glx-release-context #:glx-choose-fb-config
           #:glx-get-visual-from-fb-config #:glx-choose-visual
           #:glx-wait-gl #:glx-swap-buffers))

(in-package #:glop-glx)

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

(defcfun ("glXGetConfig" glx-get-config) :int
  (display-ptr :pointer)
  (visual-info :pointer)
  (attribute :int)
  (value (:pointer :int)))

(defcfun ("glXGetFBConfigAttrib" %glx-get-fb-config-attrib) :int
  (display-ptr :pointer)
  (fb-config :pointer)
  (attribute :int)
  (value (:pointer :int)))

(defcfun ("glXGetVisualFromFBConfig" %glx-get-visual-from-fb-config) visual-info
  (display-ptr :pointer)
  (fb-config (:pointer fb-config)))

(defun glx-get-visual-from-fb-config (display-ptr fb-config)
  (let ((vis (%glx-get-visual-from-fb-config display-ptr fb-config)))
    (when (null-pointer-p vis) 
      (error "Unable to create visual info"))
    vis))

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
        (when (= (mem-aref fb-config-count :int) 0)
          (error "Unable to find any suitable frame buffer configs"))
        (loop
          for index below (mem-ref fb-config-count :int)
          with vi = (null-pointer)
          with best-samples = -1
          with cur-samples = -1
          with best-fbc = 0  do
            (setf vi (glx-get-visual-from-fb-config dpy (mem-aref fb-configs 'fb-config index)))
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
  ;; remove value for boolean attributes
  (let ((filtered-attribs '()))
    (loop for attr in attribs by #'cddr
         for value in (cdr attribs) by #'cddr
         do (cond ((eq value t) (push attr filtered-attribs))
                  ((eq value nil) t)
                  (t (push value filtered-attribs)
                     (push attr filtered-attribs))))
    (setf attribs filtered-attribs))
  ;; create the foreign attribs list
  (with-foreign-object (atts :int (1+ (length attribs)))
    (loop for i below (length attribs)
         for attr = (nth i attribs)
       do (setf (mem-aref atts :int i)
                (typecase attr
                  (keyword (foreign-enum-value 'glx-attributes attr))
                  (t attr))))
    (setf (mem-aref atts :int (length attribs)) 0)
    (let ((vis (%glx-choose-visual dpy screen atts)))
      (when (null-pointer-p vis) 
        (error "Unable to create visual info"))
      vis)))

(defctype glx-context :pointer)

(defcfun ("glXCreateContext" %glx-create-context) glx-context
  (display-ptr :pointer) (visual-infos :pointer) (share-list glx-context)
  (redirect bool))

(defun glx-create-context (dpy visual)
  (let ((ctx (%glx-create-context dpy visual (null-pointer) 1)))
    (when (null-pointer-p ctx) 
      (error "Unable to create context"))
    ctx))

(defun glx-create-specific-context (dpy fbc context-attribs)
  (with-foreign-object ( atts :int (1+ (length context-attribs)))
    (loop
      for i below (length context-attribs)
      for attr in context-attribs do
        (setf (mem-aref atts :int i)
              (typecase attr
                (keyword (foreign-enum-value 'glx-context-attributes attr))
                (t attr))))
    (setf (mem-aref atts :int (length context-attribs)) 0)
    (let ((ptr (glx-get-proc-address "glXCreateContextAttribsARB")))
      (when (null-pointer-p ptr)
        (error "glXCreateContextAttribsARB unavailable"))
      (let ((ctx (cffi:foreign-funcall-pointer ptr ()
                                               :pointer dpy
                                               :pointer fbc
                                               :pointer (null-pointer)
                                               :int 1
                                               (:pointer :int) atts
                                               :pointer)))
        (when (null-pointer-p ctx) 
          (error "Unable to create context"))
        ctx))))

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

(defun correct-context? (major-desired minor-desired)
  (multiple-value-bind (major minor)
      (parse-gl-version-string-values (foreign-string-to-lisp (get-string (foreign-enum-value 'gl-enum :version))))
    (when (or (< major major-desired)
              (and (= major major-desired) (< minor minor-desired)))
      (error "unable to create requested context"))))
