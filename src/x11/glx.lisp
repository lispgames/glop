;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;; GLX bindings
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
  (:render-type #x8011)
  (:sample-buffers 100000)
  (:samples 100001)

  (:bind-to-texture-rgb-ext #x20d0)
  (:bind-to-texture-rgba-ext #x20d1)
  (:bind-to-mipmap-texture-ext #x20d2)
  (:bind-to-texture-targets-ext #x20d3)
  (:y-inverted-ext #x20d4)

  (:drawable-type #x8010)
  (:dont-care -1))

(defbitfield (glx-attribute-flags :int)
  (:rgba-bit 1)
  (:color-index-bit 2)
  (:pixmap-bit 1)
  (:texture-1d-bit-ext #x00000001)
  (:texture-2d-bit-ext #x00000002)
  (:texture-rectangle-bit-ext #x00000004))

(defcenum (gl-enum :unsigned-int)
  (:version #x1F02))

(defcenum (glx-context-attributes :unsigned-int)
  (:major-version #x2091)
  (:minor-version #x2092)
  (:flags #x2094)
  (:profile-mask #x9126)
  (:core-profile-bit #x00000001)
  (:compatibility-profile-bit #x00000002))

(defbitfield (glx-context-attribute-flags :unsigned-int)
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

(defcenum glx-pixmap-attrib-attribs
  ;; from EXT_texture_from_pixmap
  (:texture-format-ext #x20d5)
  (:texture-target-ext #x20d6)
  (:mipmap-texture-ext #x20d7))

(defcenum glx-pixmap-attrib-values
  ;; from EXT_texture_from_pixmap
  (:texture-1d-ext #x20db)
  (:texture-2d-ext #x20dc)
  (:texture-rectangle-ext #x20dd)
  (:texture-format-none-ext #x20d8)
  (:texture-format-rgb-ext #x20d9)
  (:texture-format-rgba-ext #x20da))

(define-foreign-library opengl
  (:darwin (:framework "OpenGL"))
  (:windows "opengl32.dll" :convention :stdcall)
  (:unix (:or "libGL.so.4" "libGL.so.3" "libGL.so.2" "libGL.so.1" "libGL.so")))
(use-foreign-library opengl)

(defctype fb-config :pointer)

(defcfun ("glGetString" get-string) :pointer
  (name :unsigned-int))

(defcfun ("glXWaitGL" glx-wait-gl) :void)

(defcfun ("glXChooseVisual" %glx-choose-visual) (:pointer (:struct visual-info))
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

(defcfun ("glXGetVisualFromFBConfig" %glx-get-visual-from-fb-config)
    (:pointer (:struct visual-info))
  (display-ptr :pointer)
  (fb-config (:pointer fb-config)))

(defun glx-get-visual-from-fb-config (display-ptr fb-config)
  (let ((vis (%glx-get-visual-from-fb-config display-ptr fb-config)))
    (when (null-pointer-p vis)
      (error "Unable to create visual info from FB config"))
    vis))

(defun glx-get-fb-config-attrib (dpy fb-config attrib)
  (with-foreign-object (value :int)
    (values (%glx-get-fb-config-attrib dpy fb-config
                   (foreign-enum-value 'glx-attributes attrib) value) (mem-aref value :int))))

(defun glx-choose-fb-config (dpy screen attribs)
  ;; handle :rgba special case, yeah this is ugly...
  (let ((filtered-attribs '()))
    (loop for attr in attribs by #'cddr
       for value in (cdr attribs) by #'cddr
       do (if (eq attr :rgba)
              (progn (if value
                         (push '(:rgba-bit) filtered-attribs)
                         (push '(:color-index-bit) filtered-attribs))
                     (push :render-type filtered-attribs))
              (progn (push value filtered-attribs)
                     (push attr filtered-attribs))))
    (setf attribs filtered-attribs))
  ;; foreign attrib list
  (with-foreign-object (fb-config-count :int)
    (with-foreign-object (atts :int (1+ (length attribs)))
      (loop
        for i below (length attribs)
        for attr in attribs do
          (setf (mem-aref atts :int i)
                (cond
                  ((eq attr nil) 0)
                  ((eq attr t) 1)
                  (t (typecase attr
                       (keyword (foreign-enum-value 'glx-attributes attr))
                       (list (foreign-bitfield-value 'glx-attribute-flags attr))
                       (t attr))))))
      (setf (mem-aref atts :int (length attribs)) 0)
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
                        (glx-get-fb-config-attrib dpy (mem-aref fb-configs 'fb-config index)
                                                  :sample-buffers)
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
  (redirect :boolean))

(defun glx-create-context (dpy visual)
  (let ((ctx (%glx-create-context dpy visual (null-pointer) t)))
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
                (list (foreign-bitfield-value 'glx-context-attribute-flags attr))
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

(defcfun ("glXMakeCurrent" glx-make-current) :boolean
  (display-ptr :pointer) (drawable drawable) (context glx-context))

(defun glx-release-context (dpy)
  (glx-make-current dpy 0 (null-pointer)))

(defcfun ("glXQueryVersion" %glx-query-version) :boolean
  (display-ptr :pointer) (major :pointer) (minor :pointer))

(defun glx-get-version (dpy)
  (with-foreign-objects ((major :int) (minor :int))
    (%glx-query-version dpy major minor)
    (values (mem-ref major :int) (mem-ref minor :int))))

(defcfun ("glXSwapBuffers" glx-swap-buffers) :void
  (display-ptr :pointer) (drawable drawable))

(defcfun ("glXGetProcAddress" glx-get-proc-address) :pointer
  (proc-name :string))

(defctype glx-pixmap :int)

(defcfun ("glXCreatePixmap" %glx-create-pixmap) glx-pixmap
  (display-ptr :pointer)
  (config fb-config)
  (pixmap glop-xlib::pixmap)
  (attribs (:pointer :int)))

(defun glx-create-pixmap (display fb-config x-pixmap &rest attribs)
  (let ((l (length attribs)))
    (with-foreign-object (attr :int (+ l 2))
      (setf (mem-aref attr :int l) 0
            (mem-aref attr :int (1+ l)) 0)
      (loop for (_k _v) on attribs by #'cddr
            for k = (if (keywordp _k)
                        (foreign-enum-value 'glx-pixmap-attrib-attribs _k)
                        _k)
            for v = (if (keywordp _v)
                        (foreign-enum-value 'glx-pixmap-attrib-values _v)
                        _v)
            for i from 0 by 2
            do (setf (mem-aref attr :int i) k
                     (mem-aref attr :int (1+ i)) v))
      (%glx-create-pixmap display fb-config x-pixmap attr))))

(defcfun ("glXDestroyPixmap" glx-destroy-pixmap) :void
  (display-ptr :pointer)
  (pixmap glx-pixmap))

(defun correct-context? (major-desired minor-desired)
  (multiple-value-bind (major minor)
      (glop::parse-gl-version-string-values
       (foreign-string-to-lisp (get-string (foreign-enum-value 'gl-enum :version))))
    (when (or (< major major-desired)
              (and (= major major-desired) (< minor minor-desired)))
      (error "unable to create requested context"))))
