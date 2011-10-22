;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; WGL bindings

(in-package #:glop-wgl)

(defcstruct pixelformatdescriptor
  (size :int16)
  (version :int16)
  (flags :int32)
  (pixel-type :int8)
  (color-bits :int8)
  (red-bits :int8)
  (red-shift :int8)
  (green-bits :int8)
  (green-shift :int8)
  (blue-bits :int8)
  (blue-shift :int8)
  (alpha-bits :int8)
  (alpha-shift :int8)
  (accum-bits :int8)
  (accum-red-bits :int8)
  (accum-green-bits :int8)
  (accum-blue-bits :int8)
  (accum-alpha-bits :int8)
  (depth-bits :int8)
  (stencil-bits :int8)
  (aux-buffers :int8)
  (layer-type :int8)
  (reserved :int8)
  (layer-mask :int32)
  (visible-mask :int32)
  (damage-mask :int32))

(defbitfield (pfd-flags :int32)
  (:pfd-draw-to-window 4)
  (:pfd-draw-to-bitmap 8)
  (:pfd-support-gdi 16)
  (:pfd-support-opengl 32)
  (:pfd-generic-accelerated #x00001000)
  (:pfd-generic-format 64)
  (:pfd-need-palette 128)
  (:pfd-need-system-palette #x00000100)
  (:pfd-double-buffer 1)
  (:pfd-stereo 2)
  (:pfd-swap-layer-buffers  #x00000800)
  (:pfd-depth-dont-care     #x20000000)
  (:pfd-double-buffer-dont-care #x40000000)
  (:pfd-stereo-dont-care #x80000000)
  (:pfd-swap-copy #x00000400)
  (:pfd-swap-exchange #x00000200))

(defcenum pfd-pixel-type
  (:pfd-type-rgba 0)
  (:pfd-type-color-index 1))

(define-foreign-library opengl
  (t (:default "opengl32")))
(use-foreign-library opengl)

(defctype hglrc handle)

(defcfun ("wglCreateContext" wgl-create-context) hglrc
  (dc hdc))

(defcfun ("wglMakeCurrent" wgl-make-current) bool
  (dc hdc) (rc hglrc))

(defcfun ("wglDeleteContext" wgl-delete-context) bool
  (rc hglrc))

(defcfun ("wglGetProcAddress" wgl-get-proc-address) :pointer
  (proc-name :string))


;; Those are not really wgl funcs but more related to GL than win32
(define-foreign-library gdi32
    (t (:default "gdi32")))
(use-foreign-library gdi32)

(defcfun ("ChoosePixelFormat" %choose-pixel-format) :int
  (dc hdc) (pfd :pointer))

(defcfun ("SetPixelFormat" %set-pixel-format) bool
  (dc hdc) (pixel-format :int) (pfd :pointer))

(defun choose-pixel-format (dc &key (rgba t)
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
  (declare (ignore stencil-buffer))
  (with-foreign-object (pfd 'pixelformatdescriptor)
    (with-foreign-slots ((size version flags pixel-type color-bits
                               red-bits green-bits blue-bits alpha-bits
                               accum-bits accum-red-bits accum-green-bits accum-blue-bits
                               stencil-bits
                               depth-bits) pfd pixelformatdescriptor)
      (setf size (foreign-type-size 'pixelformatdescriptor)
            version 1
            flags (foreign-bitfield-value 'pfd-flags
                       (list :pfd-draw-to-window :pfd-support-opengl
                             (if double-buffer
                                 :pfd-double-buffer
                                 :pfd-double-buffer-dont-care)
                             ;; FIXME: there's a problem with :pfd-stereo-dont-care
                             ;; (if stereo
                             ;;     :pfd-stereo
                             ;;     :pfd-stereo-dont-care)
                             ))
            pixel-type (foreign-enum-value 'pfd-pixel-type
                                           (if rgba :pfd-type-rgba :pfd-type-color-index))
            color-bits 32 ;; we want proper RGBA but not sure to understand this struct field
            red-bits red-size
            green-bits green-size
            blue-bits blue-size
            alpha-bits alpha-size
            accum-bits (if accum-buffer
                           (+ accum-red-size accum-green-size accum-blue-size)
                           0)
            accum-red-bits accum-red-size
            accum-green-bits accum-green-size
            accum-blue-bits accum-blue-size
            depth-bits depth-size
            stencil-bits stencil-size))
    (let ((fmt (%choose-pixel-format dc pfd)))
      (%set-pixel-format dc fmt pfd)
      fmt)))

(defcfun ("SwapBuffers" swap-buffers) bool
  (dc hdc))
