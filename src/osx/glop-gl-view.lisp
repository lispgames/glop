(in-package :glop-bridge)

(defcfun ("GlopGLViewInit" glop-gl-view-init) :pointer
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (pixel-format :pointer))

(defcfun ("GlopGLViewOpenGLContext" glop-gl-view-opengl-context) :pointer
  (view :pointer))

(defcfun ("GlopGLViewSetOpenGLContext" glop-gl-view-set-opengl-context) :void
  (view :pointer)
  (context :pointer))

(defcfun ("GlopGLViewClearGLContext" glop-gl-view-clear-gl-context) :void
  (view :pointer))

(defcfun ("GlopGLViewSetNextResponder" glop-gl-view-set-next-responder) :void
  (view :pointer)
  (responder :pointer))