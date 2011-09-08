(in-package #:glop-osx)

(pushnew (asdf:system-relative-pathname :glop "src/osx/bridge/bridge.dylib")
         *foreign-library-directories* :test #'equal)
(define-foreign-library bridge
  (t "bridge.dylib"))
(use-foreign-library bridge)

; NSRect:
(defctype cg-float #+x86-64 :double #-x86-64 :float)
(defcstruct ns-point
  (x cg-float)
  (y cg-float))
(defcstruct ns-size
  (width cg-float)
  (height cg-float))
(defcstruct ns-rect
  (origin ns-point)
  (size ns-size))

; Bridge:
(defcstruct display-mode-info
  (width :int)
  (height :int)
  (refresh :double)
  (depth :int))
(defcfun ("getDisplayModeInfoArray" get-display-mode-info-array) :pointer
  (size :pointer))
(defcfun ("makeAutoreleasePool" make-autorelease-pool) :pointer)
(defcfun ("releasePool" release-pool) :void
  (pool :pointer))
(defcfun ("initNSApp" init-ns-app) :void)
(defcfun ("openWindow" open-window) :pointer
  (x :int) (y :int) (width :int) (height :int))

(defmacro with-autorelease-pool (&body body)
  (let ((pool-var (gensym "AUTORELEASE-POOL-")))
   `(let ((,pool-var (make-autorelease-pool)))
      (unwind-protect (progn ,@body)
        (release-pool ,pool-var)))))

(defun list-video-modes ()
  (with-foreign-object (array-size :long)
    (let ((modes (get-display-mode-info-array array-size)))
      (unwind-protect
           (loop for i below (mem-ref array-size :long)
                 collect (with-foreign-slots
                             ((width height refresh depth)
                              (mem-aref modes 'display-mode-info i)
                              display-mode-info)
                           (glop::make-osx-video-mode
                             :width width
                             :height height
                             :rate (truncate refresh)
                             :depth depth)))
        (foreign-free modes)))))
