(in-package #:glop-bridge)

(pushnew (asdf:system-relative-pathname :glop "src/osx/bridge/")
         cffi:*foreign-library-directories* :test #'equal)
(define-foreign-library bridge
  (t (:default "glop-bridge")))
(use-foreign-library bridge)
