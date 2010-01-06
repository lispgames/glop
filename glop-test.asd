;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem glop-test
  :depends-on (glop cl-opengl cl-glu)
  :components
  ((:module "test"
            :serial t
            :components
            ((:file "test")))))

