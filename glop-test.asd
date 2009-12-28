;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem glop-test
  :depends-on (glop cl-opengl cl-glu)
  :components
  ((:module "test"
            :serial t
            :components
            ((:file "test")))))

