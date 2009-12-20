;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem glop-test
  :depends-on (glop cl-opengl cl-glu)
  :components
  ((:file "test")))

