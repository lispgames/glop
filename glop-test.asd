;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem glop-test
  :license "MIT"
  :version "0.1.0"
  :description "Direct FFI bindings for OpenGL window and context management tests"
  :author "Morgan Veyret <patzy at oxyde dot org>"
  :depends-on (glop cl-opengl cl-glu)
  :components
  ((:module "test"
            :serial t
            :components
            ((:file "test")))))

