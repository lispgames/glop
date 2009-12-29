;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem glop
  :license "MIT"
  :version "git"
  :description "Direct FFI bindings for OpenGL window and context management"
  :author "Morgan Veyret <patzy at appart kicks-ass net>"
  :depends-on (cffi)
  :components
  ((:module "src"
           :serial t
           :components
           ((:file "package")
            (:file "utils")
            #+unix(:module "x11"
                           :serial t
                           :components ((:file "xlib")
                                        (:file "glx")
                                        (:file "display-ctrl")
                                        (:file "glop-x11")))
            #+win32(:module "win32"
                            :serial t
                            :components ((:file "win32")
                                         (:file "wgl")
                                         (:file "glop-win32")))
            (:file "glop")))))

