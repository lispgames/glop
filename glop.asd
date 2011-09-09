;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem glop
  :license "MIT"
  :version "0.1.0"
  :description "Direct FFI bindings for OpenGL window and context management"
  :author "Morgan Veyret <patzy at oxyde dot org>"
  :depends-on (:cffi)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "utils")
             #+(and unix (not darwin))
             (:module "x11"
                      :serial t
                      :components ((:file "package")
                                   (:file "keysymdef")
                                   (:file "xlib")
                                   (:file "xkb")
                                   (:file "glx")
                                   (:file "display-ctrl")
                                   (:file "glop-x11")))
             #+darwin
             (:module "osx"
                      :serial t
                      :components ((:file "package")
                                   (:file "core-foundation")
                                   (:file "quartz")
                                   (:file "glop-osx")))
             #+(or win32 windows)
             (:module "win32"
                      :serial t
                      :components ((:file "package")
                                   (:file "win32")
                                   (:file "wgl")
                                   (:file "glop-win32")))
             (:file "glop")))))

