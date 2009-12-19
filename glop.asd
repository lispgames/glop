;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem glop
  :depends-on (cffi)
  :serial t
  :components
  ((:file "package")
   (:file "glop")
   #+unix(:file "x11")
   #+win32(:file "win32")))

