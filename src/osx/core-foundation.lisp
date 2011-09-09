(in-package #:glop-core-foundation)

(define-foreign-library application-services
  (t (:framework "CoreFoundation")))
(use-foreign-library application-services)

; Enums:
(defcenum string-builtin-encodings
  (:mac-roman 0)
  (:windows-latin-1 #x0500)
  (:iso-latin-1 #x0201)
  (:next-step-latin #x0b01)
  (:ascii #x0600)
  (:unicode #x0100)
  (:utf-8 #x08000100)
  (:non-lossy-ascii #x0bff)
  (:utf-16 #x0100)
  (:utf-16-be #x10000100)
  (:utf-16-le #x14000100)
  (:utf-32 #x0c000100)
  (:utf-32-be #x18000100)
  (:utf-32-le #x1c000100))

; Types:
(defctype index :int64)
(defcstruct range
  (location index)
  (length index))

; Strings:
(defcfun ("CFStringGetLength" string-length) index
  (the-string :pointer))
(defcfun ("CFStringGetCString" string-c-string) :boolean
  (the-string :pointer)
  (buffer :pointer)
  (buffer-length index)
  (encoding string-builtin-encodings))

; Arrays:
(defcfun ("CFArrayGetCount" array-count) index
  (the-array :pointer))
(defcfun ("CFArrayGetValueAtIndex" array-value-at-index) :pointer
  (the-array :pointer)
  (index index))
(defcfun ("CFArrayGetFirstIndexOfValue" array-first-index-of-value) index
  (the-array :pointer)
  (range range)
  (value :pointer))
(defcfun ("CFArrayGetValues" array-values) :void
  (the-array :pointer)
  (range range)
  (values :pointer))

(defun string-lisp-string (cf-string)
  (let ((buffer-length (1+ (string-length cf-string))))
    (with-foreign-object (buffer :string buffer-length)
      (string-c-string cf-string buffer buffer-length :iso-latin-1)
      (foreign-string-to-lisp buffer))))

; There's a problem with CFArrayRefs that causes the following to fail:
(defun lisp-array-values (cf-array)
  (let ((buffer-length (array-count cf-array)))
    (with-foreign-object (buffer :pointer buffer-length)
      (with-foreign-object (range 'range)
        (with-foreign-slots ((location length) range range)
          (setf location 0
                length buffer-length))
        (array-values cf-array range buffer)
        (loop with lisp-array = (make-array buffer-length
                                            :element-type
                                            (type-of (null-pointer)))
              for i below buffer-length
              do (setf (aref lisp-array i) (mem-aref buffer :pointer i))
              finally (return lisp-array))))))