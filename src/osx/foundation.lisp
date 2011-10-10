(in-package #:glop-bridge)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 Types                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defctype ns-uinteger #+x86-64 :ulong #-x86-64 :uint)
(defctype ns-integer #+x86-64 :long #-x86-64 :int)
(defctype cg-float #+x86-64 :double #-x86-64 :float)

(defcstruct ns-point
  (x cg-float)
  (y cg-float))

;; (define-foreign-type ns-point-type ()
;;   ()
;;   (:actual-type ns-point-struct)
;;   (:simple-parser ns-point))

;; (defmethod translate-from-foreign (value (type ns-point-type))
;;   (with-foreign-slots ((x y) value ns-point-struct)
;;     (list x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            NSAutoreleasePool                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSAutoreleasePoolAllocInit" ns-autorelease-pool-alloc-init) :pointer)
;; release and retain are imported via CFRelease and CFRetain.
;; See Core Foundation below.
(defcfun ("NSAutorelease" ns-autorelease)
    :pointer
  (object :pointer))
(defmacro with-ns-autorelease-pool (&body body)
  (let ((pool (gensym "AUTORELEASE-POOL-")))
    `(let ((,pool (ns-autorelease-pool-alloc-init)))
       (unwind-protect (progn ,@body)
         (ns-release ,pool)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                NSArray                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSArrayCount" ns-array-count) ns-uinteger
  (ns-array :pointer))

(defcfun ("NSArrayObjectAtIndex" ns-array-object-at-index) :pointer
  (ns-array :pointer)
  (index ns-uinteger))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               NSString                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-foreign-type ns-string-type ()
  ((ns-string :initform nil
              :accessor ns-string))
  (:actual-type :pointer)
  (:simple-parser ns-string))

(defcenum ns-string-encoding
  (:ascii 1)
  (:nextstep 2)
  (:japanese-euc 3)
  (:utf-8 4)
  (:iso-latin-1 5)
  (:symbol 6)
  (:non-lossy-ascii 7)
  (:shift-jis 8)
  (:iso-latin-2 9)
  (:unicode 10)
  (:windows-cp-1251 11)
  (:windows-cp-1252 12)
  (:windows-cp-1253 13)
  (:windows-cp-1254 14)
  (:windows-cp-1250 15)
  (:iso-2022-jp 21)
  (:mac-os-roman 30)
  (:utf-16 10)
  (:utf-16-big-endian #x90000100)
  (:utf-16-little-endian #x94000100)
  (:utf-32 #x8c000100)
  (:utf-32-big-endian #x98000100)
  (:utf-32-little-endian #x9c000100)
  (:proprietary 65536))

(defcfun ("NSStringCStringUsingEncoding" ns-string-c-string-using-encoding)
    :string
  (ns-string :pointer)
  (encoding ns-string-encoding))

(defcfun ("NSStringAllocInitWithCString" ns-string-alloc-init-with-c-string)
    :pointer
  (string :string)
  (encodign ns-string-encoding))

(defmethod translate-from-foreign (ns-string (type ns-string-type))
  (ns-string-c-string-using-encoding ns-string :utf-8))

(defmethod translate-to-foreign (lisp-string (type ns-string-type))
  (let ((buffer-size (1+ (length lisp-string))))
    (with-foreign-object (buffer :char buffer-size)
      (setf (ns-string type)
            (ns-string-alloc-init-with-c-string
              (lisp-string-to-foreign lisp-string buffer buffer-size
                                      :encoding :utf-8)
              :utf-8)))))

(defmethod free-translated-object (pointer (type ns-string-type) param)
  (declare (ignore param))
  (ns-release (ns-string type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             Core Foundation                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("CFBundleGetBundleWithIdentifier"
          cf-bundle-get-bundle-with-identifier)
    :pointer
  (identifier :pointer))
(defcfun ("CFBundleGetFunctionPointerForName"
          cf-bundle-get-function-pointer-for-name)
    :pointer
  (bundle :pointer)
  (name :pointer))
(defcfun ("CFRetain" ns-retain) :pointer
  (object :pointer))
(defcfun ("CFRelease" ns-release) :pointer
  (object :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            General Functions                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcfun ("NSSelectorFromString" ns-selector-from-string) :pointer
  (string ns-string))
