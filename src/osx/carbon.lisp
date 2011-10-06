(in-package #:glop-bridge)

(defcenum process-application-transform-state
  (:transform-to-foreground-application 1))

(defcenum process-id-constant
  :no-process
  :system-process
  :current-process)

(defcstruct process-serial-number
  (high :ulong)
  (low :ulong))

(defcfun ("TransformProcessType" transform-process-type) :int32
  (process-serial-number :pointer)
  (transform-state process-application-transform-state))

(defcfun ("SetFrontProcess" set-front-process) :int16
  (process-serial-number :pointer))

(defun transform-current-process-type (transformation)
  (with-foreign-object (psn 'process-serial-number)
    (with-foreign-slots ((high low) psn process-serial-number)
      (setf high 0
            low (foreign-enum-value 'process-id-constant :current-process)))
    (transform-process-type psn transformation)))

(defun set-front-current-process ()
  (with-foreign-object (psn 'process-serial-number)
    (with-foreign-slots ((high low) psn process-serial-number)
      (setf high 0
            low (foreign-enum-value 'process-id-constant :current-process)))
    (set-front-process psn)))