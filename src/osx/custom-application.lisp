(in-package #:glop-bridge)

(defcfun ("CustomApplicationSharedApplication" custom-app-shared-application)
    :pointer)
(defcfun ("CustomApplicationRunIteration" custom-app-run-iteration) :void
  (app :pointer))
(defcfun ("CustomApplicationRun" custom-app-run) :void
  (app :pointer))