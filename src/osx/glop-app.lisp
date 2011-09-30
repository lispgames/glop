(in-package #:glop-bridge)

(defcfun ("GlopAppSharedApplication" glop-app-shared-application) :pointer)
(defcfun ("GlopAppNextEvent" glop-app-next-event) :pointer
  (app :pointer))
(defcfun ("GlopAppSendEvent" glop-app-send-event) :void
  (app :pointer))
(defcfun ("GlopAppUpdateWindows" glop-app-update-windows) :void
  (app :pointer))
(defcfun ("GlopAppRun" glop-app-run) :void
  (app :pointer))