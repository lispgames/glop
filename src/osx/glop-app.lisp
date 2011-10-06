(in-package #:glop-bridge)

(defcvar ("NSApp" *ns-app* :read-only t) :pointer)
(defcfun ("GlopAppSharedApplication" glop-app-shared-application) :pointer)
(defcfun ("GlopAppSetMainMenu" glop-app-set-main-menu) :pointer
  (app :pointer)
  (menu :pointer))
(defcfun ("GlopAppNextEvent" glop-app-next-event) :pointer
  (app :pointer)
  (blocking :boolean))
(defcfun ("GlopAppSendEvent" glop-app-send-event) :void
  (app :pointer)
  (event :pointer))
(defcfun ("GlopAppUpdateWindows" %glop-app-update-windows) :void
  (app :pointer))
(defun glop-app-update-windows (&optional (app *ns-app*))
  (%glop-app-update-windows app))
(defcfun ("GlopAppRun" %glop-app-run) :void
  (app :pointer))
(defun glop-app-run (&optional (app *ns-app*))
  (%glop-app-run app))
