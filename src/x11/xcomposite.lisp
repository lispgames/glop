;;; XComposite bindings
(in-package #:glop-xlib)

(define-foreign-library xcomposite
  (:unix (:or "libXcomposite.so" "libXcomposite.so.1"))
  (t (:default "libXcomposite")))
(use-foreign-library xcomposite)

(defctype x-server-region xid)

(defcenum composite-redirect-update
  (:automatic 0)
  (:manual 1))


(defcfun ("XCompositeQueryExtension" %x-composite-query-extension) :boolean
  (display-ptr :pointer)
  (base (:pointer :int))
  (error-base (:pointer :int)))

(defun x-composite-query-extension (win)
  (cffi:with-foreign-objects ((b :int) (e :int))
    (values (%x-composite-query-extension (glop::x11-window-display win) b e)
            (mem-aref b :int)
            (mem-aref e :int))))

(defcfun ("XCompositeQueryVersion" %x-composite-query-version) x-status
  (display-ptr :pointer)
  (major-version-inout (:pointer :int))
  (minor-version-inout (:pointer :int)))

(defun x-composite-query-version (display major minor)
  (with-foreign-objects ((&major :int) (&minor :int))
    (setf (mem-ref &major :int) major
          (mem-ref &minor :int) minor)
    (let ((r (%x-composite-query-version display &major &minor)))
      (if (/= r 0)
          (values r (mem-ref &major :int) (mem-ref &minor :int))
          (values nil (mem-ref &major :int) (mem-ref &minor :int))))))

;; returns version as (+ (* major 10000) (* minor 100) revision)
;; possibly should split it up before returning it?
(defcfun ("XCompositeVersion" x-composite-version) :int)

(defcfun ("XCompositeRedirectWindow" x-composite-redirect-window) :void
  (display-ptr :pointer)
  (window window)
  (update composite-redirect-update))

(defcfun ("XCompositeRedirectSubwindows" x-composite-redirect-subwindows) :void
  (display-ptr :pointer)
  (window window)
  (update composite-redirect-update))

(defcfun ("XCompositeUnredirectWindow" x-composite-unredirect-window) :void
  (display-ptr :pointer)
  (window window)
  (update composite-redirect-update))

(defcfun ("XCompositeUnredirectSubwindows" x-composite-unredirect-subwindows)
    :void
  (display-ptr :pointer)
  (window window)
  (update composite-redirect-update))

(defcfun ("XCompositeCreateRegionFromBorderClip"
          x-composite-create-region-from-border-clip) x-server-region
  (display-ptr :pointer)
  (window window))

(defcfun ("XCompositeNameWindowPixmap" x-composite-name-window-pixmap) pixmap
  (display-ptr :pointer)
  (window window))

(defcfun ("XCompositeGetOverlayWindow" x-composite-get-overlay-window) window
  (display-ptr :pointer)
  (window window))

(defcfun ("XCompositeReleaseOverlayWindow" x-composite-release-overlay-window)
    :void
  (display-ptr :pointer)
  (window window))

