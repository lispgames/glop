;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; Win32  bindings
(in-package #:glop-win32)

(defvar *window-id-mapping* (tg:make-weak-hash-table :weakness :value))

;; only on windows 32 bit
(defctype wparam :int32)
(defctype lparam :int32)

(defctype word :int16)
(defctype dword	 :int32)

(defctype bool :int) ;; XXX: Win32 BOOL isn't used as a boolean (e.g.: see GetMessage)

(defctype handle :pointer)
(defctype hwnd handle)
(defctype hdc handle)
(defctype hmenu handle)
(defctype hmodule handle)
(defctype hinstance handle)
(defctype hicon handle)
(defctype hcursor handle)
(defctype hbrush handle)

(defcstruct point
  (x :long)
  (y :long))

(defcstruct msg
  (h-wnd hwnd)
  (message :unsigned-int)
  (w-param wparam)
  (l-param lparam)
  (time dword)
  (pt point))

(defbitfield wex-style
  (:ws-ex-topmost #x0000008)
  (:ws-ex-app-window #x40000)
  (:ws-ex-window-edge 256))

(defbitfield (wstyle :unsigned-int)
  (:ws-overlapped #x00000000)
  (:ws-popup #x80000000)
  (:ws-child #x40000000)
  (:ws-minimize #x20000000)
  (:ws-visible #x10000000)
  (:ws-disabled #x08000000)
  (:ws-clip-siblings #x04000000)
  (:ws-clip-children #x02000000)
  (:ws-maximize #x01000000)
  (:ws-caption #x00c00000)
  (:ws-border #x00800000)
  (:ws-dialog-frame #x00400000)
  (:ws-vscroll #x00200000)
  (:ws-hscroll #x00100000)
  (:ws-sys-menu #x00080000)
  (:ws-thick-frame #x00040000)
  (:ws-group #x00020000)
  (:ws-tabstop #x00010000)
  (:ws-minimize-box #x00020000)
  (:ws-maximize-box #x00010000)
  (:ws-tiled  #x00000000)
  (:ws-iconic  #x20000000)
  (:ws-sizebox #x00040000)

  (:ws-overlapped-window #xcf0000))

(defcenum gwl-index
  (:gwl-ex-style -20)
  (:gwl-style -16))

(defbitfield class-style-flags
  (:cs-byte-align-client 4096)
  (:cs-byte-align-window 8192)
  (:cs-key-cvt-window 4)
  (:cs-no-key-cvt 256)
  (:cs-class-dc 64)
  (:cs-dbl-clks 8)
  (:cs-global-class 16384)
  (:cs-hredraw 2)
  (:cs-no-close 512)
  (:cs-own-dc 32)
  (:cs-parent-dc 128)
  (:cs-save-bits 2048)
  (:cs-vredraw 1)
  (:cs-ime #x10000)
  (:cs-drop-shadow #x20000))

(defcstruct wndclass
  (style class-style-flags)
  (wndproc :pointer)
  (cls-extra :int)
  (wnd-extra :int)
  (instance hinstance)
  (icon hicon)
  (cursor hcursor)
  (br-background hbrush)
  (menu-name :string)
  (class-name :string))


(defcstruct wndclass-ex
  (size :uint)
  (style class-style-flags)
  (wndproc :pointer)
  (cls-extra :int)
  (wnd-extra :int)
  (instance hinstance)
  (icon hicon)
  (cursor hcursor)
  (br-background hbrush)
  (menu-name :string)
  (class-name :string)
  (small-icon hicon))

(defcenum msg-type
  (:wm-create 1)
  (:wm-destroy 2)
  (:wm-move 3)
  (:wm-size 5)
  (:wm-activate 6)
  (:wm-set-focus 7)
  (:wm-kill-focus 8)
  (:wm-enable #xa)
  (:wm-set-redraw #xb)
  (:wm-set-text #xc)
  (:wm-get-text #xd)
  (:wm-get-text-length #xe)
  (:wm-paint #xf)
  (:wm-close #x10)
  (:wm-quit #x12)
  (:wm-erase-background #x14)
  (:wm-sys-color-change #x15)
  (:wm-show-window #x18)
  (:wm-win-ini-change #x1a)
  (:wm-win-setting-change #x1a)

  (:wm-dev-mode-change #x001b)
  (:wm-activate-app #x001c)
  (:wm-font-change #x001d)
  (:wm-time-change #x001e)
  (:wm-cancel-mode #x001f)
  (:wm-set-cursor #x0020)
  (:wm-mouse-activate #x0021)
  (:wm-child-activate #x0022)
  (:wm-queue-sync #x0023)
  (:wm-get-min-max-info #x24)

  (:WM-PAINT-ICON #x0026)
  (:WM-ICON-ERASE-background #x0027)
  (:WM-NEXT-dialog-control #x0028)
  (:WM-SPOOLER-STATUS #x002A)
  (:WM-DRAW-ITEM #x002B)
  (:WM-MEASURE-ITEM #x002C)
  (:WM-DELETE-ITEM #x002D)
  (:WM-VKEY-TO-ITEM #x002E)
  (:WM-CHAR-TO-ITEM #x002F)
  (:WM-SET-FONT #x0030)
  (:WM-GET-FONT #x0031)
  (:WM-SET-HOTKEY #x0032)
  (:WM-GET-HOTKEY #x0033)
  (:WM-QUERY-DRAG-ICON #x0037)
  (:WM-COMPARE-ITEM #x0039)
  (:WM-GET-OBJECT #x003D)
  (:WM-COMPACTING #x0041)
  (:WM-WINDOW-POS-CHANGING #x0046)
  (:WM-WINDOW-POS-CHANGED #x0047)
  (:WM-POWER #x0048)
  (:WM-COPY-DATA #x004A)
  (:WM-CANCEL-JOURNAL #x004B)
  (:WM-NOTIFY #x004E)
  (:WM-INPUT-LANG-CHANGE-REQUEST #x0050)
  (:WM-INPUT-LANG-CHANGE #x0051)
  (:WM-TCARD #x0052)
  (:WM-HELP #x0053)
  (:WM-USER-CHANGED #x0054)
  (:WM-NOTIFY-FORMAT #x0055)
  (:WM-CONTEXT-MENU #x007B)
  (:WM-STYLE-CHANGING #x007C)
  (:WM-STYLE-CHANGED #x007D)
  (:WM-DISPLAY-CHANGE #x007E)
  (:WM-GET-ICON #x007F)
  (:WM-SET-ICON #x0080)
  (:WM-NC-CREATE #x0081)
  (:WM-NC-DESTROY #x0082)
  (:WM-NC-CALC-SIZE #x0083)
  (:WM-NC-HIT-TEST #x0084)
  (:WM-NC-PAINT #x0085)
  (:WM-NC-ACTIVATE #x0086)
  (:WM-GET-dialog-CODE #x0087)
  (:WM-SYNC-PAINT #x0088)
  (:WM-UAH-DESTROY-WINDOW #x0090)
  (:WM-UAH-DRAW-MENU #x0091)
  (:WM-UAH-DRAW-MENU-ITEM #x0092)
  (:WM-UAH-INIT-MENU #x0093)
  (:WM-UAH-MEASURE-MENU-ITEM #x0094)
  (:WM-UAH-NC-PAINT-MENU-POPUP #x0095)
  (:WM-NC-MOUSE-MOVE #x00A0)
  (:WM-NC-LBUTTON-DOWN #x00A1)
  (:WM-NC-LBUTTON-UP #x00A2)
  (:WM-NC-LBUTTON-double-click #x00A3)
  (:WM-NC-RBUTTON-DOWN #x00A4)
  (:WM-NC-RBUTTON-UP #x00A5)
  (:WM-NC-RBUTTON-double-click #x00A6)
  (:WM-NC-MBUTTON-DOWN #x00A7)
  (:WM-NC-MBUTTON-UP #x00A8)
  (:WM-NC-MBUTTON-double-click #x00A9)
  (:WM-NC-XBUTTON-DOWN #x00AB)
  (:WM-NC-XBUTTON-UP #x00AC)
  (:WM-NC-XBUTTON-double-click #x00AD)
  (:WM-INPUT-DEVICE-CHANGE #x00FE)
  (:WM-INPUT #x00FF)
  (:WM-KEY-DOWN #x0100)
  (:WM-KEY-UP #x0101)
  (:WM-CHAR #x0102)
  (:WM-DEAD-CHAR #x0103)
  (:WM-SYS-KEY-DOWN #x0104)
  (:WM-SYS-KEY-UP #x0105)
  (:WM-SYS-CHAR #x0106)
  (:WM-SYS-DEAD-CHAR #x0107)
  (:WM-IME-START-COMPOSITION #x010D)
  (:WM-IME-END-COMPOSITION #x010E)
  (:WM-IME-COMPOSITION #x010F)
  (:WM-INIT-DIALOG #x0110)
  (:WM-COMMAND #x0111)
  (:WM-SYS-COMMAND #x0112)
  (:WM-TIMER #x0113)
  (:WM-HSCROLL #x0114)
  (:WM-VSCROLL #x0115)
  (:WM-INIT-MENU #x0116)
  (:WM-INIT-MENU-POPUP #x0117)
  (:WM-GESTURE #x0119)
  (:WM-GESTURE-NOTIFY #x011A)
  (:WM-MENU-SELECT #x011F)
  (:WM-MENU-CHAR #x0120)
  (:WM-ENTER-IDLE #x0121)
  (:WM-MENU-RBUTTON-UP #x0122)
  (:WM-MENU-DRAG #x0123)
  (:WM-MENU-GET-OBJECT #x0124)
  (:WM-UNINIT-MENU-POPUP #x0125)
  (:WM-MENU-COMMAND #x0126)
  (:WM-CHANGE-UI-STATE #x0127)
  (:WM-UPDATE-UI-STATE #x0128)
  (:WM-QUERY-UI-STATE #x0129)
  (:WM-CTL-COLOR-MSGBOX #x0132)
  (:WM-CTL-COLOR-EDIT #x0133)
  (:WM-CTL-COLOR-LISTBOX #x0134)
  (:WM-CTL-COLOR-BTN #x0135)
  (:WM-CTL-COLOR-DLG #x0136)
  (:WM-CTL-COLOR-SCROLLBAR #x0137)
  (:WM-CTL-COLOR-STATIC #x0138)
  (:MN-GET-HMENU #x01E1)
  (:WM-MOUSE-MOVE #x0200)
  (:WM-LBUTTON-DOWN #x0201)
  (:WM-LBUTTON-UP #x0202)
  (:WM-LBUTTON-double-click #x0203)
  (:WM-RBUTTON-DOWN #x0204)
  (:WM-RBUTTON-UP #x0205)
  (:WM-RBUTTON-double-click #x0206)
  (:WM-MBUTTON-DOWN #x0207)
  (:WM-MBUTTON-UP #x0208)
  (:WM-MBUTTON-double-click #x0209)
  (:WM-MOUSE-WHEEL #x020A)
  (:WM-XBUTTON-DOWN #x020B)
  (:WM-XBUTTON-UP #x020C)
  (:WM-XBUTTON-double-click #x020D)
  (:WM-MOUSE-HWHEEL #x020E)
  (:WM-PARENT-NOTIFY #x0210)
  (:WM-ENTER-MENU-LOOP #x0211)
  (:WM-EXIT-MENU-LOOP #x0212)
  (:WM-NEXT-MENU #x0213)
  (:WM-SIZING #x0214)
  (:WM-CAPTURE-CHANGED #x0215)
  (:WM-MOVING #x0216)
  (:WM-POWER-BROADCAST #x0218)
  (:WM-DEVICE-CHANGE #x0219)
  (:WM-MDI-CREATE #x0220)
  (:WM-MDI-DESTROY #x0221)
  (:WM-MDI-ACTIVATE #x0222)
  (:WM-MDI-RESTORE #x0223)
  (:WM-MDI-NEXT #x0224)
  (:WM-MDI-MAXIMIZE #x0225)
  (:WM-MDI-TILE #x0226)
  (:WM-MDI-CASCADE #x0227)
  (:WM-MDI-ICON-ARRANGE #x0228)
  (:WM-MDI-GET-ACTIVE #x0229)
  (:WM-MDI-SET-MENU #x0230)
  (:WM-ENTER-SIZE-MOVE #x0231)
  (:WM-EXIT-SIZE-MOVE #x0232)
  (:WM-DROP-FILES #x0233)
  (:WM-MDI-REFRESH-MENU #x0234)
  (:WM-TOUCH #x0240)
  (:WM-IME-SET-CONTEXT #x0281)
  (:WM-IME-NOTIFY #x0282)
  (:WM-IME-CONTROL #x0283)
  (:WM-IME-COMPOSITION-FULL #x0284)
  (:WM-IME-SELECT #x0285)
  (:WM-IME-CHAR #x0286)
  (:WM-IME-REQUEST #x0288)
  (:WM-IME-KEYDOWN #x0290)
  (:WM-IME-KEYUP #x0291)
  (:WM-MOUSE-HOVER #x02A1)
  (:WM-MOUSE-LEAVE #x02A3)
  (:WM-NC-MOUSE-HOVER #x02A0)
  (:WM-NC-MOUSE-LEAVE #x02A2)
  (:WM-WT-SSESSION-CHANGE #x02B1)
  (:WM-CUT #x0300)
  (:WM-COPY #x0301)
  (:WM-PASTE #x0302)
  (:WM-CLEAR #x0303)
  (:WM-UNDO #x0304)
  (:WM-RENDER-FORMAT #x0305)
  (:WM-RENDER-ALL-FORMATS #x0306)
  (:WM-DESTROY-CLIPBOARD #x0307)
  (:WM-DRAW-CLIPBOARD #x0308)
  (:WM-PAINT-CLIPBOARD #x0309)
  (:WM-VSCROLL-CLIPBOARD #x030A)
  (:WM-SIZE-CLIPBOARD #x030B)
  (:WM-ASK-CB-FORMAT-NAME #x030C)
  (:WM-CHANGE-CBC-HAIN #x030D)
  (:WM-HSCROLL-CLIPBOARD #x030E)
  (:WM-QUERY-NEW-PALETTE #x030F)
  (:WM-PALETTE-IS-CHANGING #x0310)
  (:WM-PALETTE-CHANGED #x0311)
  (:WM-HOTKEY #x0312)
  (:WM-PRINT #x0317)
  (:WM-PRINT-CLIENT #x0318)
  (:WM-APP-COMMAND #x0319)
  (:WM-THEM-ECHANGED #x031A)
  (:WM-CLIP-BOARD-UPDATE #x031D)
  (:WM-DWM-COMPOSITION-CHANGED #x031E)
  (:WM-DWM-NC-RENDERING-CHANGED #x031F)
  (:WM-DWM-COLORIZATION-COLOR-CHANGED #x0320)
  (:WM-DWM-WINDOW-MAXIMIZED-CHANGE #x0321)
  (:WM-DWM-SEND-ICON-IC-THUMBNAIL #x0323)
  (:WM-DWM-SEND-ICON-IC-LIVE-PREVIE-WBITMAP #x0326)
  (:WM-GET-TITLE-BAR-INFO-EX #x033F))

(defcenum vkey-type
  (:l-button 1)
  :r-button
  :cancel
  :m-button
  :x-button1
  :x-button2
  :backspace
  :tab
  :clear
  :return
  (:shift 16)
  (:control 17)
  :menu
  :pause
  :capital
  (:kana #x15)
  (:junja #x17)
  (:final #x18)
  (:hanja #x19)
  (:escape #x1B)
  (:convert #x1C)
  (:no-convert #x1D)
  (:accept #x1E)
  (:mode-change #x1F)
  (:space 32)
  (:page-up 33)
  (:page-down 34)
  (:end 35)
  (:home 36)
  (:left 37)
  (:up 38)
  (:right 39)
  (:down 40)
  (:select 41)
  (:print 42)
  (:execute 43)
  (:snapshot 44)
  (:insert 45)
  (:delete 46)
  (:help 47)
  (:0 #x30)
  :1
  :2
  :3
  :4
  :5
  :6
  :7
  :8
  :9
  (:a #x41)
  :b
  :c
  :d
  :e
  :f
  :g
  :h
  :i
  :j
  :k
  :l
  :m
  :n
  :o
  :p
  :q
  :r
  :s
  :t
  :u
  :v
  :w
  :x
  :y
  :z
  (:lwin #x5B)
  (:rwin #x5C)
  (:apps #x5D)
  (:sleep #x5F)
  (:numpad0 #x60)
  (:numpad1 #x61)
  (:numpad2 #x62)
  (:numpad3 #x63)
  (:numpad4 #x64)
  (:numpad5 #x65)
  (:numpad6 #x66)
  (:numpad7 #x67)
  (:numpad8 #x68)
  (:numpad9 #x69)
  (:multiply #x6A)
  (:add #x6B)
  (:separator #x6C)
  (:substract #x6D)
  (:decimal #x6E)
  (:divide #x6F)
  (:f1 #x70)
  :f2
  :f3
  :f4
  :f5
  :f6
  :f7
  :f8
  :f9
  :f10
  :f11
  :f12
  :f13
  :f14
  :f15
  :f16
  :f17
  :f18
  :f19
  :f20
  :f21
  :f22
  :f23
  :f24
  (:numlock #x90)
  :scroll
  (:shift-l #xA0)
  :shift-r
  :control-l
  :control-r
  :menu-l
  :menu-r
  :browser-back
  :browser-forward
  :browser-refresh
  :browser-stop
  :browser-search
  :browser-favorites
  :browser-home
  :volume-mute
  :volume-down
  :volume-up
  :media-next-track
  :media-prev-track
  :media-stop
  :media-play-pause
  :launch-mail
  :maunch-media-select
  :launch-app1
  :launch-app2
  :oem1
  :oem-plus
  :oem-comma
  :oem-minus
  :oem-period
  :oem2
  (:oem3 #xC0)
  (:oem4 #xDB)
  :oem5
  :oem6
  :oem7
  :oem8
  (:oem102 #xE2)
  (:process-key #xE5)
  (:packet #xE7)
  (:attn #xF6)
  :crsel
  :exsel
  :ereof
  :play
  :zoom
  :no-name
  :pa1
  :oem-clear)

(defcenum system-command-type
  (:sc-minimize #xf020)
  (:sc-maximize #xf040)
  (:sc-restore #xf120))

(defcenum sw-cmd-show
  (:sw-hide 0)
  :sw-normal
  (:sw-show-normal 1)
  :sw-show-minimized
  :sw-maximize
  (:sw-show-maximized 3)
  :sw-show-no-activate
  :sw-show
  :sw-minimize
  :sw-show-min-no-activate
  :sw-show-na
  :sw-restore
  :sw-show-default
  :sw-force-minimize
  (:sw-max 11))

(defcenum remove-msg
  (:pm-no-remove 0)
  (:pm-remove 1))

(defcstruct rect
  (left :long)
  (top :long)
  (right :long)
  (bottom :long))

(defcenum display-settings-mode
  (:cds-update-registry 1)
  (:cds-test 2)
  (:cds-fullscreen 4)
  (:cds-global 8)
  (:cds-set-primary 16)
  (:cds-reset #x40000000)
  (:cds-setrect #x20000000)
  (:cds-no-reset #x10000000))

(defbitfield device-mode-fields
  (:dm-bits-per-pixel #x00040000)
  (:dm-pels-width #x00080000)
  (:dm-pels-height #x00100000)
  (:dm-display-frequency #x00400000))

(defbitfield swp-flags
  (:swp-no-size #x0001)
  (:swp-no-move #x0002)
  (:swp-no-zorder #x0004)
  (:swp-no-redraw #x0008)
  (:swp-no-activate #x0010)
  (:swp-frame-changed #x0020)
  (:swp-show-window #x0040)
  (:swp-hide-window #x0080)
  (:swp-no-copy-bits #x0100)
  (:swp-no-owner-zorder #x0200)
  (:swp-no-send-changing #x0400)
  (:swp-draw-frame #x0020)
  (:swp-no-reposition #x0200)
  (:swp-defer-erase #x2000)
  (:swp-async-window-pos #x4000))

(defcstruct devmode
  (device-name :char :count 32) ;; CCHDEVICENAME = 32 (winuser.h)
  (spec-version word)
  (driver-version word)
  (size word)
  (driver-extra word)
  (fields dword)
  (union-1 :short :count 8) ;; XXX: orientation data is here
  (color :short)
  (duplex :short)
  (y-resolution :short)
  (tt-option :short)
  (collate :short)
  (form-name :char :count 32) ;; CCHFORMNAME = 32
  (log-pixels word)
  (bits-per-pixel dword)
  (pels-width dword)
  (pels-height dword)
  (display-flags dword) ;; this is also dmNup
  (display-frequency dword)
  ;; WINVER >= 0x0400
  (icm-method dword)
  (icm-intent dword)
  (media-type dword)
  (dither-type dword)
  (reserved-1 dword)
  (reserved-2 dword)
  ;; WINVER >= 0x0500 || _WIN32_WINNT >= 0x0400
  (panning-width dword)
  (panning-height dword))

(define-foreign-library user32
  (t (:default "user32")))
(use-foreign-library user32)

(defcfun ("ShowCursor" show-cursor) :int
  (show bool))

(defcfun ("EnumDisplaySettingsA" enum-display-settings) bool
  (device-name :string) (mode-num dword) (dev-mode :pointer))

(defcfun ("ChangeDisplaySettingsA" change-display-settings) :long
  (dmode devmode) (flags dword))

(defcfun ("GetWindowLongA" get-window-long) :long
  (wnd hwnd) (index gwl-index))

(defcfun ("SetWindowLongA" set-window-long) :long
  (wnd hwnd) (index gwl-index) (new-long :unsigned-long))

(defcfun ("SetWindowPos" set-window-pos) bool
  (wnd hwnd)
  (wnd-insert-after hwnd)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (flags swp-flags))

(defun current-video-mode ()
  (with-foreign-object (dmode 'devmode)
	(with-foreign-slots ((size bits-per-pixel pels-width pels-height display-frequency)
						 dmode devmode)
	  (setf size (foreign-type-size 'devmode))
	  (enum-display-settings (cffi:null-pointer) -1 dmode)
	  (glop::make-video-mode :width pels-width
							 :height pels-height
							 :depth bits-per-pixel
							 :rate display-frequency))))

(defun list-video-modes ()
  (with-foreign-object (dmode 'devmode)
	(with-foreign-slots ((size bits-per-pixel pels-width pels-height display-frequency)
						 dmode devmode)
	  (setf size (foreign-type-size 'devmode))
	  (loop with mode-index = 0
		 for res = (enum-display-settings (cffi:null-pointer) mode-index dmode)
		 do (incf mode-index)
		 until (zerop res)
		 collect (glop::make-video-mode :width pels-width
										:height pels-height
										:depth bits-per-pixel
										:rate display-frequency)))))

(defun set-video-mode (mode)
  (let ((width (glop::video-mode-width mode))
		(height (glop::video-mode-height mode))
		(depth (glop::video-mode-depth mode))
		(rate (glop::video-mode-rate mode)))
	(with-foreign-object (dmode 'devmode)
	  (with-foreign-slots ((size bits-per-pixel pels-width pels-height display-frequency fields)
						   dmode devmode)
		(setf size (foreign-type-size 'devmode))
		(enum-display-settings (cffi:null-pointer) -1 dmode)
		(setf pels-width width
			  pels-height height
			  display-frequency rate
			  bits-per-pixel depth
			  fields (foreign-bitfield-value 'device-mode-fields '(:dm-pels-width
																   :dm-pels-height
																   :dm-bits-per-pixel
																   :dm-display-frequency)))
		(change-display-settings dmode
								 (foreign-enum-value 'display-settings-mode
													 :cds-fullscreen))))))

(defun default-video-mode ()
  (change-display-settings (cffi:null-pointer) 0))

(defun %set-borderless (wnd state &key keep-client-size)
  (multiple-value-bind (.x .y w h)
      (glop-win32::get-client-rect wnd)
    (multiple-value-bind (x y) (glop-win32:client-to-screen wnd .x .y)
      (let ((style (if state
                       '(:ws-popup :ws-clip-siblings :ws-clip-children)
                       '(:ws-overlapped-window
                         :ws-clip-siblings :ws-clip-children)))
            (ex-style (if state
                          '(:ws-ex-app-window :ws-ex-topmost)
                          '(:ws-ex-app-window :ws-ex-window-edge))))
        (set-window-long wnd :gwl-style
                         (foreign-bitfield-value 'wstyle style))
        (set-window-long wnd :gwl-ex-style
                         (foreign-bitfield-value 'wex-style ex-style)))
      ;; need to call set-window-pos for some changes to take effect
      (set-window-pos wnd (cffi:null-pointer) 0 0 0 0 '(:swp-no-move
                                                        :swp-no-size
                                                        :swp-frame-changed
                                                        :swp-no-zorder
                                                        :swp-no-copy-bits))
      ;; make sure client rect didn't change size
      (when keep-client-size
        (multiple-value-bind (.x2 .y2 w2 h2)
            (glop-win32::get-client-rect wnd)
          (multiple-value-bind (x2 y2) (glop-win32:client-to-screen wnd .x2 .y2)
            (unless (and (= x x2) (= y y2) (= w w2) (= h h2))
              (set-window-pos wnd (cffi:null-pointer) x y w h
                              '(:swp-no-zorder :swp-no-copy-bits)))))))))

(defun %maximize-window (wnd)
  (show-window wnd :sw-show-maximized))

(defun %restore-window (wnd)
  (show-window wnd :sw-show-normal))

(defun %set-fullscreen (wnd state)
  (if state
      (progn
        (%set-borderless wnd t)
        (%maximize-window wnd))
      (progn
        (%set-borderless wnd nil)
        (%restore-window wnd))))

(defcfun ("GetClientRect" %get-client-rect) bool
  (wnd hwnd) (rect-out :pointer))

(defun get-client-rect (wnd)
  (with-foreign-object (rct 'rect)
	(%get-client-rect wnd rct)
	(with-foreign-slots ((left top right bottom) rct rect)
	  (values left top
			  (- right left)
			  (- bottom top)))))

(defcfun ("GetWindowRect" %get-window-rect) bool
  (wnd hwnd) (rect-out :pointer))

(defun get-window-rect (wnd)
  (with-foreign-object (rct 'rect)
	(%get-window-rect wnd rct)
	(with-foreign-slots ((left top right bottom) rct rect)
	  (values left top
			  (- right left)
			  (- bottom top)))))

(defun get-client-area-offset (wnd)
  (multiple-value-bind (wx wy ww wh) (get-window-rect wnd)
	(multiple-value-bind (cx cy cw ch) (get-client-rect wnd)
	  (declare (ignore ww wh cw ch))
	  (values (- cx wx) (- cy wy)))))


(defcfun ("AdjustWindowRectEx" %adjust-window-rect-ex) bool
  (rect (:pointer (:struct rect)))
  (style wstyle)
  (menu bool)
  (ex-style wex-style))

(defun adjust-window-rect-ex (x y width height &key style menu ex-style)
  (with-foreign-object (rect '(:struct rect))
    (with-foreign-slots ((left top right bottom) rect (:struct rect))
      (setf left x
            top y
            right (+ x width)
            bottom (+ y height))
      (when (zerop
               (%adjust-window-rect-ex rect style (if menu 1 0) ex-style))
        (error "adjust-window-rect-ex failed ~s" (get-last-error)))
      (values left top
              (- right left)
              (- bottom top)))))

(defcfun ("ClientToScreen" %client-to-screen) bool
  (hwnd hwnd)
  (point (:pointer (:struct point))))

(defun client-to-screen (hwnd cx cy)
  (with-foreign-object (p '(:struct point))
    (with-foreign-slots ((x y) p (:struct point))
      (setf x cx y cy)
      (%client-to-screen hwnd p)
      (values x y))))


(defun %update-geometry-from-window (win)
  ;; update geometry from client rect of actual window
  (let ((wnd (glop:win32-window-id win)))
    (multiple-value-bind (cx cy cwidth cheight)
        (glop-win32::get-client-rect wnd)
      (multiple-value-bind (sx sy)
          (glop-win32::client-to-screen wnd cx cy)
        (glop::%update-geometry win sx sy cwidth cheight)))))


(defcfun ("MoveWindow" move-window) bool
  (wnd hwnd) (x :int) (y :int) (width :int) (height :int)
  (repaint bool))

(defun set-geometry (wnd x y width height)
  ;; we specify position/size of client rect, convert to whole window
  (multiple-value-bind (ax ay aw ah)
      (glop-win32:adjust-window-rect-ex x y width height)
    (setf x ax y ay width aw height ah))
  (move-window wnd x y width height 1))

(defcfun ("SetCapture" set-capture) hwnd
  (wnd hwnd))

(defcfun ("ReleaseCapture" release-capture) bool)

(defcfun ("GetDC" get-dc) hdc
  (wnd hwnd))

(defcfun ("ReleaseDC" %release-dc) :int
  (wnd hwnd) (dc hdc))

(defcfun ("PostQuitMessage" %post-quit-message) :void
  (exit-code :int))

(defcfun ("DefWindowProcA" %def-window-proc) :long
  (wnd hwnd) (msg :uint) (w-param wparam) (l-param lparam))

(defcfun ("GetMessageA" %get-message) bool
  (msg :pointer) (wnd hwnd) (filter-min :uint) (filter-max :uint))

(defcfun ("TranslateMessage" %translate-message) bool
  (msg :pointer))

(defcfun ("DispatchMessageA" %dispatch-message) bool
  (msg :pointer))

(defcfun ("PeekMessageA" %peek-message) bool
  (lpmsg :pointer) (h-wnd hwnd)
  (filter-min :uint) (filter-max :uint)
  (remove remove-msg))

(defcfun ("GetKeyboardState" get-keyboard-state) bool
  (state-out :pointer))



(defcfun ("ToAscii" to-ascii) :int
  (vkey :uint)
  (scan-code :uint) (kbd-state :pointer) (buffer :pointer) (flags :uint))

(defcfun ("ToUnicode" to-unicode) :int
  (vkey :uint)
  (scan-code :uint) (kbd-state :pointer) (buffer :pointer) (buffer-size :int) (flags :uint))

;; XXX: this is an ugly hack and should probably be changed
;; We use the %event% var to allow window-proc callback to generate glop:event objects
;; that can be return from next-event

(defvar %event% nil)

(defun next-event (win wnd &optional blocking)
  (when (glop::win32-window-pushed-size-event win)
    (return-from next-event (shiftf (glop::win32-window-pushed-size-event win)
                                    nil)))
  (let ((%window% win))
    (with-foreign-object (msg '(:struct msg))
      (if blocking
          (when (> (%get-message msg wnd 0 0) 0)
            (%translate-message msg)
            (%dispatch-message msg))
          (when (> (%peek-message msg wnd 0 0 :pm-remove) 0)
            (%translate-message msg)
            (%dispatch-message msg)))))
  %event%)

;; XXX: we probably have problems with negative numbers here...
(defun low-word (value)
  (logand value #xFFFF))

(defun high-word (value)
  (logand (ash value -16) #xFFFF))

(defun win32-lookup-key (w-param l-param)
  (values (foreign-enum-keyword 'vkey-type w-param :errorp nil)
		  (with-foreign-object (kbd-state :char 256)
			(when (get-keyboard-state kbd-state)
			  (with-foreign-object (buffer :int32)
				(setf (mem-ref buffer :int32) 0)
				(let ((res (to-unicode (ldb (byte 32 0) w-param)
									   (ldb (byte 32 0) l-param)
									   kbd-state buffer 4 0)))
				  (case res
					(0 nil)
					(t (foreign-string-to-lisp buffer)))))))))


(let ((last-x 0)
	  (last-y 0))
  (defcallback window-proc :long ((wnd hwnd) (msg :uint) (w-param wparam) (l-param lparam))
    (let ((msg-type (foreign-enum-keyword 'msg-type msg :errorp nil))
          (%window% (gethash (pointer-address wnd) *window-id-mapping*))
          )
      (flet ((resize-event ()
               (when %window%
                (setf (glop::win32-window-pushed-size-event %window%)
                      (make-instance 'glop:resize-event
                                     :width (glop:window-width %window%)
                                     :height (glop:window-height %window%))))))
        (case msg-type
          (:wm-close
           (setf %event% (glop::make-instance 'glop:close-event))
           (return-from window-proc 0))
          (:wm-destroy
           (%post-quit-message 0)
           (remhash (pointer-address wnd) *window-id-mapping*)
           (return-from window-proc 0))
          (:wm-mouse-move
           (let ((low (low-word l-param))
                 (high (high-word l-param)))
             (when (or (/= low last-x) (/= high last-y))
               (setf %event% (glop::make-instance 'glop:mouse-motion-event
                                                  :x low :y high
                                                  :dx (- low last-x) :dy (- high last-y)))
               (setf last-x low last-y high))
             (return-from window-proc 0)))
          (:wm-size
           (when %window% ;; XXX: WM_SIZE is called on window creation when %window% is nil ...
             (%update-geometry-from-window %window%)
             (unless (glop::win32-window-in-size-move %window%)
               (resize-event)))
           (return-from window-proc 0))
          (:wm-move
           (when %window% ;; XXX: WM_MOVE is called on window creation when %window% is nil ...
             (%update-geometry-from-window %window%)
             (unless (glop::win32-window-in-size-move %window%)
               (resize-event))
             (return-from window-proc 0)))
          (:wm-enter-size-move
           (setf (glop::win32-window-in-size-move %window%) t)
           (return-from window-proc 0))
          (:wm-exit-size-move
           (setf (glop::win32-window-in-size-move %window%) nil)
           (resize-event)
           (return-from window-proc 0))
          (:wm-paint ;; XXX: we need to call defaut windowproc too here
           (multiple-value-bind (x y width height) (get-client-rect wnd)
             (declare (ignore x y))
             (setf %event% (glop::make-instance 'glop:expose-event
                                                :width width :height height))))
          (:wm-lbutton-down
           (set-capture wnd)
           (setf %event% (glop::make-instance 'glop:button-press-event
                                              :button 1))
           (return-from window-proc 0))
          (:wm-lbutton-up
           (release-capture)
           (setf %event% (glop::make-instance 'glop:button-release-event
                                              :button 1))
           (return-from window-proc 0))
          (:wm-rbutton-down
           (set-capture wnd)
           (setf %event% (glop::make-instance 'glop:button-press-event
                                              :button 3))
           (return-from window-proc 0))
          (:wm-rbutton-up
           (release-capture)
           (setf %event% (glop::make-instance 'glop:button-release-event
                                              :button 3))
           (return-from window-proc 0))
          (:wm-mbutton-down
           (set-capture wnd)
           (setf %event% (glop::make-instance 'glop:button-press-event
                                              :button 2))
           (return-from window-proc 0))
          (:wm-mbutton-up
           (release-capture)
           (setf %event% (glop::make-instance 'glop:button-release-event
                                              :button 2))
           (return-from window-proc 0))
          (:wm-key-up
           (multiple-value-bind (keysym text) (win32-lookup-key w-param l-param)
             (setf (glop:key-pressed w-param) nil)
             (setf %event% (glop::make-instance 'glop:key-release-event
                                                :keycode	 w-param
                                                :keysym keysym
                                                :text text)))
           (return-from window-proc 0))
          (:wm-key-down
           (multiple-value-bind (keysym text) (win32-lookup-key w-param l-param)
             (when (and glop:*ignore-auto-repeat* (glop:key-pressed w-param))
               (return-from window-proc 0))
             (setf (glop:key-pressed w-param) t)
             (setf %event% (glop::make-instance 'glop:key-press-event
                                                :keycode	 w-param
                                                :keysym keysym
                                                :text text)))
           (return-from window-proc 0))
          (:wm-mouse-wheel
           (setf %event% (glop::make-instance 'glop:button-press-event
                                              :button (if (> w-param 0)
                                                          4 5)))
           (return-from window-proc 0))
          (:wm-show-window
           (setf %event% (glop::make-instance (if (zerop w-param)
                                                  'glop:visibility-unobscured-event
                                                  'glop:visibility-obscured-event)))
           (return-from window-proc 0))
          (:wm-set-focus
           (setf %event% (make-instance 'glop:focus-in-event))
           (return-from window-proc 0))
          (:wm-kill-focus
           (setf %event% (make-instance 'glop:focus-out-event))
           (return-from window-proc 0))
          (:wm-erase-background
           (return-from window-proc 0))
          (:wm-dwm-composition-changed
           ;; assuming if we get this, dwm-is-composition-enabled returns
           ;; meaningful data...
           (glop::%dwm-composition-changed %window%))))
      ;; Pass unhandled messages to default windowproc
      (%def-window-proc wnd msg w-param l-param))))

(defcfun ("RegisterClassA" %register-class) :int16
  (wndclass :pointer))

(defcfun ("RegisterClassExA" %register-class-ex) :int16
  (wndclass-ex :pointer))

(defcfun ("GetClassInfoA" %get-class-info) bool
  (instance hinstance) (class-name :string) (wndclass :pointer))

(defcfun ("UnregisterClassA" unregister-class) bool
  (class-name :string) (instance hinstance))

(defun class-exists-p (module-instance name)
  (with-foreign-object (class 'wndclass)
	(%get-class-info module-instance name class)))

(defun create-and-register-class (module-instance name)
  (with-foreign-object (class 'wndclass)
    (with-foreign-slots ((style wndproc cls-extra wnd-extra instance icon cursor
                                br-background menu-name class-name) class wndclass)
      (setf style (foreign-bitfield-value 'class-style-flags
                                          '(:cs-hredraw :cs-vredraw :cs-own-dc))
            wndproc (callback window-proc)
            cls-extra 0
            wnd-extra 0
            instance module-instance
            icon	(null-pointer)
            cursor (null-pointer)
            br-background (null-pointer)
            menu-name (null-pointer)
            class-name name))
	  (when (zerop (%register-class class))
		(format t "Error registering class ~S: ~S~%" name (get-last-error)))))

(defcfun ("SetWindowTextA" set-window-text) bool
  (wnd hwnd) (title :string))

(defcfun ("CreateWindowExA" create-window-ex) hwnd
  (ex-style wex-style) (class-name :string) (win-name :string)
  (style wstyle) (x :int) (y :int) (width :int) (height :int)
  (parent hwnd) (menu hmenu) (instance hinstance) (param :pointer))

(defcfun ("DestroyWindow" destroy-window) bool
  (wnd hwnd))

(defcfun ("UpdateWindow" update-window) bool
  (wnd hwnd))

(defcfun ("ShowWindow" show-window) bool
  (wnd hwnd) (cmd-show sw-cmd-show))

(defcfun ("SetForegroundWindow" set-foreground-window) bool
  (wnd hwnd))

(defcfun ("SetFocus" set-focus) hwnd
  (wnd hwnd))

(define-foreign-library kernel32
  (t (:default "kernel32")))
(use-foreign-library kernel32)

(defcfun ("GetModuleHandleW" get-module-handle) hmodule
  (module-name :string))

(defcfun ("GetLastError" get-last-error) :int32)

(defcfun ("GetVersion" %get-version) dword)

(defun get-version* ()
  (let ((v (%get-version)))
    (values (ldb (byte 8 0) v) ; major
            (ldb (byte 8 8) v) ; minor
            (ldb (byte 16 16) v) ; build
            )))

(defun get-version ()
  ;; Windows 10 Insider Preview         10.0*
  ;; Windows Server Technical Preview   10.0*
  ;; Windows 8.1                        6.3*
  ;; Windows Server 2012 R2             6.3*
  ;; * only reports 6.3+ with manifest, and then only reports specific
  ;;    version listed in manifest
  ;; Windows 8                          6.2
  ;; Windows Server 2012                6.2
  ;; Windows 7                          6.1
  ;; Windows Server 2008 R2             6.1
  ;; Windows Server 2008                6.0
  ;; Windows Vista                      6.0
  ;; Windows Server 2003 R2             5.2
  ;; Windows Server 2003                5.2
  ;; Windows XP 64-Bit Edition          5.2
  ;; Windows XP                         5.1
  ;; Windows 2000                       5.0
  (let ((v (%get-version)))
    (float (+ (ldb (byte 8 0) v)
              (/ (ldb (byte 8 8) v) 10)))))

