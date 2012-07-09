(in-package :glop-xlib)

;;; Taken from X11's keysymdef.h header, capital- prefixed on redundant symbols, with deprecated entries removed.
(defcenum x-keysym-value
  (:voidsymbol #xffffff)              ; void symbol
  ;; tty function keys, cleverly chosen to map to ascii, for convenience of
  ;; programming, but could have been arbitrary (at the cost of lookup
  ;; tables in client code).
  (:backspace #xff08)                 ; back space, back char
  (:tab #xff09)
  (:linefeed #xff0a)                  ; linefeed, lf
  (:clear #xff0b)
  (:return #xff0d)                    ; return, enter
  (:pause #xff13)                     ; pause, hold
  (:scroll-lock #xff14)
  (:sys-req #xff15)
  (:escape #xff1b)
  (:delete #xffff)                    ; delete, rubout

  ;; international & multi-key character composition
  (:multi-key #xff20)  ; multi-key character compose
  (:codeinput #xff37)
  (:singlecandidate #xff3c)
  (:multiplecandidate #xff3d)
  (:previouscandidate #xff3e)

  ;; japanese keyboard support

  (:kanji #xff21)  ; kanji, kanji convert
  (:muhenkan #xff22)  ; cancel conversion
  (:henkan-mode #xff23)  ; start/stop conversion
  (:henkan #xff23)  ; alias for henkan-mode
  (:romaji #xff24)  ; to romaji
  (:hiragana #xff25)  ; to hiragana
  (:katakana #xff26)  ; to katakana
  (:hiragana-katakana #xff27)  ; hiragana/katakana toggle
  (:zenkaku #xff28)  ; to zenkaku
  (:hankaku #xff29)  ; to hankaku
  (:zenkaku-hankaku #xff2a)  ; zenkaku/hankaku toggle
  (:touroku #xff2b)  ; add to dictionary
  (:massyo #xff2c)  ; delete from dictionary
  (:kana-lock #xff2d)  ; kana lock
  (:kana-shift #xff2e)  ; kana shift
  (:eisu-shift #xff2f)  ; alphanumeric shift
  (:eisu-toggle #xff30)  ; alphanumeric toggle
  (:kanji-bangou #xff37)  ; codeinput
  (:zen-koho #xff3d)  ; multiple/all candidate(:s)
  (:mae-koho #xff3e)  ; previous candidate

  ;; #xff31 thru #xff3f are under korean
  ;; cursor control & motion
  (:home #xff50)
  (:left #xff51)  ; move left, left arrow
  (:up #xff52)  ; move up, up arrow
  (:right #xff53)  ; move right, right arrow
  (:down #xff54)  ; move down, down arrow
  (:prior #xff55)  ; prior, previous
  (:page-up #xff55)
  (:next #xff56)  ; next
  (:page-down #xff56)
  (:end #xff57)  ; eol
  (:begin #xff58)  ; bol


  ;; misc functions

  (:select #xff60)  ; select, mark
  (:print #xff61)
  (:execute #xff62)  ; execute, run, do
  (:insert #xff63)  ; insert, insert here
  (:undo #xff65)
  (:redo #xff66)  ; redo, again
  (:menu #xff67)
  (:find #xff68)  ; find, search
  (:cancel #xff69)  ; cancel, stop, abort, exit
  (:help #xff6a)  ; help
  (:break #xff6b)
  (:mode-switch #xff7e)  ; character set switch
  (:script-switch #xff7e)  ; alias for mode-switch
  (:num-lock #xff7f)

  ;; keypad functions, keypad numbers cleverly chosen to map to ascii

  (:kp-space #xff80)  ; space
  (:kp-tab #xff89)
  (:kp-enter #xff8d)  ; enter
  (:kp-f1 #xff91)  ; pf1, kp-a, ...
  (:kp-f2 #xff92)
  (:kp-f3 #xff93)
  (:kp-f4 #xff94)
  (:kp-home #xff95)
  (:kp-left #xff96)
  (:kp-up #xff97)
  (:kp-right #xff98)
  (:kp-down #xff99)
  (:kp-prior #xff9a)
  (:kp-page-up #xff9a)
  (:kp-next #xff9b)
  (:kp-page-down #xff9b)
  (:kp-end #xff9c)
  (:kp-begin #xff9d)
  (:kp-insert #xff9e)
  (:kp-delete #xff9f)
  (:kp-equal #xffbd)  ; equals
  (:kp-multiply #xffaa)
  (:kp-add #xffab)
  (:kp-separator #xffac)  ; separator, often comma
  (:kp-subtract #xffad)
  (:kp-decimal #xffae)
  (:kp-divide #xffaf)

  (:kp-0 #xffb0)
  (:kp-1 #xffb1)
  (:kp-2 #xffb2)
  (:kp-3 #xffb3)
  (:kp-4 #xffb4)
  (:kp-5 #xffb5)
  (:kp-6 #xffb6)
  (:kp-7 #xffb7)
  (:kp-8 #xffb8)
  (:kp-9 #xffb9)
  ;; auxiliary functions; note the duplicate definitions for left and right
  ;; function keys;  sun keyboards and a few other manufacturers have such
  ;; function key groups on the left and/or right sides of the keyboard.
  ;; we've not found a keyboard with more than 35 function keys total.


  (:f1 #xffbe)
  (:f2 #xffbf)
  (:f3 #xffc0)
  (:f4 #xffc1)
  (:f5 #xffc2)
  (:f6 #xffc3)
  (:f7 #xffc4)
  (:f8 #xffc5)
  (:f9 #xffc6)
  (:f10 #xffc7)
  (:f11 #xffc8)
  (:l1 #xffc8)
  (:f12 #xffc9)
  (:l2 #xffc9)
  (:f13 #xffca)
  (:l3 #xffca)
  (:f14 #xffcb)
  (:l4 #xffcb)
  (:f15 #xffcc)
  (:l5 #xffcc)
  (:f16 #xffcd)
  (:l6 #xffcd)
  (:f17 #xffce)
  (:l7 #xffce)
  (:f18 #xffcf)
  (:l8 #xffcf)
  (:f19 #xffd0)
  (:l9 #xffd0)
  (:f20 #xffd1)
  (:l10 #xffd1)
  (:f21 #xffd2)
  (:r1 #xffd2)
  (:f22 #xffd3)
  (:r2 #xffd3)
  (:f23 #xffd4)
  (:r3 #xffd4)
  (:f24 #xffd5)
  (:r4 #xffd5)
  (:f25 #xffd6)
  (:r5 #xffd6)
  (:f26 #xffd7)
  (:r6 #xffd7)
  (:f27 #xffd8)
  (:r7 #xffd8)
  (:f28 #xffd9)
  (:r8 #xffd9)
  (:f29 #xffda)
  (:r9 #xffda)
  (:f30 #xffdb)
  (:r10 #xffdb)
  (:f31 #xffdc)
  (:r11 #xffdc)
  (:f32 #xffdd)
  (:r12 #xffdd)
  (:f33 #xffde)
  (:r13 #xffde)
  (:f34 #xffdf)
  (:r14 #xffdf)
  (:f35 #xffe0)
  (:r15 #xffe0)

  ;; modifiers

  (:shift-l #xffe1)  ; left shift
  (:shift-r #xffe2)  ; right shift
  (:control-l #xffe3)  ; left control
  (:control-r #xffe4)  ; right control
  (:caps-lock #xffe5)  ; caps lock
  (:shift-lock #xffe6)  ; shift lock

  (:meta-l #xffe7)  ; left meta
  (:meta-r #xffe8)  ; right meta
  (:alt-l #xffe9)  ; left alt
  (:alt-r #xffea)  ; right alt
  (:super-l #xffeb)  ; left super
  (:super-r #xffec)  ; right super
  (:hyper-l #xffed)  ; left hyper
  (:hyper-r #xffee)  ; right hyper
  ;; miscellany

  ;; keyboard (:xkb) extension function and modifier keys
  ;; (from appendix c of "the x keyboard extension: protocol specification")
  ;; byte 3 = #xfe

  
  (:iso-lock #xfe01)
  (:iso-level2-latch #xfe02)
  (:iso-level3-shift #xfe03)
  (:iso-level3-latch #xfe04)
  (:iso-level3-lock #xfe05)
  (:iso-level5-shift #xfe11)
  (:iso-level5-latch #xfe12)
  (:iso-level5-lock #xfe13)
  (:iso-group-shift #xff7e)  ; alias for mode-switch
  (:iso-group-latch #xfe06)
  (:iso-group-lock #xfe07)
  (:iso-next-group #xfe08)
  (:iso-next-group-lock #xfe09)
  (:iso-prev-group #xfe0a)
  (:iso-prev-group-lock #xfe0b)
  (:iso-first-group #xfe0c)
  (:iso-first-group-lock #xfe0d)
  (:iso-last-group #xfe0e)
  (:iso-last-group-lock #xfe0f)

  (:iso-left-tab #xfe20)
  (:iso-move-line-up #xfe21)
  (:iso-move-line-down #xfe22)
  (:iso-partial-line-up #xfe23)
  (:iso-partial-line-down #xfe24)
  (:iso-partial-space-left #xfe25)
  (:iso-partial-space-right #xfe26)
  (:iso-set-margin-left #xfe27)
  (:iso-set-margin-right #xfe28)
  (:iso-release-margin-left #xfe29)
  (:iso-release-margin-right #xfe2a)
  (:iso-release-both-margins #xfe2b)
  (:iso-fast-cursor-left #xfe2c)
  (:iso-fast-cursor-right #xfe2d)
  (:iso-fast-cursor-up #xfe2e)
  (:iso-fast-cursor-down #xfe2f)
  (:iso-continuous-underline #xfe30)
  (:iso-discontinuous-underline #xfe31)
  (:iso-emphasize #xfe32)
  (:iso-center-object #xfe33)
  (:iso-enter #xfe34)

  (:dead-grave #xfe50)
  (:dead-acute #xfe51)
  (:dead-circumflex #xfe52)
  (:dead-tilde #xfe53)
  (:dead-perispomeni #xfe53)  ; alias for dead-tilde
  (:dead-macron #xfe54)
  (:dead-breve #xfe55)
  (:dead-abovedot #xfe56)
  (:dead-diaeresis #xfe57)
  (:dead-abovering #xfe58)
  (:dead-doubleacute #xfe59)
  (:dead-caron #xfe5a)
  (:dead-cedilla #xfe5b)
  (:dead-ogonek #xfe5c)
  (:dead-iota #xfe5d)
  (:dead-voiced-sound #xfe5e)
  (:dead-semivoiced-sound #xfe5f)
  (:dead-belowdot #xfe60)
  (:dead-hook #xfe61)
  (:dead-horn #xfe62)
  (:dead-stroke #xfe63)
  (:dead-abovecomma #xfe64)
  (:dead-psili #xfe64)  ; alias for dead-abovecomma
  (:dead-abovereversedcomma #xfe65)
  (:dead-dasia #xfe65)  ; alias for dead-abovereversedcomma
  (:dead-doublegrave #xfe66)
  (:dead-belowring #xfe67)
  (:dead-belowmacron #xfe68)
  (:dead-belowcircumflex #xfe69)
  (:dead-belowtilde #xfe6a)
  (:dead-belowbreve #xfe6b)
  (:dead-belowdiaeresis #xfe6c)
  (:dead-invertedbreve #xfe6d)
  (:dead-belowcomma #xfe6e)
  (:dead-currency #xfe6f)

  ;; dead vowels for universal syllable entry
  (:dead-a #xfe80)
  (:dead-capital-a #xfe81)
  (:dead-e #xfe82)
  (:dead-capital-e #xfe83)
  (:dead-i #xfe84)
  (:dead-capital-i #xfe85)
  (:dead-o #xfe86)
  (:dead-capital-o #xfe87)
  (:dead-u #xfe88)
  (:dead-capital-u #xfe89)
  (:dead-small-schwa #xfe8a)
  (:dead-capital-schwa #xfe8b)

  (:first-virtual-screen #xfed0)
  (:prev-virtual-screen #xfed1)
  (:next-virtual-screen #xfed2)
  (:last-virtual-screen #xfed4)
  (:terminate-server #xfed5)

  (:accessx-enable #xfe70)
  (:accessx-feedback-enable #xfe71)
  (:repeatkeys-enable #xfe72)
  (:slowkeys-enable #xfe73)
  (:bouncekeys-enable #xfe74)
  (:stickykeys-enable #xfe75)
  (:mousekeys-enable #xfe76)
  (:mousekeys-accel-enable #xfe77)
  (:overlay1-enable #xfe78)
  (:overlay2-enable #xfe79)
  (:audiblebell-enable #xfe7a)

  (:pointer-left #xfee0)
  (:pointer-right #xfee1)
  (:pointer-up #xfee2)
  (:pointer-down #xfee3)
  (:pointer-upleft #xfee4)
  (:pointer-upright #xfee5)
  (:pointer-downleft #xfee6)
  (:pointer-downright #xfee7)
  (:pointer-button-dflt #xfee8)
  (:pointer-button1 #xfee9)
  (:pointer-button2 #xfeea)
  (:pointer-button3 #xfeeb)
  (:pointer-button4 #xfeec)
  (:pointer-button5 #xfeed)
  (:pointer-dblclick-dflt #xfeee)
  (:pointer-dblclick1 #xfeef)
  (:pointer-dblclick2 #xfef0)
  (:pointer-dblclick3 #xfef1)
  (:pointer-dblclick4 #xfef2)
  (:pointer-dblclick5 #xfef3)
  (:pointer-drag-dflt #xfef4)
  (:pointer-drag1 #xfef5)
  (:pointer-drag2 #xfef6)
  (:pointer-drag3 #xfef7)
  (:pointer-drag4 #xfef8)
  (:pointer-drag5 #xfefd)

  (:pointer-enablekeys #xfef9)
  (:pointer-accelerate #xfefa)
  (:pointer-dfltbtnnext #xfefb)
  (:pointer-dfltbtnprev #xfefc)

  


  ;; 3270 terminal keys
  ;; byte 3 = #xfd


  
  (:3270-duplicate #xfd01)
  (:3270-fieldmark #xfd02)
  (:3270-right2 #xfd03)
  (:3270-left2 #xfd04)
  (:3270-backtab #xfd05)
  (:3270-eraseeof #xfd06)
  (:3270-eraseinput #xfd07)
  (:3270-reset #xfd08)
  (:3270-quit #xfd09)
  (:3270-pa1 #xfd0a)
  (:3270-pa2 #xfd0b)
  (:3270-pa3 #xfd0c)
  (:3270-test #xfd0d)
  (:3270-attn #xfd0e)
  (:3270-cursorblink #xfd0f)
  (:3270-altcursor #xfd10)
  (:3270-keyclick #xfd11)
  (:3270-jump #xfd12)
  (:3270-ident #xfd13)
  (:3270-rule #xfd14)
  (:3270-copy #xfd15)
  (:3270-play #xfd16)
  (:3270-setup #xfd17)
  (:3270-record #xfd18)
  (:3270-changescreen #xfd19)
  (:3270-deleteword #xfd1a)
  (:3270-exselect #xfd1b)
  (:3270-cursorselect #xfd1c)
  (:3270-printscreen #xfd1d)
  (:3270-enter #xfd1e)
  


  ;; latin 1
  ;; (:iso/iec 8859-1 = unicode u+0020..u+00ff)
  ;; byte 3 = 0

  
  (:space #x0020)  ; u+0020 space
  (:exclam #x0021)  ; u+0021 exclamation mark
  (:quotedbl #x0022)  ; u+0022 quotation mark
  (:numbersign #x0023)  ; u+0023 number sign
  (:dollar #x0024)  ; u+0024 dollar sign
  (:percent #x0025)  ; u+0025 percent sign
  (:ampersand #x0026)  ; u+0026 ampersand
  (:apostrophe #x0027)  ; u+0027 apostrophe
  (:parenleft #x0028)  ; u+0028 left parenthesis
  (:parenright #x0029)  ; u+0029 right parenthesis
  (:asterisk #x002a)  ; u+002a asterisk
  (:plus #x002b)  ; u+002b plus sign
  (:comma #x002c)  ; u+002c comma
  (:minus #x002d)  ; u+002d hyphen-minus
  (:period #x002e)  ; u+002e full stop
  (:slash #x002f)  ; u+002f solidus
  (:0 #x0030)  ; u+0030 digit zero
  (:1 #x0031)  ; u+0031 digit one
  (:2 #x0032)  ; u+0032 digit two
  (:3 #x0033)  ; u+0033 digit three
  (:4 #x0034)  ; u+0034 digit four
  (:5 #x0035)  ; u+0035 digit five
  (:6 #x0036)  ; u+0036 digit six
  (:7 #x0037)  ; u+0037 digit seven
  (:8 #x0038)  ; u+0038 digit eight
  (:9 #x0039)  ; u+0039 digit nine
  (:colon #x003a)  ; u+003a colon
  (:semicolon #x003b)  ; u+003b semicolon
  (:less #x003c)  ; u+003c less-than sign
  (:equal #x003d)  ; u+003d equals sign
  (:greater #x003e)  ; u+003e greater-than sign
  (:question #x003f)  ; u+003f question mark
  (:at #x0040)  ; u+0040 commercial at
  (:capital-a #x0041)  ; u+0041 latin capital letter a
  (:capital-b #x0042)  ; u+0042 latin capital letter b
  (:capital-c #x0043)  ; u+0043 latin capital letter c
  (:capital-d #x0044)  ; u+0044 latin capital letter d
  (:capital-e #x0045)  ; u+0045 latin capital letter e
  (:capital-f #x0046)  ; u+0046 latin capital letter f
  (:capital-g #x0047)  ; u+0047 latin capital letter g
  (:capital-h #x0048)  ; u+0048 latin capital letter h
  (:capital-i #x0049)  ; u+0049 latin capital letter i
  (:capital-j #x004a)  ; u+004a latin capital letter j
  (:capital-k #x004b)  ; u+004b latin capital letter k
  (:capital-l #x004c)  ; u+004c latin capital letter l
  (:capital-m #x004d)  ; u+004d latin capital letter m
  (:capital-n #x004e)  ; u+004e latin capital letter n
  (:capital-o #x004f)  ; u+004f latin capital letter o
  (:capital-p #x0050)  ; u+0050 latin capital letter p
  (:capital-q #x0051)  ; u+0051 latin capital letter q
  (:capital-r #x0052)  ; u+0052 latin capital letter r
  (:capital-s #x0053)  ; u+0053 latin capital letter s
  (:capital-t #x0054)  ; u+0054 latin capital letter t
  (:capital-u #x0055)  ; u+0055 latin capital letter u
  (:capital-v #x0056)  ; u+0056 latin capital letter v
  (:capital-w #x0057)  ; u+0057 latin capital letter w
  (:capital-x #x0058)  ; u+0058 latin capital letter x
  (:capital-y #x0059)  ; u+0059 latin capital letter y
  (:capital-z #x005a)  ; u+005a latin capital letter z
  (:bracketleft #x005b)  ; u+005b left square bracket
  (:backslash #x005c)  ; u+005c reverse solidus
  (:bracketright #x005d)  ; u+005d right square bracket
  (:asciicircum #x005e)  ; u+005e circumflex accent
  (:underscore #x005f)  ; u+005f low line
  (:grave #x0060)  ; u+0060 grave accent
  (:a #x0061)  ; u+0061 latin small letter a
  (:b #x0062)  ; u+0062 latin small letter b
  (:c #x0063)  ; u+0063 latin small letter c
  (:d #x0064)  ; u+0064 latin small letter d
  (:e #x0065)  ; u+0065 latin small letter e
  (:f #x0066)  ; u+0066 latin small letter f
  (:g #x0067)  ; u+0067 latin small letter g
  (:h #x0068)  ; u+0068 latin small letter h
  (:i #x0069)  ; u+0069 latin small letter i
  (:j #x006a)  ; u+006a latin small letter j
  (:k #x006b)  ; u+006b latin small letter k
  (:l #x006c)  ; u+006c latin small letter l
  (:m #x006d)  ; u+006d latin small letter m
  (:n #x006e)  ; u+006e latin small letter n
  (:o #x006f)  ; u+006f latin small letter o
  (:p #x0070)  ; u+0070 latin small letter p
  (:q #x0071)  ; u+0071 latin small letter q
  (:r #x0072)  ; u+0072 latin small letter r
  (:s #x0073)  ; u+0073 latin small letter s
  (:t #x0074)  ; u+0074 latin small letter t
  (:u #x0075)  ; u+0075 latin small letter u
  (:v #x0076)  ; u+0076 latin small letter v
  (:w #x0077)  ; u+0077 latin small letter w
  (:x #x0078)  ; u+0078 latin small letter x
  (:y #x0079)  ; u+0079 latin small letter y
  (:z #x007a)  ; u+007a latin small letter z
  (:braceleft #x007b)  ; u+007b left curly bracket
  (:bar #x007c)  ; u+007c vertical line
  (:braceright #x007d)  ; u+007d right curly bracket
  (:asciitilde #x007e)  ; u+007e tilde

  (:nobreakspace #x00a0)  ; u+00a0 no-break space
  (:exclamdown #x00a1)  ; u+00a1 inverted exclamation mark
  (:cent #x00a2)  ; u+00a2 cent sign
  (:sterling #x00a3)  ; u+00a3 pound sign
  (:currency #x00a4)  ; u+00a4 currency sign
  (:yen #x00a5)  ; u+00a5 yen sign
  (:brokenbar #x00a6)  ; u+00a6 broken bar
  (:section #x00a7)  ; u+00a7 section sign
  (:diaeresis #x00a8)  ; u+00a8 diaeresis
  (:copyright #x00a9)  ; u+00a9 copyright sign
  (:ordfeminine #x00aa)  ; u+00aa feminine ordinal indicator
  (:guillemotleft #x00ab)  ; u+00ab left-pointing double angle quotation mark
  (:notsign #x00ac)  ; u+00ac not sign
  (:hyphen #x00ad)  ; u+00ad soft hyphen
  (:registered #x00ae)  ; u+00ae registered sign
  (:macron #x00af)  ; u+00af macron
  (:degree #x00b0)  ; u+00b0 degree sign
  (:plusminus #x00b1)  ; u+00b1 plus-minus sign
  (:twosuperior #x00b2)  ; u+00b2 superscript two
  (:threesuperior #x00b3)  ; u+00b3 superscript three
  (:acute #x00b4)  ; u+00b4 acute accent
  (:mu #x00b5)  ; u+00b5 micro sign
  (:paragraph #x00b6)  ; u+00b6 pilcrow sign
  (:periodcentered #x00b7)  ; u+00b7 middle dot
  (:cedilla #x00b8)  ; u+00b8 cedilla
  (:onesuperior #x00b9)  ; u+00b9 superscript one
  (:masculine #x00ba)  ; u+00ba masculine ordinal indicator
  (:guillemotright #x00bb)  ; u+00bb right-pointing double angle quotation mark
  (:onequarter #x00bc)  ; u+00bc vulgar fraction one quarter
  (:onehalf #x00bd)  ; u+00bd vulgar fraction one half
  (:threequarters #x00be)  ; u+00be vulgar fraction three quarters
  (:questiondown #x00bf)  ; u+00bf inverted question mark
  (:capital-agrave #x00c0)  ; u+00c0 latin capital letter a with grave
  (:capital-aacute #x00c1)  ; u+00c1 latin capital letter a with acute
  (:capital-acircumflex #x00c2)  ; u+00c2 latin capital letter a with circumflex
  (:capital-atilde #x00c3)  ; u+00c3 latin capital letter a with tilde
  (:capital-adiaeresis #x00c4)  ; u+00c4 latin capital letter a with diaeresis
  (:capital-aring #x00c5)  ; u+00c5 latin capital letter a with ring above
  (:capital-ae #x00c6)  ; u+00c6 latin capital letter ae
  (:capital-ccedilla #x00c7)  ; u+00c7 latin capital letter c with cedilla
  (:capital-egrave #x00c8)  ; u+00c8 latin capital letter e with grave
  (:capital-eacute #x00c9)  ; u+00c9 latin capital letter e with acute
  (:capital-ecircumflex #x00ca)  ; u+00ca latin capital letter e with circumflex
  (:capital-ediaeresis #x00cb)  ; u+00cb latin capital letter e with diaeresis
  (:capital-igrave #x00cc)  ; u+00cc latin capital letter i with grave
  (:capital-iacute #x00cd)  ; u+00cd latin capital letter i with acute
  (:capital-icircumflex #x00ce)  ; u+00ce latin capital letter i with circumflex
  (:capital-idiaeresis #x00cf)  ; u+00cf latin capital letter i with diaeresis
  (:capital-eth #x00d0)  ; u+00d0 latin capital letter eth
  (:capital-ntilde #x00d1)  ; u+00d1 latin capital letter n with tilde
  (:capital-ograve #x00d2)  ; u+00d2 latin capital letter o with grave
  (:capital-oacute #x00d3)  ; u+00d3 latin capital letter o with acute
  (:capital-ocircumflex #x00d4)  ; u+00d4 latin capital letter o with circumflex
  (:capital-otilde #x00d5)  ; u+00d5 latin capital letter o with tilde
  (:capital-odiaeresis #x00d6)  ; u+00d6 latin capital letter o with diaeresis
  (:multiply #x00d7)  ; u+00d7 multiplication sign
  (:capital-oslash #x00d8)  ; u+00d8 latin capital letter o with stroke
  (:capital-ugrave #x00d9)  ; u+00d9 latin capital letter u with grave
  (:capital-uacute #x00da)  ; u+00da latin capital letter u with acute
  (:capital-ucircumflex #x00db)  ; u+00db latin capital letter u with circumflex
  (:capital-udiaeresis #x00dc)  ; u+00dc latin capital letter u with diaeresis
  (:capital-yacute #x00dd)  ; u+00dd latin capital letter y with acute
  (:capital-thorn #x00de)  ; u+00de latin capital letter thorn
  (:ssharp #x00df)  ; u+00df latin small letter sharp s
  (:agrave #x00e0)  ; u+00e0 latin small letter a with grave
  (:aacute #x00e1)  ; u+00e1 latin small letter a with acute
  (:acircumflex #x00e2)  ; u+00e2 latin small letter a with circumflex
  (:atilde #x00e3)  ; u+00e3 latin small letter a with tilde
  (:adiaeresis #x00e4)  ; u+00e4 latin small letter a with diaeresis
  (:aring #x00e5)  ; u+00e5 latin small letter a with ring above
  (:ae #x00e6)  ; u+00e6 latin small letter ae
  (:ccedilla #x00e7)  ; u+00e7 latin small letter c with cedilla
  (:egrave #x00e8)  ; u+00e8 latin small letter e with grave
  (:eacute #x00e9)  ; u+00e9 latin small letter e with acute
  (:ecircumflex #x00ea)  ; u+00ea latin small letter e with circumflex
  (:ediaeresis #x00eb)  ; u+00eb latin small letter e with diaeresis
  (:igrave #x00ec)  ; u+00ec latin small letter i with grave
  (:iacute #x00ed)  ; u+00ed latin small letter i with acute
  (:icircumflex #x00ee)  ; u+00ee latin small letter i with circumflex
  (:idiaeresis #x00ef)  ; u+00ef latin small letter i with diaeresis
  (:eth #x00f0)  ; u+00f0 latin small letter eth
  (:ntilde #x00f1)  ; u+00f1 latin small letter n with tilde
  (:ograve #x00f2)  ; u+00f2 latin small letter o with grave
  (:oacute #x00f3)  ; u+00f3 latin small letter o with acute
  (:ocircumflex #x00f4)  ; u+00f4 latin small letter o with circumflex
  (:otilde #x00f5)  ; u+00f5 latin small letter o with tilde
  (:odiaeresis #x00f6)  ; u+00f6 latin small letter o with diaeresis
  (:division #x00f7)  ; u+00f7 division sign
  (:oslash #x00f8)  ; u+00f8 latin small letter o with stroke
  (:ooblique #x00f8)  ; u+00f8 latin small letter o with stroke
  (:ugrave #x00f9)  ; u+00f9 latin small letter u with grave
  (:uacute #x00fa)  ; u+00fa latin small letter u with acute
  (:ucircumflex #x00fb)  ; u+00fb latin small letter u with circumflex
  (:udiaeresis #x00fc)  ; u+00fc latin small letter u with diaeresis
  (:yacute #x00fd)  ; u+00fd latin small letter y with acute
  (:thorn #x00fe)  ; u+00fe latin small letter thorn
  (:ydiaeresis #x00ff)  ; u+00ff latin small letter y with diaeresis
  


  ;; latin 2
  ;; byte 3 = 1


  
  (:capital-aogonek #x01a1)  ; u+0104 latin capital letter a with ogonek
  (:breve #x01a2)  ; u+02d8 breve
  (:capital-lstroke #x01a3)  ; u+0141 latin capital letter l with stroke
  (:capital-lcaron #x01a5)  ; u+013d latin capital letter l with caron
  (:capital-sacute #x01a6)  ; u+015a latin capital letter s with acute
  (:capital-scaron #x01a9)  ; u+0160 latin capital letter s with caron
  (:capital-scedilla #x01aa)  ; u+015e latin capital letter s with cedilla
  (:capital-tcaron #x01ab)  ; u+0164 latin capital letter t with caron
  (:capital-zacute #x01ac)  ; u+0179 latin capital letter z with acute
  (:capital-zcaron #x01ae)  ; u+017d latin capital letter z with caron
  (:capital-zabovedot #x01af)  ; u+017b latin capital letter z with dot above
  (:aogonek #x01b1)  ; u+0105 latin small letter a with ogonek
  (:ogonek #x01b2)  ; u+02db ogonek
  (:lstroke #x01b3)  ; u+0142 latin small letter l with stroke
  (:lcaron #x01b5)  ; u+013e latin small letter l with caron
  (:sacute #x01b6)  ; u+015b latin small letter s with acute
  (:caron #x01b7)  ; u+02c7 caron
  (:scaron #x01b9)  ; u+0161 latin small letter s with caron
  (:scedilla #x01ba)  ; u+015f latin small letter s with cedilla
  (:tcaron #x01bb)  ; u+0165 latin small letter t with caron
  (:zacute #x01bc)  ; u+017a latin small letter z with acute
  (:doubleacute #x01bd)  ; u+02dd double acute accent
  (:zcaron #x01be)  ; u+017e latin small letter z with caron
  (:zabovedot #x01bf)  ; u+017c latin small letter z with dot above
  (:capital-racute #x01c0)  ; u+0154 latin capital letter r with acute
  (:capital-abreve #x01c3)  ; u+0102 latin capital letter a with breve
  (:capital-lacute #x01c5)  ; u+0139 latin capital letter l with acute
  (:capital-cacute #x01c6)  ; u+0106 latin capital letter c with acute
  (:capital-ccaron #x01c8)  ; u+010c latin capital letter c with caron
  (:capital-eogonek #x01ca)  ; u+0118 latin capital letter e with ogonek
  (:capital-ecaron #x01cc)  ; u+011a latin capital letter e with caron
  (:capital-dcaron #x01cf)  ; u+010e latin capital letter d with caron
  (:capital-dstroke #x01d0)  ; u+0110 latin capital letter d with stroke
  (:capital-nacute #x01d1)  ; u+0143 latin capital letter n with acute
  (:capital-ncaron #x01d2)  ; u+0147 latin capital letter n with caron
  (:capital-odoubleacute #x01d5)  ; u+0150 latin capital letter o with double acute
  (:capital-rcaron #x01d8)  ; u+0158 latin capital letter r with caron
  (:capital-uring #x01d9)  ; u+016e latin capital letter u with ring above
  (:capital-udoubleacute #x01db)  ; u+0170 latin capital letter u with double acute
  (:capital-tcedilla #x01de)  ; u+0162 latin capital letter t with cedilla
  (:racute #x01e0)  ; u+0155 latin small letter r with acute
  (:abreve #x01e3)  ; u+0103 latin small letter a with breve
  (:lacute #x01e5)  ; u+013a latin small letter l with acute
  (:cacute #x01e6)  ; u+0107 latin small letter c with acute
  (:ccaron #x01e8)  ; u+010d latin small letter c with caron
  (:eogonek #x01ea)  ; u+0119 latin small letter e with ogonek
  (:ecaron #x01ec)  ; u+011b latin small letter e with caron
  (:dcaron #x01ef)  ; u+010f latin small letter d with caron
  (:dstroke #x01f0)  ; u+0111 latin small letter d with stroke
  (:nacute #x01f1)  ; u+0144 latin small letter n with acute
  (:ncaron #x01f2)  ; u+0148 latin small letter n with caron
  (:odoubleacute #x01f5)  ; u+0151 latin small letter o with double acute
  (:udoubleacute #x01fb)  ; u+0171 latin small letter u with double acute
  (:rcaron #x01f8)  ; u+0159 latin small letter r with caron
  (:uring #x01f9)  ; u+016f latin small letter u with ring above
  (:tcedilla #x01fe)  ; u+0163 latin small letter t with cedilla
  (:abovedot #x01ff)  ; u+02d9 dot above
  


  ;; latin 3
  ;; byte 3 = 2


  
  (:capital-hstroke #x02a1)  ; u+0126 latin capital letter h with stroke
  (:capital-hcircumflex #x02a6)  ; u+0124 latin capital letter h with circumflex
  (:capital-iabovedot #x02a9)  ; u+0130 latin capital letter i with dot above
  (:capital-gbreve #x02ab)  ; u+011e latin capital letter g with breve
  (:capital-jcircumflex #x02ac)  ; u+0134 latin capital letter j with circumflex
  (:hstroke #x02b1)  ; u+0127 latin small letter h with stroke
  (:hcircumflex #x02b6)  ; u+0125 latin small letter h with circumflex
  (:idotless #x02b9)  ; u+0131 latin small letter dotless i
  (:gbreve #x02bb)  ; u+011f latin small letter g with breve
  (:jcircumflex #x02bc)  ; u+0135 latin small letter j with circumflex
  (:capital-cabovedot #x02c5)  ; u+010a latin capital letter c with dot above
  (:capital-ccircumflex #x02c6)  ; u+0108 latin capital letter c with circumflex
  (:capital-gabovedot #x02d5)  ; u+0120 latin capital letter g with dot above
  (:capital-gcircumflex #x02d8)  ; u+011c latin capital letter g with circumflex
  (:capital-ubreve #x02dd)  ; u+016c latin capital letter u with breve
  (:capital-scircumflex #x02de)  ; u+015c latin capital letter s with circumflex
  (:cabovedot #x02e5)  ; u+010b latin small letter c with dot above
  (:ccircumflex #x02e6)  ; u+0109 latin small letter c with circumflex
  (:gabovedot #x02f5)  ; u+0121 latin small letter g with dot above
  (:gcircumflex #x02f8)  ; u+011d latin small letter g with circumflex
  (:ubreve #x02fd)  ; u+016d latin small letter u with breve
  (:scircumflex #x02fe)  ; u+015d latin small letter s with circumflex
  



  ;; latin 4
  ;; byte 3 = 3


  
  (:kra #x03a2)  ; u+0138 latin small letter kra
  (:capital-rcedilla #x03a3)  ; u+0156 latin capital letter r with cedilla
  (:capital-itilde #x03a5)  ; u+0128 latin capital letter i with tilde
  (:capital-lcedilla #x03a6)  ; u+013b latin capital letter l with cedilla
  (:capital-emacron #x03aa)  ; u+0112 latin capital letter e with macron
  (:capital-gcedilla #x03ab)  ; u+0122 latin capital letter g with cedilla
  (:capital-tslash #x03ac)  ; u+0166 latin capital letter t with stroke
  (:rcedilla #x03b3)  ; u+0157 latin small letter r with cedilla
  (:itilde #x03b5)  ; u+0129 latin small letter i with tilde
  (:lcedilla #x03b6)  ; u+013c latin small letter l with cedilla
  (:emacron #x03ba)  ; u+0113 latin small letter e with macron
  (:gcedilla #x03bb)  ; u+0123 latin small letter g with cedilla
  (:tslash #x03bc)  ; u+0167 latin small letter t with stroke
  (:capital-eng #x03bd)  ; u+014a latin capital letter eng
  (:eng #x03bf)  ; u+014b latin small letter eng
  (:capital-amacron #x03c0)  ; u+0100 latin capital letter a with macron
  (:capital-iogonek #x03c7)  ; u+012e latin capital letter i with ogonek
  (:capital-eabovedot #x03cc)  ; u+0116 latin capital letter e with dot above
  (:capital-imacron #x03cf)  ; u+012a latin capital letter i with macron
  (:capital-ncedilla #x03d1)  ; u+0145 latin capital letter n with cedilla
  (:capital-omacron #x03d2)  ; u+014c latin capital letter o with macron
  (:capital-kcedilla #x03d3)  ; u+0136 latin capital letter k with cedilla
  (:capital-uogonek #x03d9)  ; u+0172 latin capital letter u with ogonek
  (:capital-utilde #x03dd)  ; u+0168 latin capital letter u with tilde
  (:capital-umacron #x03de)  ; u+016a latin capital letter u with macron
  (:amacron #x03e0)  ; u+0101 latin small letter a with macron
  (:iogonek #x03e7)  ; u+012f latin small letter i with ogonek
  (:eabovedot #x03ec)  ; u+0117 latin small letter e with dot above
  (:imacron #x03ef)  ; u+012b latin small letter i with macron
  (:ncedilla #x03f1)  ; u+0146 latin small letter n with cedilla
  (:omacron #x03f2)  ; u+014d latin small letter o with macron
  (:kcedilla #x03f3)  ; u+0137 latin small letter k with cedilla
  (:uogonek #x03f9)  ; u+0173 latin small letter u with ogonek
  (:utilde #x03fd)  ; u+0169 latin small letter u with tilde
  (:umacron #x03fe)  ; u+016b latin small letter u with macron
  


  ;; latin 8

  
  (:capital-babovedot #x1001e02)  ; u+1e02 latin capital letter b with dot above
  (:babovedot #x1001e03)  ; u+1e03 latin small letter b with dot above
  (:capital-dabovedot #x1001e0a)  ; u+1e0a latin capital letter d with dot above
  (:capital-wgrave #x1001e80)  ; u+1e80 latin capital letter w with grave
  (:capital-wacute #x1001e82)  ; u+1e82 latin capital letter w with acute
  (:dabovedot #x1001e0b)  ; u+1e0b latin small letter d with dot above
  (:capital-ygrave #x1001ef2)  ; u+1ef2 latin capital letter y with grave
  (:capital-fabovedot #x1001e1e)  ; u+1e1e latin capital letter f with dot above
  (:fabovedot #x1001e1f)  ; u+1e1f latin small letter f with dot above
  (:capital-mabovedot #x1001e40)  ; u+1e40 latin capital letter m with dot above
  (:mabovedot #x1001e41)  ; u+1e41 latin small letter m with dot above
  (:capital-pabovedot #x1001e56)  ; u+1e56 latin capital letter p with dot above
  (:wgrave #x1001e81)  ; u+1e81 latin small letter w with grave
  (:pabovedot #x1001e57)  ; u+1e57 latin small letter p with dot above
  (:wacute #x1001e83)  ; u+1e83 latin small letter w with acute
  (:capital-sabovedot #x1001e60)  ; u+1e60 latin capital letter s with dot above
  (:ygrave #x1001ef3)  ; u+1ef3 latin small letter y with grave
  (:capital-wdiaeresis #x1001e84)  ; u+1e84 latin capital letter w with diaeresis
  (:wdiaeresis #x1001e85)  ; u+1e85 latin small letter w with diaeresis
  (:sabovedot #x1001e61)  ; u+1e61 latin small letter s with dot above
  (:capital-wcircumflex #x1000174)  ; u+0174 latin capital letter w with circumflex
  (:capital-tabovedot #x1001e6a)  ; u+1e6a latin capital letter t with dot above
  (:capital-ycircumflex #x1000176)  ; u+0176 latin capital letter y with circumflex
  (:wcircumflex #x1000175)  ; u+0175 latin small letter w with circumflex
  (:tabovedot #x1001e6b)  ; u+1e6b latin small letter t with dot above
  (:ycircumflex #x1000177)  ; u+0177 latin small letter y with circumflex
  


  ;; latin 9
  ;; byte 3 = #x13


  
  (:capital-oe #x13bc)  ; u+0152 latin capital ligature oe
  (:oe #x13bd)  ; u+0153 latin small ligature oe
  (:capital-ydiaeresis #x13be)  ; u+0178 latin capital letter y with diaeresis
  


  ;; katakana
  ;; byte 3 = 4


  
  (:overline #x047e)  ; u+203e overline
  (:kana-fullstop #x04a1)  ; u+3002 ideographic full stop
  (:kana-openingbracket #x04a2)  ; u+300c left corner bracket
  (:kana-closingbracket #x04a3)  ; u+300d right corner bracket
  (:kana-comma #x04a4)  ; u+3001 ideographic comma
  (:kana-conjunctive #x04a5)  ; u+30fb katakana middle dot
  (:kana-wo #x04a6)  ; u+30f2 katakana letter wo
  (:kana-a #x04a7)  ; u+30a1 katakana letter small a
  (:kana-i #x04a8)  ; u+30a3 katakana letter small i
  (:kana-u #x04a9)  ; u+30a5 katakana letter small u
  (:kana-e #x04aa)  ; u+30a7 katakana letter small e
  (:kana-o #x04ab)  ; u+30a9 katakana letter small o
  (:kana-ya #x04ac)  ; u+30e3 katakana letter small ya
  (:kana-yu #x04ad)  ; u+30e5 katakana letter small yu
  (:kana-yo #x04ae)  ; u+30e7 katakana letter small yo
  (:kana-tsu #x04af)  ; u+30c3 katakana letter small tu
  (:prolongedsound #x04b0)  ; u+30fc katakana-hiragana prolonged sound mark
  (:capital-kana-a #x04b1)  ; u+30a2 katakana letter a
  (:capital-kana-i #x04b2)  ; u+30a4 katakana letter i
  (:capital-kana-u #x04b3)  ; u+30a6 katakana letter u
  (:capital-kana-e #x04b4)  ; u+30a8 katakana letter e
  (:capital-kana-o #x04b5)  ; u+30aa katakana letter o
  (:kana-ka #x04b6)  ; u+30ab katakana letter ka
  (:kana-ki #x04b7)  ; u+30ad katakana letter ki
  (:kana-ku #x04b8)  ; u+30af katakana letter ku
  (:kana-ke #x04b9)  ; u+30b1 katakana letter ke
  (:kana-ko #x04ba)  ; u+30b3 katakana letter ko
  (:kana-sa #x04bb)  ; u+30b5 katakana letter sa
  (:kana-shi #x04bc)  ; u+30b7 katakana letter si
  (:kana-su #x04bd)  ; u+30b9 katakana letter su
  (:kana-se #x04be)  ; u+30bb katakana letter se
  (:kana-so #x04bf)  ; u+30bd katakana letter so
  (:kana-ta #x04c0)  ; u+30bf katakana letter ta
  (:kana-chi #x04c1)  ; u+30c1 katakana letter ti
  (:capital-kana-tsu #x04c2)  ; u+30c4 katakana letter tu
  (:kana-te #x04c3)  ; u+30c6 katakana letter te
  (:kana-to #x04c4)  ; u+30c8 katakana letter to
  (:kana-na #x04c5)  ; u+30ca katakana letter na
  (:kana-ni #x04c6)  ; u+30cb katakana letter ni
  (:kana-nu #x04c7)  ; u+30cc katakana letter nu
  (:kana-ne #x04c8)  ; u+30cd katakana letter ne
  (:kana-no #x04c9)  ; u+30ce katakana letter no
  (:kana-ha #x04ca)  ; u+30cf katakana letter ha
  (:kana-hi #x04cb)  ; u+30d2 katakana letter hi
  (:kana-fu #x04cc)  ; u+30d5 katakana letter hu
  (:kana-he #x04cd)  ; u+30d8 katakana letter he
  (:kana-ho #x04ce)  ; u+30db katakana letter ho
  (:kana-ma #x04cf)  ; u+30de katakana letter ma
  (:kana-mi #x04d0)  ; u+30df katakana letter mi
  (:kana-mu #x04d1)  ; u+30e0 katakana letter mu
  (:kana-me #x04d2)  ; u+30e1 katakana letter me
  (:kana-mo #x04d3)  ; u+30e2 katakana letter mo
  (:capital-kana-ya #x04d4)  ; u+30e4 katakana letter ya
  (:capital-kana-yu #x04d5)  ; u+30e6 katakana letter yu
  (:capital-kana-yo #x04d6)  ; u+30e8 katakana letter yo
  (:kana-ra #x04d7)  ; u+30e9 katakana letter ra
  (:kana-ri #x04d8)  ; u+30ea katakana letter ri
  (:kana-ru #x04d9)  ; u+30eb katakana letter ru
  (:kana-re #x04da)  ; u+30ec katakana letter re
  (:kana-ro #x04db)  ; u+30ed katakana letter ro
  (:kana-wa #x04dc)  ; u+30ef katakana letter wa
  (:kana-n #x04dd)  ; u+30f3 katakana letter n
  (:voicedsound #x04de)  ; u+309b katakana-hiragana voiced sound mark
  (:semivoicedsound #x04df)  ; u+309c katakana-hiragana semi-voiced sound mark
  (:kana-switch #xff7e)  ; alias for mode-switch
  


  ;; arabic
  ;; byte 3 = 5


  
  (:farsi-0 #x10006f0)  ; u+06f0 extended arabic-indic digit zero
  (:farsi-1 #x10006f1)  ; u+06f1 extended arabic-indic digit one
  (:farsi-2 #x10006f2)  ; u+06f2 extended arabic-indic digit two
  (:farsi-3 #x10006f3)  ; u+06f3 extended arabic-indic digit three
  (:farsi-4 #x10006f4)  ; u+06f4 extended arabic-indic digit four
  (:farsi-5 #x10006f5)  ; u+06f5 extended arabic-indic digit five
  (:farsi-6 #x10006f6)  ; u+06f6 extended arabic-indic digit six
  (:farsi-7 #x10006f7)  ; u+06f7 extended arabic-indic digit seven
  (:farsi-8 #x10006f8)  ; u+06f8 extended arabic-indic digit eight
  (:farsi-9 #x10006f9)  ; u+06f9 extended arabic-indic digit nine
  (:arabic-percent #x100066a)  ; u+066a arabic percent sign
  (:arabic-superscript-alef #x1000670)  ; u+0670 arabic letter superscript alef
  (:arabic-tteh #x1000679)  ; u+0679 arabic letter tteh
  (:arabic-peh #x100067e)  ; u+067e arabic letter peh
  (:arabic-tcheh #x1000686)  ; u+0686 arabic letter tcheh
  (:arabic-ddal #x1000688)  ; u+0688 arabic letter ddal
  (:arabic-rreh #x1000691)  ; u+0691 arabic letter rreh
  (:arabic-comma #x05ac)  ; u+060c arabic comma
  (:arabic-fullstop #x10006d4)  ; u+06d4 arabic full stop
  (:arabic-0 #x1000660)  ; u+0660 arabic-indic digit zero
  (:arabic-1 #x1000661)  ; u+0661 arabic-indic digit one
  (:arabic-2 #x1000662)  ; u+0662 arabic-indic digit two
  (:arabic-3 #x1000663)  ; u+0663 arabic-indic digit three
  (:arabic-4 #x1000664)  ; u+0664 arabic-indic digit four
  (:arabic-5 #x1000665)  ; u+0665 arabic-indic digit five
  (:arabic-6 #x1000666)  ; u+0666 arabic-indic digit six
  (:arabic-7 #x1000667)  ; u+0667 arabic-indic digit seven
  (:arabic-8 #x1000668)  ; u+0668 arabic-indic digit eight
  (:arabic-9 #x1000669)  ; u+0669 arabic-indic digit nine
  (:arabic-semicolon #x05bb)  ; u+061b arabic semicolon
  (:arabic-question-mark #x05bf)  ; u+061f arabic question mark
  (:arabic-hamza #x05c1)  ; u+0621 arabic letter hamza
  (:arabic-maddaonalef #x05c2)  ; u+0622 arabic letter alef with madda above
  (:arabic-hamzaonalef #x05c3)  ; u+0623 arabic letter alef with hamza above
  (:arabic-hamzaonwaw #x05c4)  ; u+0624 arabic letter waw with hamza above
  (:arabic-hamzaunderalef #x05c5)  ; u+0625 arabic letter alef with hamza below
  (:arabic-hamzaonyeh #x05c6)  ; u+0626 arabic letter yeh with hamza above
  (:arabic-alef #x05c7)  ; u+0627 arabic letter alef
  (:arabic-beh #x05c8)  ; u+0628 arabic letter beh
  (:arabic-tehmarbuta #x05c9)  ; u+0629 arabic letter teh marbuta
  (:arabic-teh #x05ca)  ; u+062a arabic letter teh
  (:arabic-theh #x05cb)  ; u+062b arabic letter theh
  (:arabic-jeem #x05cc)  ; u+062c arabic letter jeem
  (:arabic-hah #x05cd)  ; u+062d arabic letter hah
  (:arabic-khah #x05ce)  ; u+062e arabic letter khah
  (:arabic-dal #x05cf)  ; u+062f arabic letter dal
  (:arabic-thal #x05d0)  ; u+0630 arabic letter thal
  (:arabic-ra #x05d1)  ; u+0631 arabic letter reh
  (:arabic-zain #x05d2)  ; u+0632 arabic letter zain
  (:arabic-seen #x05d3)  ; u+0633 arabic letter seen
  (:arabic-sheen #x05d4)  ; u+0634 arabic letter sheen
  (:arabic-sad #x05d5)  ; u+0635 arabic letter sad
  (:arabic-dad #x05d6)  ; u+0636 arabic letter dad
  (:arabic-tah #x05d7)  ; u+0637 arabic letter tah
  (:arabic-zah #x05d8)  ; u+0638 arabic letter zah
  (:arabic-ain #x05d9)  ; u+0639 arabic letter ain
  (:arabic-ghain #x05da)  ; u+063a arabic letter ghain
  (:arabic-tatweel #x05e0)  ; u+0640 arabic tatweel
  (:arabic-feh #x05e1)  ; u+0641 arabic letter feh
  (:arabic-qaf #x05e2)  ; u+0642 arabic letter qaf
  (:arabic-kaf #x05e3)  ; u+0643 arabic letter kaf
  (:arabic-lam #x05e4)  ; u+0644 arabic letter lam
  (:arabic-meem #x05e5)  ; u+0645 arabic letter meem
  (:arabic-noon #x05e6)  ; u+0646 arabic letter noon
  (:arabic-ha #x05e7)  ; u+0647 arabic letter heh
  (:arabic-waw #x05e8)  ; u+0648 arabic letter waw
  (:arabic-alefmaksura #x05e9)  ; u+0649 arabic letter alef maksura
  (:arabic-yeh #x05ea)  ; u+064a arabic letter yeh
  (:arabic-fathatan #x05eb)  ; u+064b arabic fathatan
  (:arabic-dammatan #x05ec)  ; u+064c arabic dammatan
  (:arabic-kasratan #x05ed)  ; u+064d arabic kasratan
  (:arabic-fatha #x05ee)  ; u+064e arabic fatha
  (:arabic-damma #x05ef)  ; u+064f arabic damma
  (:arabic-kasra #x05f0)  ; u+0650 arabic kasra
  (:arabic-shadda #x05f1)  ; u+0651 arabic shadda
  (:arabic-sukun #x05f2)  ; u+0652 arabic sukun
  (:arabic-madda-above #x1000653)  ; u+0653 arabic maddah above
  (:arabic-hamza-above #x1000654)  ; u+0654 arabic hamza above
  (:arabic-hamza-below #x1000655)  ; u+0655 arabic hamza below
  (:arabic-jeh #x1000698)  ; u+0698 arabic letter jeh
  (:arabic-veh #x10006a4)  ; u+06a4 arabic letter veh
  (:arabic-keheh #x10006a9)  ; u+06a9 arabic letter keheh
  (:arabic-gaf #x10006af)  ; u+06af arabic letter gaf
  (:arabic-noon-ghunna #x10006ba)  ; u+06ba arabic letter noon ghunna
  (:arabic-heh-doachashmee #x10006be)  ; u+06be arabic letter heh doachashmee
  (:farsi-yeh #x10006cc)  ; u+06cc arabic letter farsi yeh
  (:arabic-farsi-yeh #x10006cc)  ; u+06cc arabic letter farsi yeh
  (:arabic-yeh-baree #x10006d2)  ; u+06d2 arabic letter yeh barree
  (:arabic-heh-goal #x10006c1)  ; u+06c1 arabic letter heh goal
  (:arabic-switch #xff7e)  ; alias for mode-switch
  


  ;; cyrillic
  ;; byte 3 = 6

  
  (:capital-cyrillic-ghe-bar #x1000492)  ; u+0492 cyrillic capital letter ghe with stroke
  (:cyrillic-ghe-bar #x1000493)  ; u+0493 cyrillic small letter ghe with stroke
  (:capital-cyrillic-zhe-descender #x1000496)  ; u+0496 cyrillic capital letter zhe with descender
  (:cyrillic-zhe-descender #x1000497)  ; u+0497 cyrillic small letter zhe with descender
  (:capital-cyrillic-ka-descender #x100049a)  ; u+049a cyrillic capital letter ka with descender
  (:cyrillic-ka-descender #x100049b)  ; u+049b cyrillic small letter ka with descender
  (:capital-cyrillic-ka-vertstroke #x100049c)  ; u+049c cyrillic capital letter ka with vertical stroke
  (:cyrillic-ka-vertstroke #x100049d)  ; u+049d cyrillic small letter ka with vertical stroke
  (:capital-cyrillic-en-descender #x10004a2)  ; u+04a2 cyrillic capital letter en with descender
  (:cyrillic-en-descender #x10004a3)  ; u+04a3 cyrillic small letter en with descender
  (:capital-cyrillic-u-straight #x10004ae)  ; u+04ae cyrillic capital letter straight u
  (:cyrillic-u-straight #x10004af)  ; u+04af cyrillic small letter straight u
  (:capital-cyrillic-u-straight-bar #x10004b0)  ; u+04b0 cyrillic capital letter straight u with stroke
  (:cyrillic-u-straight-bar #x10004b1)  ; u+04b1 cyrillic small letter straight u with stroke
  (:capital-cyrillic-ha-descender #x10004b2)  ; u+04b2 cyrillic capital letter ha with descender
  (:cyrillic-ha-descender #x10004b3)  ; u+04b3 cyrillic small letter ha with descender
  (:capital-cyrillic-che-descender #x10004b6)  ; u+04b6 cyrillic capital letter che with descender
  (:cyrillic-che-descender #x10004b7)  ; u+04b7 cyrillic small letter che with descender
  (:capital-cyrillic-che-vertstroke #x10004b8)  ; u+04b8 cyrillic capital letter che with vertical stroke
  (:cyrillic-che-vertstroke #x10004b9)  ; u+04b9 cyrillic small letter che with vertical stroke
  (:capital-cyrillic-shha #x10004ba)  ; u+04ba cyrillic capital letter shha
  (:cyrillic-shha #x10004bb)  ; u+04bb cyrillic small letter shha

  (:capital-cyrillic-schwa #x10004d8)  ; u+04d8 cyrillic capital letter schwa
  (:cyrillic-schwa #x10004d9)  ; u+04d9 cyrillic small letter schwa
  (:capital-cyrillic-i-macron #x10004e2)  ; u+04e2 cyrillic capital letter i with macron
  (:cyrillic-i-macron #x10004e3)  ; u+04e3 cyrillic small letter i with macron
  (:capital-cyrillic-o-bar #x10004e8)  ; u+04e8 cyrillic capital letter barred o
  (:cyrillic-o-bar #x10004e9)  ; u+04e9 cyrillic small letter barred o
  (:capital-cyrillic-u-macron #x10004ee)  ; u+04ee cyrillic capital letter u with macron
  (:cyrillic-u-macron #x10004ef)  ; u+04ef cyrillic small letter u with macron

  (:serbian-dje #x06a1)  ; u+0452 cyrillic small letter dje
  (:macedonia-gje #x06a2)  ; u+0453 cyrillic small letter gje
  (:cyrillic-io #x06a3)  ; u+0451 cyrillic small letter io
  (:ukrainian-ie #x06a4)  ; u+0454 cyrillic small letter ukrainian ie
  (:macedonia-dse #x06a5)  ; u+0455 cyrillic small letter dze
  (:ukrainian-i #x06a6)  ; u+0456 cyrillic small letter byelorussian-ukrainian i
  (:ukrainian-yi #x06a7)  ; u+0457 cyrillic small letter yi
  (:cyrillic-je #x06a8)  ; u+0458 cyrillic small letter je
  (:cyrillic-lje #x06a9)  ; u+0459 cyrillic small letter lje
  (:cyrillic-nje #x06aa)  ; u+045a cyrillic small letter nje
  (:serbian-tshe #x06ab)  ; u+045b cyrillic small letter tshe
  (:macedonia-kje #x06ac)  ; u+045c cyrillic small letter kje
  (:ukrainian-ghe-with-upturn #x06ad)  ; u+0491 cyrillic small letter ghe with upturn
  (:byelorussian-shortu #x06ae)  ; u+045e cyrillic small letter short u
  (:cyrillic-dzhe #x06af)  ; u+045f cyrillic small letter dzhe
  (:numerosign #x06b0)  ; u+2116 numero sign
  (:capital-serbian-dje #x06b1)  ; u+0402 cyrillic capital letter dje
  (:capital-macedonia-gje #x06b2)  ; u+0403 cyrillic capital letter gje
  (:capital-cyrillic-io #x06b3)  ; u+0401 cyrillic capital letter io
  (:capital-ukrainian-ie #x06b4)  ; u+0404 cyrillic capital letter ukrainian ie
  (:capital-macedonia-dse #x06b5)  ; u+0405 cyrillic capital letter dze
  (:capital-ukrainian-i #x06b6)  ; u+0406 cyrillic capital letter byelorussian-ukrainian i
  (:capital-ukrainian-yi #x06b7)  ; u+0407 cyrillic capital letter yi
  (:capital-cyrillic-je #x06b8)  ; u+0408 cyrillic capital letter je
  (:capital-cyrillic-lje #x06b9)  ; u+0409 cyrillic capital letter lje
  (:capital-cyrillic-nje #x06ba)  ; u+040a cyrillic capital letter nje
  (:capital-serbian-tshe #x06bb)  ; u+040b cyrillic capital letter tshe
  (:capital-macedonia-kje #x06bc)  ; u+040c cyrillic capital letter kje
  (:capital-ukrainian-ghe-with-upturn #x06bd)  ; u+0490 cyrillic capital letter ghe with upturn
  (:capital-byelorussian-shortu #x06be)  ; u+040e cyrillic capital letter short u
  (:capital-cyrillic-dzhe #x06bf)  ; u+040f cyrillic capital letter dzhe
  (:cyrillic-yu #x06c0)  ; u+044e cyrillic small letter yu
  (:cyrillic-a #x06c1)  ; u+0430 cyrillic small letter a
  (:cyrillic-be #x06c2)  ; u+0431 cyrillic small letter be
  (:cyrillic-tse #x06c3)  ; u+0446 cyrillic small letter tse
  (:cyrillic-de #x06c4)  ; u+0434 cyrillic small letter de
  (:cyrillic-ie #x06c5)  ; u+0435 cyrillic small letter ie
  (:cyrillic-ef #x06c6)  ; u+0444 cyrillic small letter ef
  (:cyrillic-ghe #x06c7)  ; u+0433 cyrillic small letter ghe
  (:cyrillic-ha #x06c8)  ; u+0445 cyrillic small letter ha
  (:cyrillic-i #x06c9)  ; u+0438 cyrillic small letter i
  (:cyrillic-shorti #x06ca)  ; u+0439 cyrillic small letter short i
  (:cyrillic-ka #x06cb)  ; u+043a cyrillic small letter ka
  (:cyrillic-el #x06cc)  ; u+043b cyrillic small letter el
  (:cyrillic-em #x06cd)  ; u+043c cyrillic small letter em
  (:cyrillic-en #x06ce)  ; u+043d cyrillic small letter en
  (:cyrillic-o #x06cf)  ; u+043e cyrillic small letter o
  (:cyrillic-pe #x06d0)  ; u+043f cyrillic small letter pe
  (:cyrillic-ya #x06d1)  ; u+044f cyrillic small letter ya
  (:cyrillic-er #x06d2)  ; u+0440 cyrillic small letter er
  (:cyrillic-es #x06d3)  ; u+0441 cyrillic small letter es
  (:cyrillic-te #x06d4)  ; u+0442 cyrillic small letter te
  (:cyrillic-u #x06d5)  ; u+0443 cyrillic small letter u
  (:cyrillic-zhe #x06d6)  ; u+0436 cyrillic small letter zhe
  (:cyrillic-ve #x06d7)  ; u+0432 cyrillic small letter ve
  (:cyrillic-softsign #x06d8)  ; u+044c cyrillic small letter soft sign
  (:cyrillic-yeru #x06d9)  ; u+044b cyrillic small letter yeru
  (:cyrillic-ze #x06da)  ; u+0437 cyrillic small letter ze
  (:cyrillic-sha #x06db)  ; u+0448 cyrillic small letter sha
  (:cyrillic-e #x06dc)  ; u+044d cyrillic small letter e
  (:cyrillic-shcha #x06dd)  ; u+0449 cyrillic small letter shcha
  (:cyrillic-che #x06de)  ; u+0447 cyrillic small letter che
  (:cyrillic-hardsign #x06df)  ; u+044a cyrillic small letter hard sign
  (:capital-cyrillic-yu #x06e0)  ; u+042e cyrillic capital letter yu
  (:capital-cyrillic-a #x06e1)  ; u+0410 cyrillic capital letter a
  (:capital-cyrillic-be #x06e2)  ; u+0411 cyrillic capital letter be
  (:capital-cyrillic-tse #x06e3)  ; u+0426 cyrillic capital letter tse
  (:capital-cyrillic-de #x06e4)  ; u+0414 cyrillic capital letter de
  (:capital-cyrillic-ie #x06e5)  ; u+0415 cyrillic capital letter ie
  (:capital-cyrillic-ef #x06e6)  ; u+0424 cyrillic capital letter ef
  (:capital-cyrillic-ghe #x06e7)  ; u+0413 cyrillic capital letter ghe
  (:capital-cyrillic-ha #x06e8)  ; u+0425 cyrillic capital letter ha
  (:capital-cyrillic-i #x06e9)  ; u+0418 cyrillic capital letter i
  (:capital-cyrillic-shorti #x06ea)  ; u+0419 cyrillic capital letter short i
  (:capital-cyrillic-ka #x06eb)  ; u+041a cyrillic capital letter ka
  (:capital-cyrillic-el #x06ec)  ; u+041b cyrillic capital letter el
  (:capital-cyrillic-em #x06ed)  ; u+041c cyrillic capital letter em
  (:capital-cyrillic-en #x06ee)  ; u+041d cyrillic capital letter en
  (:capital-cyrillic-o #x06ef)  ; u+041e cyrillic capital letter o
  (:capital-cyrillic-pe #x06f0)  ; u+041f cyrillic capital letter pe
  (:capital-cyrillic-ya #x06f1)  ; u+042f cyrillic capital letter ya
  (:capital-cyrillic-er #x06f2)  ; u+0420 cyrillic capital letter er
  (:capital-cyrillic-es #x06f3)  ; u+0421 cyrillic capital letter es
  (:capital-cyrillic-te #x06f4)  ; u+0422 cyrillic capital letter te
  (:capital-cyrillic-u #x06f5)  ; u+0423 cyrillic capital letter u
  (:capital-cyrillic-zhe #x06f6)  ; u+0416 cyrillic capital letter zhe
  (:capital-cyrillic-ve #x06f7)  ; u+0412 cyrillic capital letter ve
  (:capital-cyrillic-softsign #x06f8)  ; u+042c cyrillic capital letter soft sign
  (:capital-cyrillic-yeru #x06f9)  ; u+042b cyrillic capital letter yeru
  (:capital-cyrillic-ze #x06fa)  ; u+0417 cyrillic capital letter ze
  (:capital-cyrillic-sha #x06fb)  ; u+0428 cyrillic capital letter sha
  (:capital-cyrillic-e #x06fc)  ; u+042d cyrillic capital letter e
  (:capital-cyrillic-shcha #x06fd)  ; u+0429 cyrillic capital letter shcha
  (:capital-cyrillic-che #x06fe)  ; u+0427 cyrillic capital letter che
  (:capital-cyrillic-hardsign #x06ff)  ; u+042a cyrillic capital letter hard sign
  


  ;; greek
  ;; (:based on an early draft of, and not quite identical to, iso/iec 8859-7)
  ;; byte 3 = 7


  
  (:capital-greek-alphaaccent #x07a1)  ; u+0386 greek capital letter alpha with tonos
  (:capital-greek-epsilonaccent #x07a2)  ; u+0388 greek capital letter epsilon with tonos
  (:capital-greek-etaaccent #x07a3)  ; u+0389 greek capital letter eta with tonos
  (:capital-greek-iotaaccent #x07a4)  ; u+038a greek capital letter iota with tonos
  (:capital-greek-iotadieresis #x07a5)  ; u+03aa greek capital letter iota with dialytika
  (:greek-iotadiaeresis #x07a5)  ; old typo
  (:capital-greek-omicronaccent #x07a7)  ; u+038c greek capital letter omicron with tonos
  (:capital-greek-upsilonaccent #x07a8)  ; u+038e greek capital letter upsilon with tonos
  (:capital-greek-upsilondieresis #x07a9)  ; u+03ab greek capital letter upsilon with dialytika
  (:capital-greek-omegaaccent #x07ab)  ; u+038f greek capital letter omega with tonos
  (:greek-accentdieresis #x07ae)  ; u+0385 greek dialytika tonos
  (:greek-horizbar #x07af)  ; u+2015 horizontal bar
  (:greek-alphaaccent #x07b1)  ; u+03ac greek small letter alpha with tonos
  (:greek-epsilonaccent #x07b2)  ; u+03ad greek small letter epsilon with tonos
  (:greek-etaaccent #x07b3)  ; u+03ae greek small letter eta with tonos
  (:greek-iotaaccent #x07b4)  ; u+03af greek small letter iota with tonos
  (:greek-iotadieresis #x07b5)  ; u+03ca greek small letter iota with dialytika
  (:greek-iotaaccentdieresis #x07b6)  ; u+0390 greek small letter iota with dialytika and tonos
  (:greek-omicronaccent #x07b7)  ; u+03cc greek small letter omicron with tonos
  (:greek-upsilonaccent #x07b8)  ; u+03cd greek small letter upsilon with tonos
  (:greek-upsilondieresis #x07b9)  ; u+03cb greek small letter upsilon with dialytika
  (:greek-upsilonaccentdieresis #x07ba)  ; u+03b0 greek small letter upsilon with dialytika and tonos
  (:greek-omegaaccent #x07bb)  ; u+03ce greek small letter omega with tonos
  (:capital-greek-alpha #x07c1)  ; u+0391 greek capital letter alpha
  (:capital-greek-beta #x07c2)  ; u+0392 greek capital letter beta
  (:capital-greek-gamma #x07c3)  ; u+0393 greek capital letter gamma
  (:capital-greek-delta #x07c4)  ; u+0394 greek capital letter delta
  (:capital-greek-epsilon #x07c5)  ; u+0395 greek capital letter epsilon
  (:capital-greek-zeta #x07c6)  ; u+0396 greek capital letter zeta
  (:capital-greek-eta #x07c7)  ; u+0397 greek capital letter eta
  (:capital-greek-theta #x07c8)  ; u+0398 greek capital letter theta
  (:capital-greek-iota #x07c9)  ; u+0399 greek capital letter iota
  (:capital-greek-kappa #x07ca)  ; u+039a greek capital letter kappa
  (:capital-greek-lamda #x07cb)  ; u+039b greek capital letter lamda
  (:capital-greek-lambda #x07cb)  ; u+039b greek capital letter lamda
  (:capital-greek-mu #x07cc)  ; u+039c greek capital letter mu
  (:capital-greek-nu #x07cd)  ; u+039d greek capital letter nu
  (:capital-greek-xi #x07ce)  ; u+039e greek capital letter xi
  (:capital-greek-omicron #x07cf)  ; u+039f greek capital letter omicron
  (:capital-greek-pi #x07d0)  ; u+03a0 greek capital letter pi
  (:capital-greek-rho #x07d1)  ; u+03a1 greek capital letter rho
  (:capital-greek-sigma #x07d2)  ; u+03a3 greek capital letter sigma
  (:capital-greek-tau #x07d4)  ; u+03a4 greek capital letter tau
  (:capital-greek-upsilon #x07d5)  ; u+03a5 greek capital letter upsilon
  (:capital-greek-phi #x07d6)  ; u+03a6 greek capital letter phi
  (:capital-greek-chi #x07d7)  ; u+03a7 greek capital letter chi
  (:capital-greek-psi #x07d8)  ; u+03a8 greek capital letter psi
  (:capital-greek-omega #x07d9)  ; u+03a9 greek capital letter omega
  (:greek-alpha #x07e1)  ; u+03b1 greek small letter alpha
  (:greek-beta #x07e2)  ; u+03b2 greek small letter beta
  (:greek-gamma #x07e3)  ; u+03b3 greek small letter gamma
  (:greek-delta #x07e4)  ; u+03b4 greek small letter delta
  (:greek-epsilon #x07e5)  ; u+03b5 greek small letter epsilon
  (:greek-zeta #x07e6)  ; u+03b6 greek small letter zeta
  (:greek-eta #x07e7)  ; u+03b7 greek small letter eta
  (:greek-theta #x07e8)  ; u+03b8 greek small letter theta
  (:greek-iota #x07e9)  ; u+03b9 greek small letter iota
  (:greek-kappa #x07ea)  ; u+03ba greek small letter kappa
  (:greek-lamda #x07eb)  ; u+03bb greek small letter lamda
  (:greek-lambda #x07eb)  ; u+03bb greek small letter lamda
  (:greek-mu #x07ec)  ; u+03bc greek small letter mu
  (:greek-nu #x07ed)  ; u+03bd greek small letter nu
  (:greek-xi #x07ee)  ; u+03be greek small letter xi
  (:greek-omicron #x07ef)  ; u+03bf greek small letter omicron
  (:greek-pi #x07f0)  ; u+03c0 greek small letter pi
  (:greek-rho #x07f1)  ; u+03c1 greek small letter rho
  (:greek-sigma #x07f2)  ; u+03c3 greek small letter sigma
  (:greek-finalsmallsigma #x07f3)  ; u+03c2 greek small letter final sigma
  (:greek-tau #x07f4)  ; u+03c4 greek small letter tau
  (:greek-upsilon #x07f5)  ; u+03c5 greek small letter upsilon
  (:greek-phi #x07f6)  ; u+03c6 greek small letter phi
  (:greek-chi #x07f7)  ; u+03c7 greek small letter chi
  (:greek-psi #x07f8)  ; u+03c8 greek small letter psi
  (:greek-omega #x07f9)  ; u+03c9 greek small letter omega
  (:greek-switch #xff7e)  ; alias for mode-switch
  


  ;; technical
  ;; (:from the dec vt330/vt420 technical character set, http://vt100.net/charsets/technical.html)
  ;; byte 3 = 8


  
  (:leftradical #x08a1)  ; u+23b7 radical symbol bottom
  (:topleftradical #x08a2)  ;(:u+250c box drawings light down and right)*/
  (:horizconnector #x08a3)  ;(:u+2500 box drawings light horizontal)*/
  (:topintegral #x08a4)  ; u+2320 top half integral
  (:botintegral #x08a5)  ; u+2321 bottom half integral
  (:vertconnector #x08a6)  ;(:u+2502 box drawings light vertical)*/
  (:topleftsqbracket #x08a7)  ; u+23a1 left square bracket upper corner
  (:botleftsqbracket #x08a8)  ; u+23a3 left square bracket lower corner
  (:toprightsqbracket #x08a9)  ; u+23a4 right square bracket upper corner
  (:botrightsqbracket #x08aa)  ; u+23a6 right square bracket lower corner
  (:topleftparens #x08ab)  ; u+239b left parenthesis upper hook
  (:botleftparens #x08ac)  ; u+239d left parenthesis lower hook
  (:toprightparens #x08ad)  ; u+239e right parenthesis upper hook
  (:botrightparens #x08ae)  ; u+23a0 right parenthesis lower hook
  (:leftmiddlecurlybrace #x08af)  ; u+23a8 left curly bracket middle piece
  (:rightmiddlecurlybrace #x08b0)  ; u+23ac right curly bracket middle piece
  (:topleftsummation #x08b1)
  (:botleftsummation #x08b2)
  (:topvertsummationconnector #x08b3)
  (:botvertsummationconnector #x08b4)
  (:toprightsummation #x08b5)
  (:botrightsummation #x08b6)
  (:rightmiddlesummation #x08b7)
  (:lessthanequal #x08bc)  ; u+2264 less-than or equal to
  (:notequal #x08bd)  ; u+2260 not equal to
  (:greaterthanequal #x08be)  ; u+2265 greater-than or equal to
  (:integral #x08bf)  ; u+222b integral
  (:therefore #x08c0)  ; u+2234 therefore
  (:variation #x08c1)  ; u+221d proportional to
  (:infinity #x08c2)  ; u+221e infinity
  (:nabla #x08c5)  ; u+2207 nabla
  (:approximate #x08c8)  ; u+223c tilde operator
  (:similarequal #x08c9)  ; u+2243 asymptotically equal to
  (:ifonlyif #x08cd)  ; u+21d4 left right double arrow
  (:implies #x08ce)  ; u+21d2 rightwards double arrow
  (:identical #x08cf)  ; u+2261 identical to
  (:radical #x08d6)  ; u+221a square root
  (:includedin #x08da)  ; u+2282 subset of
  (:includes #x08db)  ; u+2283 superset of
  (:intersection #x08dc)  ; u+2229 intersection
  (:union #x08dd)  ; u+222a union
  (:logicaland #x08de)  ; u+2227 logical and
  (:logicalor #x08df)  ; u+2228 logical or
  (:partialderivative #x08ef)  ; u+2202 partial differential
  (:function #x08f6)  ; u+0192 latin small letter f with hook
  (:leftarrow #x08fb)  ; u+2190 leftwards arrow
  (:uparrow #x08fc)  ; u+2191 upwards arrow
  (:rightarrow #x08fd)  ; u+2192 rightwards arrow
  (:downarrow #x08fe)  ; u+2193 downwards arrow
  


  ;; special
  ;; (:from the dec vt100 special graphics character set)
  ;; byte 3 = 9


  
  (:blank #x09df)
  (:soliddiamond #x09e0)  ; u+25c6 black diamond
  (:checkerboard #x09e1)  ; u+2592 medium shade
  (:ht #x09e2)  ; u+2409 symbol for horizontal tabulation
  (:ff #x09e3)  ; u+240c symbol for form feed
  (:cr #x09e4)  ; u+240d symbol for carriage return
  (:lf #x09e5)  ; u+240a symbol for line feed
  (:nl #x09e8)  ; u+2424 symbol for newline
  (:vt #x09e9)  ; u+240b symbol for vertical tabulation
  (:lowrightcorner #x09ea)  ; u+2518 box drawings light up and left
  (:uprightcorner #x09eb)  ; u+2510 box drawings light down and left
  (:upleftcorner #x09ec)  ; u+250c box drawings light down and right
  (:lowleftcorner #x09ed)  ; u+2514 box drawings light up and right
  (:crossinglines #x09ee)  ; u+253c box drawings light vertical and horizontal
  (:horizlinescan1 #x09ef)  ; u+23ba horizontal scan line-1
  (:horizlinescan3 #x09f0)  ; u+23bb horizontal scan line-3
  (:horizlinescan5 #x09f1)  ; u+2500 box drawings light horizontal
  (:horizlinescan7 #x09f2)  ; u+23bc horizontal scan line-7
  (:horizlinescan9 #x09f3)  ; u+23bd horizontal scan line-9
  (:leftt #x09f4)  ; u+251c box drawings light vertical and right
  (:rightt #x09f5)  ; u+2524 box drawings light vertical and left
  (:bott #x09f6)  ; u+2534 box drawings light up and horizontal
  (:topt #x09f7)  ; u+252c box drawings light down and horizontal
  (:vertbar #x09f8)  ; u+2502 box drawings light vertical
  


  ;; publishing
  ;; (:these are probably from a long forgotten dec publishing
  ;; font that once shipped with decwrite)
  ;; byte 3 = #x0a


  
  (:emspace #x0aa1)  ; u+2003 em space
  (:enspace #x0aa2)  ; u+2002 en space
  (:em3space #x0aa3)  ; u+2004 three-per-em space
  (:em4space #x0aa4)  ; u+2005 four-per-em space
  (:digitspace #x0aa5)  ; u+2007 figure space
  (:punctspace #x0aa6)  ; u+2008 punctuation space
  (:thinspace #x0aa7)  ; u+2009 thin space
  (:hairspace #x0aa8)  ; u+200a hair space
  (:emdash #x0aa9)  ; u+2014 em dash
  (:endash #x0aaa)  ; u+2013 en dash
  (:signifblank #x0aac)  ;(:u+2423 open box)*/
  (:ellipsis #x0aae)  ; u+2026 horizontal ellipsis
  (:doubbaselinedot #x0aaf)  ; u+2025 two dot leader
  (:onethird #x0ab0)  ; u+2153 vulgar fraction one third
  (:twothirds #x0ab1)  ; u+2154 vulgar fraction two thirds
  (:onefifth #x0ab2)  ; u+2155 vulgar fraction one fifth
  (:twofifths #x0ab3)  ; u+2156 vulgar fraction two fifths
  (:threefifths #x0ab4)  ; u+2157 vulgar fraction three fifths
  (:fourfifths #x0ab5)  ; u+2158 vulgar fraction four fifths
  (:onesixth #x0ab6)  ; u+2159 vulgar fraction one sixth
  (:fivesixths #x0ab7)  ; u+215a vulgar fraction five sixths
  (:careof #x0ab8)  ; u+2105 care of
  (:figdash #x0abb)  ; u+2012 figure dash
  (:leftanglebracket #x0abc)  ;(:u+27e8 mathematical left angle bracket)*/
  (:decimalpoint #x0abd)  ;(:u+002e full stop)*/
  (:rightanglebracket #x0abe)  ;(:u+27e9 mathematical right angle bracket)*/
  (:marker #x0abf)
  (:oneeighth #x0ac3)  ; u+215b vulgar fraction one eighth
  (:threeeighths #x0ac4)  ; u+215c vulgar fraction three eighths
  (:fiveeighths #x0ac5)  ; u+215d vulgar fraction five eighths
  (:seveneighths #x0ac6)  ; u+215e vulgar fraction seven eighths
  (:trademark #x0ac9)  ; u+2122 trade mark sign
  (:signaturemark #x0aca)  ;(:u+2613 saltire)*/
  (:trademarkincircle #x0acb)
  (:leftopentriangle #x0acc)  ;(:u+25c1 white left-pointing triangle)*/
  (:rightopentriangle #x0acd)  ;(:u+25b7 white right-pointing triangle)*/
  (:emopencircle #x0ace)  ;(:u+25cb white circle)*/
  (:emopenrectangle #x0acf)  ;(:u+25af white vertical rectangle)*/
  (:leftsinglequotemark #x0ad0)  ; u+2018 left single quotation mark
  (:rightsinglequotemark #x0ad1)  ; u+2019 right single quotation mark
  (:leftdoublequotemark #x0ad2)  ; u+201c left double quotation mark
  (:rightdoublequotemark #x0ad3)  ; u+201d right double quotation mark
  (:prescription #x0ad4)  ; u+211e prescription take
  (:minutes #x0ad6)  ; u+2032 prime
  (:seconds #x0ad7)  ; u+2033 double prime
  (:latincross #x0ad9)  ; u+271d latin cross
  (:hexagram #x0ada)
  (:filledrectbullet #x0adb)  ;(:u+25ac black rectangle)*/
  (:filledlefttribullet #x0adc)  ;(:u+25c0 black left-pointing triangle)*/
  (:filledrighttribullet #x0add)  ;(:u+25b6 black right-pointing triangle)*/
  (:emfilledcircle #x0ade)  ;(:u+25cf black circle)*/
  (:emfilledrect #x0adf)  ;(:u+25ae black vertical rectangle)*/
  (:enopencircbullet #x0ae0)  ;(:u+25e6 white bullet)*/
  (:enopensquarebullet #x0ae1)  ;(:u+25ab white small square)*/
  (:openrectbullet #x0ae2)  ;(:u+25ad white rectangle)*/
  (:opentribulletup #x0ae3)  ;(:u+25b3 white up-pointing triangle)*/
  (:opentribulletdown #x0ae4)  ;(:u+25bd white down-pointing triangle)*/
  (:openstar #x0ae5)  ;(:u+2606 white star)*/
  (:enfilledcircbullet #x0ae6)  ;(:u+2022 bullet)*/
  (:enfilledsqbullet #x0ae7)  ;(:u+25aa black small square)*/
  (:filledtribulletup #x0ae8)  ;(:u+25b2 black up-pointing triangle)*/
  (:filledtribulletdown #x0ae9)  ;(:u+25bc black down-pointing triangle)*/
  (:leftpointer #x0aea)  ;(:u+261c white left pointing index)*/
  (:rightpointer #x0aeb)  ;(:u+261e white right pointing index)*/
  (:club #x0aec)  ; u+2663 black club suit
  (:diamond #x0aed)  ; u+2666 black diamond suit
  (:heart #x0aee)  ; u+2665 black heart suit
  (:maltesecross #x0af0)  ; u+2720 maltese cross
  (:dagger #x0af1)  ; u+2020 dagger
  (:doubledagger #x0af2)  ; u+2021 double dagger
  (:checkmark #x0af3)  ; u+2713 check mark
  (:ballotcross #x0af4)  ; u+2717 ballot x
  (:musicalsharp #x0af5)  ; u+266f music sharp sign
  (:musicalflat #x0af6)  ; u+266d music flat sign
  (:malesymbol #x0af7)  ; u+2642 male sign
  (:femalesymbol #x0af8)  ; u+2640 female sign
  (:telephone #x0af9)  ; u+260e black telephone
  (:telephonerecorder #x0afa)  ; u+2315 telephone recorder
  (:phonographcopyright #x0afb)  ; u+2117 sound recording copyright
  (:caret #x0afc)  ; u+2038 caret
  (:singlelowquotemark #x0afd)  ; u+201a single low-9 quotation mark
  (:doublelowquotemark #x0afe)  ; u+201e double low-9 quotation mark
  (:cursor #x0aff)
  


  ;; apl
  ;; byte 3 = #x0b


  
  (:leftcaret #x0ba3)  ;(:u+003c less-than sign)*/
  (:rightcaret #x0ba6)  ;(:u+003e greater-than sign)*/
  (:downcaret #x0ba8)  ;(:u+2228 logical or)*/
  (:upcaret #x0ba9)  ;(:u+2227 logical and)*/
  (:overbar #x0bc0)  ;(:u+00af macron)*/
  (:downtack #x0bc2)  ; u+22a4 down tack
  (:upshoe #x0bc3)  ;(:u+2229 intersection)*/
  (:downstile #x0bc4)  ; u+230a left floor
  (:underbar #x0bc6)  ;(:u+005f low line)*/
  (:jot #x0bca)  ; u+2218 ring operator
  (:quad #x0bcc)  ; u+2395 apl functional symbol quad
  (:uptack #x0bce)  ; u+22a5 up tack
  (:circle #x0bcf)  ; u+25cb white circle
  (:upstile #x0bd3)  ; u+2308 left ceiling
  (:downshoe #x0bd6)  ;(:u+222a union)*/
  (:rightshoe #x0bd8)  ;(:u+2283 superset of)*/
  (:leftshoe #x0bda)  ;(:u+2282 subset of)*/
  (:lefttack #x0bdc)  ; u+22a3 left tack
  (:righttack #x0bfc)  ; u+22a2 right tack
  


  ;; hebrew
  ;; byte 3 = #x0c


  
  (:hebrew-doublelowline #x0cdf)  ; u+2017 double low line
  (:hebrew-aleph #x0ce0)  ; u+05d0 hebrew letter alef
  (:hebrew-bet #x0ce1)  ; u+05d1 hebrew letter bet
  (:hebrew-gimel #x0ce2)  ; u+05d2 hebrew letter gimel
  (:hebrew-dalet #x0ce3)  ; u+05d3 hebrew letter dalet
  (:hebrew-he #x0ce4)  ; u+05d4 hebrew letter he
  (:hebrew-waw #x0ce5)  ; u+05d5 hebrew letter vav
  (:hebrew-zain #x0ce6)  ; u+05d6 hebrew letter zayin
  (:hebrew-chet #x0ce7)  ; u+05d7 hebrew letter het
  (:hebrew-tet #x0ce8)  ; u+05d8 hebrew letter tet
  (:hebrew-yod #x0ce9)  ; u+05d9 hebrew letter yod
  (:hebrew-finalkaph #x0cea)  ; u+05da hebrew letter final kaf
  (:hebrew-kaph #x0ceb)  ; u+05db hebrew letter kaf
  (:hebrew-lamed #x0cec)  ; u+05dc hebrew letter lamed
  (:hebrew-finalmem #x0ced)  ; u+05dd hebrew letter final mem
  (:hebrew-mem #x0cee)  ; u+05de hebrew letter mem
  (:hebrew-finalnun #x0cef)  ; u+05df hebrew letter final nun
  (:hebrew-nun #x0cf0)  ; u+05e0 hebrew letter nun
  (:hebrew-samech #x0cf1)  ; u+05e1 hebrew letter samekh
  (:hebrew-ayin #x0cf2)  ; u+05e2 hebrew letter ayin
  (:hebrew-finalpe #x0cf3)  ; u+05e3 hebrew letter final pe
  (:hebrew-pe #x0cf4)  ; u+05e4 hebrew letter pe
  (:hebrew-finalzade #x0cf5)  ; u+05e5 hebrew letter final tsadi
  (:hebrew-zade #x0cf6)  ; u+05e6 hebrew letter tsadi
  (:hebrew-qoph #x0cf7)  ; u+05e7 hebrew letter qof
  (:hebrew-resh #x0cf8)  ; u+05e8 hebrew letter resh
  (:hebrew-shin #x0cf9)  ; u+05e9 hebrew letter shin
  (:hebrew-taw #x0cfa)  ; u+05ea hebrew letter tav
  (:hebrew-switch #xff7e)  ; alias for mode-switch
  


  ;; thai
  ;; byte 3 = #x0d  
  (:thai-kokai #x0da1)  ; u+0e01 thai character ko kai
  (:thai-khokhai #x0da2)  ; u+0e02 thai character kho khai
  (:thai-khokhuat #x0da3)  ; u+0e03 thai character kho khuat
  (:thai-khokhwai #x0da4)  ; u+0e04 thai character kho khwai
  (:thai-khokhon #x0da5)  ; u+0e05 thai character kho khon
  (:thai-khorakhang #x0da6)  ; u+0e06 thai character kho rakhang
  (:thai-ngongu #x0da7)  ; u+0e07 thai character ngo ngu
  (:thai-chochan #x0da8)  ; u+0e08 thai character cho chan
  (:thai-choching #x0da9)  ; u+0e09 thai character cho ching
  (:thai-chochang #x0daa)  ; u+0e0a thai character cho chang
  (:thai-soso #x0dab)  ; u+0e0b thai character so so
  (:thai-chochoe #x0dac)  ; u+0e0c thai character cho choe
  (:thai-yoying #x0dad)  ; u+0e0d thai character yo ying
  (:thai-dochada #x0dae)  ; u+0e0e thai character do chada
  (:thai-topatak #x0daf)  ; u+0e0f thai character to patak
  (:thai-thothan #x0db0)  ; u+0e10 thai character tho than
  (:thai-thonangmontho #x0db1)  ; u+0e11 thai character tho nangmontho
  (:thai-thophuthao #x0db2)  ; u+0e12 thai character tho phuthao
  (:thai-nonen #x0db3)  ; u+0e13 thai character no nen
  (:thai-dodek #x0db4)  ; u+0e14 thai character do dek
  (:thai-totao #x0db5)  ; u+0e15 thai character to tao
  (:thai-thothung #x0db6)  ; u+0e16 thai character tho thung
  (:thai-thothahan #x0db7)  ; u+0e17 thai character tho thahan
  (:thai-thothong #x0db8)  ; u+0e18 thai character tho thong
  (:thai-nonu #x0db9)  ; u+0e19 thai character no nu
  (:thai-bobaimai #x0dba)  ; u+0e1a thai character bo baimai
  (:thai-popla #x0dbb)  ; u+0e1b thai character po pla
  (:thai-phophung #x0dbc)  ; u+0e1c thai character pho phung
  (:thai-fofa #x0dbd)  ; u+0e1d thai character fo fa
  (:thai-phophan #x0dbe)  ; u+0e1e thai character pho phan
  (:thai-fofan #x0dbf)  ; u+0e1f thai character fo fan
  (:thai-phosamphao #x0dc0)  ; u+0e20 thai character pho samphao
  (:thai-moma #x0dc1)  ; u+0e21 thai character mo ma
  (:thai-yoyak #x0dc2)  ; u+0e22 thai character yo yak
  (:thai-rorua #x0dc3)  ; u+0e23 thai character ro rua
  (:thai-ru #x0dc4)  ; u+0e24 thai character ru
  (:thai-loling #x0dc5)  ; u+0e25 thai character lo ling
  (:thai-lu #x0dc6)  ; u+0e26 thai character lu
  (:thai-wowaen #x0dc7)  ; u+0e27 thai character wo waen
  (:thai-sosala #x0dc8)  ; u+0e28 thai character so sala
  (:thai-sorusi #x0dc9)  ; u+0e29 thai character so rusi
  (:thai-sosua #x0dca)  ; u+0e2a thai character so sua
  (:thai-hohip #x0dcb)  ; u+0e2b thai character ho hip
  (:thai-lochula #x0dcc)  ; u+0e2c thai character lo chula
  (:thai-oang #x0dcd)  ; u+0e2d thai character o ang
  (:thai-honokhuk #x0dce)  ; u+0e2e thai character ho nokhuk
  (:thai-paiyannoi #x0dcf)  ; u+0e2f thai character paiyannoi
  (:thai-saraa #x0dd0)  ; u+0e30 thai character sara a
  (:thai-maihanakat #x0dd1)  ; u+0e31 thai character mai han-akat
  (:thai-saraaa #x0dd2)  ; u+0e32 thai character sara aa
  (:thai-saraam #x0dd3)  ; u+0e33 thai character sara am
  (:thai-sarai #x0dd4)  ; u+0e34 thai character sara i
  (:thai-saraii #x0dd5)  ; u+0e35 thai character sara ii
  (:thai-saraue #x0dd6)  ; u+0e36 thai character sara ue
  (:thai-sarauee #x0dd7)  ; u+0e37 thai character sara uee
  (:thai-sarau #x0dd8)  ; u+0e38 thai character sara u
  (:thai-sarauu #x0dd9)  ; u+0e39 thai character sara uu
  (:thai-phinthu #x0dda)  ; u+0e3a thai character phinthu
  (:thai-maihanakat-maitho #x0dde)
  (:thai-baht #x0ddf)  ; u+0e3f thai currency symbol baht
  (:thai-sarae #x0de0)  ; u+0e40 thai character sara e
  (:thai-saraae #x0de1)  ; u+0e41 thai character sara ae
  (:thai-sarao #x0de2)  ; u+0e42 thai character sara o
  (:thai-saraaimaimuan #x0de3)  ; u+0e43 thai character sara ai maimuan
  (:thai-saraaimaimalai #x0de4)  ; u+0e44 thai character sara ai maimalai
  (:thai-lakkhangyao #x0de5)  ; u+0e45 thai character lakkhangyao
  (:thai-maiyamok #x0de6)  ; u+0e46 thai character maiyamok
  (:thai-maitaikhu #x0de7)  ; u+0e47 thai character maitaikhu
  (:thai-maiek #x0de8)  ; u+0e48 thai character mai ek
  (:thai-maitho #x0de9)  ; u+0e49 thai character mai tho
  (:thai-maitri #x0dea)  ; u+0e4a thai character mai tri
  (:thai-maichattawa #x0deb)  ; u+0e4b thai character mai chattawa
  (:thai-thanthakhat #x0dec)  ; u+0e4c thai character thanthakhat
  (:thai-nikhahit #x0ded)  ; u+0e4d thai character nikhahit
  (:thai-leksun #x0df0)  ; u+0e50 thai digit zero
  (:thai-leknung #x0df1)  ; u+0e51 thai digit one
  (:thai-leksong #x0df2)  ; u+0e52 thai digit two
  (:thai-leksam #x0df3)  ; u+0e53 thai digit three
  (:thai-leksi #x0df4)  ; u+0e54 thai digit four
  (:thai-lekha #x0df5)  ; u+0e55 thai digit five
  (:thai-lekhok #x0df6)  ; u+0e56 thai digit six
  (:thai-lekchet #x0df7)  ; u+0e57 thai digit seven
  (:thai-lekpaet #x0df8)  ; u+0e58 thai digit eight
  (:thai-lekkao #x0df9)  ; u+0e59 thai digit nine



  ;; korean
  ;; byte 3 = #x0e




  (:hangul #xff31)  ; hangul start/stop(:toggle)
  (:hangul-start #xff32)  ; hangul start
  (:hangul-end #xff33)  ; hangul end, english start
  (:hangul-hanja #xff34)  ; start hangul->hanja conversion
  (:hangul-jamo #xff35)  ; hangul jamo mode
  (:hangul-romaja #xff36)  ; hangul romaja mode
  (:hangul-codeinput #xff37)  ; hangul code input mode
  (:hangul-jeonja #xff38)  ; jeonja mode
  (:hangul-banja #xff39)  ; banja mode
  (:hangul-prehanja #xff3a)  ; pre hanja conversion
  (:hangul-posthanja #xff3b)  ; post hanja conversion
  (:hangul-singlecandidate #xff3c)  ; single candidate
  (:hangul-multiplecandidate #xff3d)  ; multiple candidate
  (:hangul-previouscandidate #xff3e)  ; previous candidate
  (:hangul-special #xff3f)  ; special symbols
  (:hangul-switch #xff7e)  ; alias for mode-switch

  ;; hangul consonant characters
  (:hangul-kiyeog #x0ea1)
  (:hangul-ssangkiyeog #x0ea2)
  (:hangul-kiyeogsios #x0ea3)
  (:hangul-nieun #x0ea4)
  (:hangul-nieunjieuj #x0ea5)
  (:hangul-nieunhieuh #x0ea6)
  (:hangul-dikeud #x0ea7)
  (:hangul-ssangdikeud #x0ea8)
  (:hangul-rieul #x0ea9)
  (:hangul-rieulkiyeog #x0eaa)
  (:hangul-rieulmieum #x0eab)
  (:hangul-rieulpieub #x0eac)
  (:hangul-rieulsios #x0ead)
  (:hangul-rieultieut #x0eae)
  (:hangul-rieulphieuf #x0eaf)
  (:hangul-rieulhieuh #x0eb0)
  (:hangul-mieum #x0eb1)
  (:hangul-pieub #x0eb2)
  (:hangul-ssangpieub #x0eb3)
  (:hangul-pieubsios #x0eb4)
  (:hangul-sios #x0eb5)
  (:hangul-ssangsios #x0eb6)
  (:hangul-ieung #x0eb7)
  (:hangul-jieuj #x0eb8)
  (:hangul-ssangjieuj #x0eb9)
  (:hangul-cieuc #x0eba)
  (:hangul-khieuq #x0ebb)
  (:hangul-tieut #x0ebc)
  (:hangul-phieuf #x0ebd)
  (:hangul-hieuh #x0ebe)

  ;; hangul vowel characters
  (:hangul-a #x0ebf)
  (:hangul-ae #x0ec0)
  (:hangul-ya #x0ec1)
  (:hangul-yae #x0ec2)
  (:hangul-eo #x0ec3)
  (:hangul-e #x0ec4)
  (:hangul-yeo #x0ec5)
  (:hangul-ye #x0ec6)
  (:hangul-o #x0ec7)
  (:hangul-wa #x0ec8)
  (:hangul-wae #x0ec9)
  (:hangul-oe #x0eca)
  (:hangul-yo #x0ecb)
  (:hangul-u #x0ecc)
  (:hangul-weo #x0ecd)
  (:hangul-we #x0ece)
  (:hangul-wi #x0ecf)
  (:hangul-yu #x0ed0)
  (:hangul-eu #x0ed1)
  (:hangul-yi #x0ed2)
  (:hangul-i #x0ed3)

  ;; hangul syllable-final (:jongseong) characters
  (:hangul-j-kiyeog #x0ed4)
  (:hangul-j-ssangkiyeog #x0ed5)
  (:hangul-j-kiyeogsios #x0ed6)
  (:hangul-j-nieun #x0ed7)
  (:hangul-j-nieunjieuj #x0ed8)
  (:hangul-j-nieunhieuh #x0ed9)
  (:hangul-j-dikeud #x0eda)
  (:hangul-j-rieul #x0edb)
  (:hangul-j-rieulkiyeog #x0edc)
  (:hangul-j-rieulmieum #x0edd)
  (:hangul-j-rieulpieub #x0ede)
  (:hangul-j-rieulsios #x0edf)
  (:hangul-j-rieultieut #x0ee0)
  (:hangul-j-rieulphieuf #x0ee1)
  (:hangul-j-rieulhieuh #x0ee2)
  (:hangul-j-mieum #x0ee3)
  (:hangul-j-pieub #x0ee4)
  (:hangul-j-pieubsios #x0ee5)
  (:hangul-j-sios #x0ee6)
  (:hangul-j-ssangsios #x0ee7)
  (:hangul-j-ieung #x0ee8)
  (:hangul-j-jieuj #x0ee9)
  (:hangul-j-cieuc #x0eea)
  (:hangul-j-khieuq #x0eeb)
  (:hangul-j-tieut #x0eec)
  (:hangul-j-phieuf #x0eed)
  (:hangul-j-hieuh #x0eee)

  ;; ancient hangul consonant characters
  (:hangul-rieulyeorinhieuh #x0eef)
  (:hangul-sunkyeongeummieum #x0ef0)
  (:hangul-sunkyeongeumpieub #x0ef1)
  (:hangul-pansios #x0ef2)
  (:hangul-kkogjidalrinieung #x0ef3)
  (:hangul-sunkyeongeumphieuf #x0ef4)
  (:hangul-yeorinhieuh #x0ef5)

  ;; ancient hangul vowel characters
  (:hangul-araea #x0ef6)
  (:hangul-araeae #x0ef7)

  ;; ancient hangul syllable-final (:jongseong) characters
  (:hangul-j-pansios #x0ef8)
  (:hangul-j-kkogjidalrinieung #x0ef9)
  (:hangul-j-yeorinhieuh #x0efa)

  ;; korean currency symbol
  (:korean-won #x0eff)  ;(:u+20a9 won sign)*/




  ;; armenian



  (:armenian-ligature-ew #x1000587)  ; u+0587 armenian small ligature ech yiwn
  (:armenian-full-stop #x1000589)  ; u+0589 armenian full stop
  (:armenian-verjaket #x1000589)  ; u+0589 armenian full stop
  (:armenian-separation-mark #x100055d)  ; u+055d armenian comma
  (:armenian-but #x100055d)  ; u+055d armenian comma
  (:armenian-hyphen #x100058a)  ; u+058a armenian hyphen
  (:armenian-yentamna #x100058a)  ; u+058a armenian hyphen
  (:armenian-exclam #x100055c)  ; u+055c armenian exclamation mark
  (:armenian-amanak #x100055c)  ; u+055c armenian exclamation mark
  (:armenian-accent #x100055b)  ; u+055b armenian emphasis mark
  (:armenian-shesht #x100055b)  ; u+055b armenian emphasis mark
  (:armenian-question #x100055e)  ; u+055e armenian question mark
  (:armenian-paruyk #x100055e)  ; u+055e armenian question mark
  (:capital-armenian-ayb #x1000531)  ; u+0531 armenian capital letter ayb
  (:armenian-ayb #x1000561)  ; u+0561 armenian small letter ayb
  (:capital-armenian-ben #x1000532)  ; u+0532 armenian capital letter ben
  (:armenian-ben #x1000562)  ; u+0562 armenian small letter ben
  (:capital-armenian-gim #x1000533)  ; u+0533 armenian capital letter gim
  (:armenian-gim #x1000563)  ; u+0563 armenian small letter gim
  (:capital-armenian-da #x1000534)  ; u+0534 armenian capital letter da
  (:armenian-da #x1000564)  ; u+0564 armenian small letter da
  (:capital-armenian-yech #x1000535)  ; u+0535 armenian capital letter ech
  (:armenian-yech #x1000565)  ; u+0565 armenian small letter ech
  (:capital-armenian-za #x1000536)  ; u+0536 armenian capital letter za
  (:armenian-za #x1000566)  ; u+0566 armenian small letter za
  (:capital-armenian-e #x1000537)  ; u+0537 armenian capital letter eh
  (:armenian-e #x1000567)  ; u+0567 armenian small letter eh
  (:capital-armenian-at #x1000538)  ; u+0538 armenian capital letter et
  (:armenian-at #x1000568)  ; u+0568 armenian small letter et
  (:capital-armenian-to #x1000539)  ; u+0539 armenian capital letter to
  (:armenian-to #x1000569)  ; u+0569 armenian small letter to
  (:capital-armenian-zhe #x100053a)  ; u+053a armenian capital letter zhe
  (:armenian-zhe #x100056a)  ; u+056a armenian small letter zhe
  (:capital-armenian-ini #x100053b)  ; u+053b armenian capital letter ini
  (:armenian-ini #x100056b)  ; u+056b armenian small letter ini
  (:capital-armenian-lyun #x100053c)  ; u+053c armenian capital letter liwn
  (:armenian-lyun #x100056c)  ; u+056c armenian small letter liwn
  (:capital-armenian-khe #x100053d)  ; u+053d armenian capital letter xeh
  (:armenian-khe #x100056d)  ; u+056d armenian small letter xeh
  (:capital-armenian-tsa #x100053e)  ; u+053e armenian capital letter ca
  (:armenian-tsa #x100056e)  ; u+056e armenian small letter ca
  (:capital-armenian-ken #x100053f)  ; u+053f armenian capital letter ken
  (:armenian-ken #x100056f)  ; u+056f armenian small letter ken
  (:capital-armenian-ho #x1000540)  ; u+0540 armenian capital letter ho
  (:armenian-ho #x1000570)  ; u+0570 armenian small letter ho
  (:capital-armenian-dza #x1000541)  ; u+0541 armenian capital letter ja
  (:armenian-dza #x1000571)  ; u+0571 armenian small letter ja
  (:capital-armenian-ghat #x1000542)  ; u+0542 armenian capital letter ghad
  (:armenian-ghat #x1000572)  ; u+0572 armenian small letter ghad
  (:capital-armenian-tche #x1000543)  ; u+0543 armenian capital letter cheh
  (:armenian-tche #x1000573)  ; u+0573 armenian small letter cheh
  (:capital-armenian-men #x1000544)  ; u+0544 armenian capital letter men
  (:armenian-men #x1000574)  ; u+0574 armenian small letter men
  (:capital-armenian-hi #x1000545)  ; u+0545 armenian capital letter yi
  (:armenian-hi #x1000575)  ; u+0575 armenian small letter yi
  (:capital-armenian-nu #x1000546)  ; u+0546 armenian capital letter now
  (:armenian-nu #x1000576)  ; u+0576 armenian small letter now
  (:capital-armenian-sha #x1000547)  ; u+0547 armenian capital letter sha
  (:armenian-sha #x1000577)  ; u+0577 armenian small letter sha
  (:capital-armenian-vo #x1000548)  ; u+0548 armenian capital letter vo
  (:armenian-vo #x1000578)  ; u+0578 armenian small letter vo
  (:capital-armenian-cha #x1000549)  ; u+0549 armenian capital letter cha
  (:armenian-cha #x1000579)  ; u+0579 armenian small letter cha
  (:capital-armenian-pe #x100054a)  ; u+054a armenian capital letter peh
  (:armenian-pe #x100057a)  ; u+057a armenian small letter peh
  (:capital-armenian-je #x100054b)  ; u+054b armenian capital letter jheh
  (:armenian-je #x100057b)  ; u+057b armenian small letter jheh
  (:capital-armenian-ra #x100054c)  ; u+054c armenian capital letter ra
  (:armenian-ra #x100057c)  ; u+057c armenian small letter ra
  (:capital-armenian-se #x100054d)  ; u+054d armenian capital letter seh
  (:armenian-se #x100057d)  ; u+057d armenian small letter seh
  (:capital-armenian-vev #x100054e)  ; u+054e armenian capital letter vew
  (:armenian-vev #x100057e)  ; u+057e armenian small letter vew
  (:capital-armenian-tyun #x100054f)  ; u+054f armenian capital letter tiwn
  (:armenian-tyun #x100057f)  ; u+057f armenian small letter tiwn
  (:capital-armenian-re #x1000550)  ; u+0550 armenian capital letter reh
  (:armenian-re #x1000580)  ; u+0580 armenian small letter reh
  (:capital-armenian-tso #x1000551)  ; u+0551 armenian capital letter co
  (:armenian-tso #x1000581)  ; u+0581 armenian small letter co
  (:capital-armenian-vyun #x1000552)  ; u+0552 armenian capital letter yiwn
  (:armenian-vyun #x1000582)  ; u+0582 armenian small letter yiwn
  (:capital-armenian-pyur #x1000553)  ; u+0553 armenian capital letter piwr
  (:armenian-pyur #x1000583)  ; u+0583 armenian small letter piwr
  (:capital-armenian-ke #x1000554)  ; u+0554 armenian capital letter keh
  (:armenian-ke #x1000584)  ; u+0584 armenian small letter keh
  (:capital-armenian-o #x1000555)  ; u+0555 armenian capital letter oh
  (:armenian-o #x1000585)  ; u+0585 armenian small letter oh
  (:capital-armenian-fe #x1000556)  ; u+0556 armenian capital letter feh
  (:armenian-fe #x1000586)  ; u+0586 armenian small letter feh
  (:armenian-apostrophe #x100055a)  ; u+055a armenian apostrophe



  ;; georgian
  (:georgian-an #x10010d0)  ; u+10d0 georgian letter an
  (:georgian-ban #x10010d1)  ; u+10d1 georgian letter ban
  (:georgian-gan #x10010d2)  ; u+10d2 georgian letter gan
  (:georgian-don #x10010d3)  ; u+10d3 georgian letter don
  (:georgian-en #x10010d4)  ; u+10d4 georgian letter en
  (:georgian-vin #x10010d5)  ; u+10d5 georgian letter vin
  (:georgian-zen #x10010d6)  ; u+10d6 georgian letter zen
  (:georgian-tan #x10010d7)  ; u+10d7 georgian letter tan
  (:georgian-in #x10010d8)  ; u+10d8 georgian letter in
  (:georgian-kan #x10010d9)  ; u+10d9 georgian letter kan
  (:georgian-las #x10010da)  ; u+10da georgian letter las
  (:georgian-man #x10010db)  ; u+10db georgian letter man
  (:georgian-nar #x10010dc)  ; u+10dc georgian letter nar
  (:georgian-on #x10010dd)  ; u+10dd georgian letter on
  (:georgian-par #x10010de)  ; u+10de georgian letter par
  (:georgian-zhar #x10010df)  ; u+10df georgian letter zhar
  (:georgian-rae #x10010e0)  ; u+10e0 georgian letter rae
  (:georgian-san #x10010e1)  ; u+10e1 georgian letter san
  (:georgian-tar #x10010e2)  ; u+10e2 georgian letter tar
  (:georgian-un #x10010e3)  ; u+10e3 georgian letter un
  (:georgian-phar #x10010e4)  ; u+10e4 georgian letter phar
  (:georgian-khar #x10010e5)  ; u+10e5 georgian letter khar
  (:georgian-ghan #x10010e6)  ; u+10e6 georgian letter ghan
  (:georgian-qar #x10010e7)  ; u+10e7 georgian letter qar
  (:georgian-shin #x10010e8)  ; u+10e8 georgian letter shin
  (:georgian-chin #x10010e9)  ; u+10e9 georgian letter chin
  (:georgian-can #x10010ea)  ; u+10ea georgian letter can
  (:georgian-jil #x10010eb)  ; u+10eb georgian letter jil
  (:georgian-cil #x10010ec)  ; u+10ec georgian letter cil
  (:georgian-char #x10010ed)  ; u+10ed georgian letter char
  (:georgian-xan #x10010ee)  ; u+10ee georgian letter xan
  (:georgian-jhan #x10010ef)  ; u+10ef georgian letter jhan
  (:georgian-hae #x10010f0)  ; u+10f0 georgian letter hae
  (:georgian-he #x10010f1)  ; u+10f1 georgian letter he
  (:georgian-hie #x10010f2)  ; u+10f2 georgian letter hie
  (:georgian-we #x10010f3)  ; u+10f3 georgian letter we
  (:georgian-har #x10010f4)  ; u+10f4 georgian letter har
  (:georgian-hoe #x10010f5)  ; u+10f5 georgian letter hoe
  (:georgian-fi #x10010f6)  ; u+10f6 georgian letter fi


  ;; azeri (:and other turkic or caucasian languages)



  ;; latin
  (:capital-xabovedot #x1001e8a)  ; u+1e8a latin capital letter x with dot above
  (:capital-ibreve #x100012c)  ; u+012c latin capital letter i with breve
  (:capital-zstroke #x10001b5)  ; u+01b5 latin capital letter z with stroke
  (:capital-gcaron #x10001e6)  ; u+01e6 latin capital letter g with caron
  (:capital-ocaron #x10001d1)  ; u+01d2 latin capital letter o with caron
  (:capital-obarred #x100019f)  ; u+019f latin capital letter o with middle tilde
  (:xabovedot #x1001e8b)  ; u+1e8b latin small letter x with dot above
  (:ibreve #x100012d)  ; u+012d latin small letter i with breve
  (:zstroke #x10001b6)  ; u+01b6 latin small letter z with stroke
  (:gcaron #x10001e7)  ; u+01e7 latin small letter g with caron
  (:ocaron #x10001d2)  ; u+01d2 latin small letter o with caron
  (:obarred #x1000275)  ; u+0275 latin small letter barred o
  (:capital-schwa #x100018f)  ; u+018f latin capital letter schwa
  (:schwa #x1000259)  ; u+0259 latin small letter schwa
  ;; those are not really caucasus
  ;; for inupiak
  (:capital-lbelowdot #x1001e36)  ; u+1e36 latin capital letter l with dot below
  (:lbelowdot #x1001e37)  ; u+1e37 latin small letter l with dot below


  ;; vietnamese
  (:capital-abelowdot #x1001ea0)  ; u+1ea0 latin capital letter a with dot below
  (:abelowdot #x1001ea1)  ; u+1ea1 latin small letter a with dot below
  (:capital-ahook #x1001ea2)  ; u+1ea2 latin capital letter a with hook above
  (:ahook #x1001ea3)  ; u+1ea3 latin small letter a with hook above
  (:capital-acircumflexacute #x1001ea4)  ; u+1ea4 latin capital letter a with circumflex and acute
  (:acircumflexacute #x1001ea5)  ; u+1ea5 latin small letter a with circumflex and acute
  (:capital-acircumflexgrave #x1001ea6)  ; u+1ea6 latin capital letter a with circumflex and grave
  (:acircumflexgrave #x1001ea7)  ; u+1ea7 latin small letter a with circumflex and grave
  (:capital-acircumflexhook #x1001ea8)  ; u+1ea8 latin capital letter a with circumflex and hook above
  (:acircumflexhook #x1001ea9)  ; u+1ea9 latin small letter a with circumflex and hook above
  (:capital-acircumflextilde #x1001eaa)  ; u+1eaa latin capital letter a with circumflex and tilde
  (:acircumflextilde #x1001eab)  ; u+1eab latin small letter a with circumflex and tilde
  (:capital-acircumflexbelowdot #x1001eac)  ; u+1eac latin capital letter a with circumflex and dot below
  (:acircumflexbelowdot #x1001ead)  ; u+1ead latin small letter a with circumflex and dot below
  (:capital-abreveacute #x1001eae)  ; u+1eae latin capital letter a with breve and acute
  (:abreveacute #x1001eaf)  ; u+1eaf latin small letter a with breve and acute
  (:capital-abrevegrave #x1001eb0)  ; u+1eb0 latin capital letter a with breve and grave
  (:abrevegrave #x1001eb1)  ; u+1eb1 latin small letter a with breve and grave
  (:capital-abrevehook #x1001eb2)  ; u+1eb2 latin capital letter a with breve and hook above
  (:abrevehook #x1001eb3)  ; u+1eb3 latin small letter a with breve and hook above
  (:capital-abrevetilde #x1001eb4)  ; u+1eb4 latin capital letter a with breve and tilde
  (:abrevetilde #x1001eb5)  ; u+1eb5 latin small letter a with breve and tilde
  (:capital-abrevebelowdot #x1001eb6)  ; u+1eb6 latin capital letter a with breve and dot below
  (:abrevebelowdot #x1001eb7)  ; u+1eb7 latin small letter a with breve and dot below
  (:capital-ebelowdot #x1001eb8)  ; u+1eb8 latin capital letter e with dot below
  (:ebelowdot #x1001eb9)  ; u+1eb9 latin small letter e with dot below
  (:capital-ehook #x1001eba)  ; u+1eba latin capital letter e with hook above
  (:ehook #x1001ebb)  ; u+1ebb latin small letter e with hook above
  (:capital-etilde #x1001ebc)  ; u+1ebc latin capital letter e with tilde
  (:etilde #x1001ebd)  ; u+1ebd latin small letter e with tilde
  (:capital-ecircumflexacute #x1001ebe)  ; u+1ebe latin capital letter e with circumflex and acute
  (:ecircumflexacute #x1001ebf)  ; u+1ebf latin small letter e with circumflex and acute
  (:capital-ecircumflexgrave #x1001ec0)  ; u+1ec0 latin capital letter e with circumflex and grave
  (:ecircumflexgrave #x1001ec1)  ; u+1ec1 latin small letter e with circumflex and grave
  (:capital-ecircumflexhook #x1001ec2)  ; u+1ec2 latin capital letter e with circumflex and hook above
  (:ecircumflexhook #x1001ec3)  ; u+1ec3 latin small letter e with circumflex and hook above
  (:capital-ecircumflextilde #x1001ec4)  ; u+1ec4 latin capital letter e with circumflex and tilde
  (:ecircumflextilde #x1001ec5)  ; u+1ec5 latin small letter e with circumflex and tilde
  (:capital-ecircumflexbelowdot #x1001ec6)  ; u+1ec6 latin capital letter e with circumflex and dot below
  (:ecircumflexbelowdot #x1001ec7)  ; u+1ec7 latin small letter e with circumflex and dot below
  (:capital-ihook #x1001ec8)  ; u+1ec8 latin capital letter i with hook above
  (:ihook #x1001ec9)  ; u+1ec9 latin small letter i with hook above
  (:capital-ibelowdot #x1001eca)  ; u+1eca latin capital letter i with dot below
  (:ibelowdot #x1001ecb)  ; u+1ecb latin small letter i with dot below
  (:capital-obelowdot #x1001ecc)  ; u+1ecc latin capital letter o with dot below
  (:obelowdot #x1001ecd)  ; u+1ecd latin small letter o with dot below
  (:capital-ohook #x1001ece)  ; u+1ece latin capital letter o with hook above
  (:ohook #x1001ecf)  ; u+1ecf latin small letter o with hook above
  (:capital-ocircumflexacute #x1001ed0)  ; u+1ed0 latin capital letter o with circumflex and acute
  (:ocircumflexacute #x1001ed1)  ; u+1ed1 latin small letter o with circumflex and acute
  (:capital-ocircumflexgrave #x1001ed2)  ; u+1ed2 latin capital letter o with circumflex and grave
  (:ocircumflexgrave #x1001ed3)  ; u+1ed3 latin small letter o with circumflex and grave
  (:capital-ocircumflexhook #x1001ed4)  ; u+1ed4 latin capital letter o with circumflex and hook above
  (:ocircumflexhook #x1001ed5)  ; u+1ed5 latin small letter o with circumflex and hook above
  (:capital-ocircumflextilde #x1001ed6)  ; u+1ed6 latin capital letter o with circumflex and tilde
  (:ocircumflextilde #x1001ed7)  ; u+1ed7 latin small letter o with circumflex and tilde
  (:capital-ocircumflexbelowdot #x1001ed8)  ; u+1ed8 latin capital letter o with circumflex and dot below
  (:ocircumflexbelowdot #x1001ed9)  ; u+1ed9 latin small letter o with circumflex and dot below
  (:capital-ohornacute #x1001eda)  ; u+1eda latin capital letter o with horn and acute
  (:ohornacute #x1001edb)  ; u+1edb latin small letter o with horn and acute
  (:capital-ohorngrave #x1001edc)  ; u+1edc latin capital letter o with horn and grave
  (:ohorngrave #x1001edd)  ; u+1edd latin small letter o with horn and grave
  (:capital-ohornhook #x1001ede)  ; u+1ede latin capital letter o with horn and hook above
  (:ohornhook #x1001edf)  ; u+1edf latin small letter o with horn and hook above
  (:capital-ohorntilde #x1001ee0)  ; u+1ee0 latin capital letter o with horn and tilde
  (:ohorntilde #x1001ee1)  ; u+1ee1 latin small letter o with horn and tilde
  (:capital-ohornbelowdot #x1001ee2)  ; u+1ee2 latin capital letter o with horn and dot below
  (:ohornbelowdot #x1001ee3)  ; u+1ee3 latin small letter o with horn and dot below
  (:capital-ubelowdot #x1001ee4)  ; u+1ee4 latin capital letter u with dot below
  (:ubelowdot #x1001ee5)  ; u+1ee5 latin small letter u with dot below
  (:capital-uhook #x1001ee6)  ; u+1ee6 latin capital letter u with hook above
  (:uhook #x1001ee7)  ; u+1ee7 latin small letter u with hook above
  (:capital-uhornacute #x1001ee8)  ; u+1ee8 latin capital letter u with horn and acute
  (:uhornacute #x1001ee9)  ; u+1ee9 latin small letter u with horn and acute
  (:capital-uhorngrave #x1001eea)  ; u+1eea latin capital letter u with horn and grave
  (:uhorngrave #x1001eeb)  ; u+1eeb latin small letter u with horn and grave
  (:capital-uhornhook #x1001eec)  ; u+1eec latin capital letter u with horn and hook above
  (:uhornhook #x1001eed)  ; u+1eed latin small letter u with horn and hook above
  (:capital-uhorntilde #x1001eee)  ; u+1eee latin capital letter u with horn and tilde
  (:uhorntilde #x1001eef)  ; u+1eef latin small letter u with horn and tilde
  (:capital-uhornbelowdot #x1001ef0)  ; u+1ef0 latin capital letter u with horn and dot below
  (:uhornbelowdot #x1001ef1)  ; u+1ef1 latin small letter u with horn and dot below
  (:capital-ybelowdot #x1001ef4)  ; u+1ef4 latin capital letter y with dot below
  (:ybelowdot #x1001ef5)  ; u+1ef5 latin small letter y with dot below
  (:capital-yhook #x1001ef6)  ; u+1ef6 latin capital letter y with hook above
  (:yhook #x1001ef7)  ; u+1ef7 latin small letter y with hook above
  (:capital-ytilde #x1001ef8)  ; u+1ef8 latin capital letter y with tilde
  (:ytilde #x1001ef9)  ; u+1ef9 latin small letter y with tilde
  (:capital-ohorn #x10001a0)  ; u+01a0 latin capital letter o with horn
  (:ohorn #x10001a1)  ; u+01a1 latin small letter o with horn
  (:capital-uhorn #x10001af)  ; u+01af latin capital letter u with horn
  (:uhorn #x10001b0)  ; u+01b0 latin small letter u with horn




  (:ecusign #x10020a0)  ; u+20a0 euro-currency sign
  (:colonsign #x10020a1)  ; u+20a1 colon sign
  (:cruzeirosign #x10020a2)  ; u+20a2 cruzeiro sign
  (:ffrancsign #x10020a3)  ; u+20a3 french franc sign
  (:lirasign #x10020a4)  ; u+20a4 lira sign
  (:millsign #x10020a5)  ; u+20a5 mill sign
  (:nairasign #x10020a6)  ; u+20a6 naira sign
  (:pesetasign #x10020a7)  ; u+20a7 peseta sign
  (:rupeesign #x10020a8)  ; u+20a8 rupee sign
  (:wonsign #x10020a9)  ; u+20a9 won sign
  (:newsheqelsign #x10020aa)  ; u+20aa new sheqel sign
  (:dongsign #x10020ab)  ; u+20ab dong sign
  (:eurosign #x20ac)  ; u+20ac euro sign



  ;; one, two and three are defined above.
  (:zerosuperior #x1002070)  ; u+2070 superscript zero
  (:foursuperior #x1002074)  ; u+2074 superscript four
  (:fivesuperior #x1002075)  ; u+2075 superscript five
  (:sixsuperior #x1002076)  ; u+2076 superscript six
  (:sevensuperior #x1002077)  ; u+2077 superscript seven
  (:eightsuperior #x1002078)  ; u+2078 superscript eight
  (:ninesuperior #x1002079)  ; u+2079 superscript nine
  (:zerosubscript #x1002080)  ; u+2080 subscript zero
  (:onesubscript #x1002081)  ; u+2081 subscript one
  (:twosubscript #x1002082)  ; u+2082 subscript two
  (:threesubscript #x1002083)  ; u+2083 subscript three
  (:foursubscript #x1002084)  ; u+2084 subscript four
  (:fivesubscript #x1002085)  ; u+2085 subscript five
  (:sixsubscript #x1002086)  ; u+2086 subscript six
  (:sevensubscript #x1002087)  ; u+2087 subscript seven
  (:eightsubscript #x1002088)  ; u+2088 subscript eight
  (:ninesubscript #x1002089)  ; u+2089 subscript nine
  (:partdifferential #x1002202)  ; u+2202 partial differential
  (:emptyset #x1002205)  ; u+2205 null set
  (:elementof #x1002208)  ; u+2208 element of
  (:notelementof #x1002209)  ; u+2209 not an element of
  (:containsas #x100220b)  ; u+220b contains as member
  (:squareroot #x100221a)  ; u+221a square root
  (:cuberoot #x100221b)  ; u+221b cube root
  (:fourthroot #x100221c)  ; u+221c fourth root
  (:dintegral #x100222c)  ; u+222c double integral
  (:tintegral #x100222d)  ; u+222d triple integral
  (:because #x1002235)  ; u+2235 because
  (:approxeq #x1002248)  ; u+2245 almost equal to
  (:notapproxeq #x1002247)  ; u+2247 not almost equal to
  (:notidentical #x1002262)  ; u+2262 not identical to
  (:stricteq #x1002263)  ; u+2263 strictly equivalent to          



  (:braille-dot-1 #xfff1)
  (:braille-dot-2 #xfff2)
  (:braille-dot-3 #xfff3)
  (:braille-dot-4 #xfff4)
  (:braille-dot-5 #xfff5)
  (:braille-dot-6 #xfff6)
  (:braille-dot-7 #xfff7)
  (:braille-dot-8 #xfff8)
  (:braille-dot-9 #xfff9)
  (:braille-dot-10 #xfffa)
  (:braille-blank #x1002800)  ; u+2800 braille pattern blank
  (:braille-dots-1 #x1002801)  ; u+2801 braille pattern dots-1
  (:braille-dots-2 #x1002802)  ; u+2802 braille pattern dots-2
  (:braille-dots-12 #x1002803)  ; u+2803 braille pattern dots-12
  (:braille-dots-3 #x1002804)  ; u+2804 braille pattern dots-3
  (:braille-dots-13 #x1002805)  ; u+2805 braille pattern dots-13
  (:braille-dots-23 #x1002806)  ; u+2806 braille pattern dots-23
  (:braille-dots-123 #x1002807)  ; u+2807 braille pattern dots-123
  (:braille-dots-4 #x1002808)  ; u+2808 braille pattern dots-4
  (:braille-dots-14 #x1002809)  ; u+2809 braille pattern dots-14
  (:braille-dots-24 #x100280a)  ; u+280a braille pattern dots-24
  (:braille-dots-124 #x100280b)  ; u+280b braille pattern dots-124
  (:braille-dots-34 #x100280c)  ; u+280c braille pattern dots-34
  (:braille-dots-134 #x100280d)  ; u+280d braille pattern dots-134
  (:braille-dots-234 #x100280e)  ; u+280e braille pattern dots-234
  (:braille-dots-1234 #x100280f)  ; u+280f braille pattern dots-1234
  (:braille-dots-5 #x1002810)  ; u+2810 braille pattern dots-5
  (:braille-dots-15 #x1002811)  ; u+2811 braille pattern dots-15
  (:braille-dots-25 #x1002812)  ; u+2812 braille pattern dots-25
  (:braille-dots-125 #x1002813)  ; u+2813 braille pattern dots-125
  (:braille-dots-35 #x1002814)  ; u+2814 braille pattern dots-35
  (:braille-dots-135 #x1002815)  ; u+2815 braille pattern dots-135
  (:braille-dots-235 #x1002816)  ; u+2816 braille pattern dots-235
  (:braille-dots-1235 #x1002817)  ; u+2817 braille pattern dots-1235
  (:braille-dots-45 #x1002818)  ; u+2818 braille pattern dots-45
  (:braille-dots-145 #x1002819)  ; u+2819 braille pattern dots-145
  (:braille-dots-245 #x100281a)  ; u+281a braille pattern dots-245
  (:braille-dots-1245 #x100281b)  ; u+281b braille pattern dots-1245
  (:braille-dots-345 #x100281c)  ; u+281c braille pattern dots-345
  (:braille-dots-1345 #x100281d)  ; u+281d braille pattern dots-1345
  (:braille-dots-2345 #x100281e)  ; u+281e braille pattern dots-2345
  (:braille-dots-12345 #x100281f)  ; u+281f braille pattern dots-12345
  (:braille-dots-6 #x1002820)  ; u+2820 braille pattern dots-6
  (:braille-dots-16 #x1002821)  ; u+2821 braille pattern dots-16
  (:braille-dots-26 #x1002822)  ; u+2822 braille pattern dots-26
  (:braille-dots-126 #x1002823)  ; u+2823 braille pattern dots-126
  (:braille-dots-36 #x1002824)  ; u+2824 braille pattern dots-36
  (:braille-dots-136 #x1002825)  ; u+2825 braille pattern dots-136
  (:braille-dots-236 #x1002826)  ; u+2826 braille pattern dots-236
  (:braille-dots-1236 #x1002827)  ; u+2827 braille pattern dots-1236
  (:braille-dots-46 #x1002828)  ; u+2828 braille pattern dots-46
  (:braille-dots-146 #x1002829)  ; u+2829 braille pattern dots-146
  (:braille-dots-246 #x100282a)  ; u+282a braille pattern dots-246
  (:braille-dots-1246 #x100282b)  ; u+282b braille pattern dots-1246
  (:braille-dots-346 #x100282c)  ; u+282c braille pattern dots-346
  (:braille-dots-1346 #x100282d)  ; u+282d braille pattern dots-1346
  (:braille-dots-2346 #x100282e)  ; u+282e braille pattern dots-2346
  (:braille-dots-12346 #x100282f)  ; u+282f braille pattern dots-12346
  (:braille-dots-56 #x1002830)  ; u+2830 braille pattern dots-56
  (:braille-dots-156 #x1002831)  ; u+2831 braille pattern dots-156
  (:braille-dots-256 #x1002832)  ; u+2832 braille pattern dots-256
  (:braille-dots-1256 #x1002833)  ; u+2833 braille pattern dots-1256
  (:braille-dots-356 #x1002834)  ; u+2834 braille pattern dots-356
  (:braille-dots-1356 #x1002835)  ; u+2835 braille pattern dots-1356
  (:braille-dots-2356 #x1002836)  ; u+2836 braille pattern dots-2356
  (:braille-dots-12356 #x1002837)  ; u+2837 braille pattern dots-12356
  (:braille-dots-456 #x1002838)  ; u+2838 braille pattern dots-456
  (:braille-dots-1456 #x1002839)  ; u+2839 braille pattern dots-1456
  (:braille-dots-2456 #x100283a)  ; u+283a braille pattern dots-2456
  (:braille-dots-12456 #x100283b)  ; u+283b braille pattern dots-12456
  (:braille-dots-3456 #x100283c)  ; u+283c braille pattern dots-3456
  (:braille-dots-13456 #x100283d)  ; u+283d braille pattern dots-13456
  (:braille-dots-23456 #x100283e)  ; u+283e braille pattern dots-23456
  (:braille-dots-123456 #x100283f)  ; u+283f braille pattern dots-123456
  (:braille-dots-7 #x1002840)  ; u+2840 braille pattern dots-7
  (:braille-dots-17 #x1002841)  ; u+2841 braille pattern dots-17
  (:braille-dots-27 #x1002842)  ; u+2842 braille pattern dots-27
  (:braille-dots-127 #x1002843)  ; u+2843 braille pattern dots-127
  (:braille-dots-37 #x1002844)  ; u+2844 braille pattern dots-37
  (:braille-dots-137 #x1002845)  ; u+2845 braille pattern dots-137
  (:braille-dots-237 #x1002846)  ; u+2846 braille pattern dots-237
  (:braille-dots-1237 #x1002847)  ; u+2847 braille pattern dots-1237
  (:braille-dots-47 #x1002848)  ; u+2848 braille pattern dots-47
  (:braille-dots-147 #x1002849)  ; u+2849 braille pattern dots-147
  (:braille-dots-247 #x100284a)  ; u+284a braille pattern dots-247
  (:braille-dots-1247 #x100284b)  ; u+284b braille pattern dots-1247
  (:braille-dots-347 #x100284c)  ; u+284c braille pattern dots-347
  (:braille-dots-1347 #x100284d)  ; u+284d braille pattern dots-1347
  (:braille-dots-2347 #x100284e)  ; u+284e braille pattern dots-2347
  (:braille-dots-12347 #x100284f)  ; u+284f braille pattern dots-12347
  (:braille-dots-57 #x1002850)  ; u+2850 braille pattern dots-57
  (:braille-dots-157 #x1002851)  ; u+2851 braille pattern dots-157
  (:braille-dots-257 #x1002852)  ; u+2852 braille pattern dots-257
  (:braille-dots-1257 #x1002853)  ; u+2853 braille pattern dots-1257
  (:braille-dots-357 #x1002854)  ; u+2854 braille pattern dots-357
  (:braille-dots-1357 #x1002855)  ; u+2855 braille pattern dots-1357
  (:braille-dots-2357 #x1002856)  ; u+2856 braille pattern dots-2357
  (:braille-dots-12357 #x1002857)  ; u+2857 braille pattern dots-12357
  (:braille-dots-457 #x1002858)  ; u+2858 braille pattern dots-457
  (:braille-dots-1457 #x1002859)  ; u+2859 braille pattern dots-1457
  (:braille-dots-2457 #x100285a)  ; u+285a braille pattern dots-2457
  (:braille-dots-12457 #x100285b)  ; u+285b braille pattern dots-12457
  (:braille-dots-3457 #x100285c)  ; u+285c braille pattern dots-3457
  (:braille-dots-13457 #x100285d)  ; u+285d braille pattern dots-13457
  (:braille-dots-23457 #x100285e)  ; u+285e braille pattern dots-23457
  (:braille-dots-123457 #x100285f)  ; u+285f braille pattern dots-123457
  (:braille-dots-67 #x1002860)  ; u+2860 braille pattern dots-67
  (:braille-dots-167 #x1002861)  ; u+2861 braille pattern dots-167
  (:braille-dots-267 #x1002862)  ; u+2862 braille pattern dots-267
  (:braille-dots-1267 #x1002863)  ; u+2863 braille pattern dots-1267
  (:braille-dots-367 #x1002864)  ; u+2864 braille pattern dots-367
  (:braille-dots-1367 #x1002865)  ; u+2865 braille pattern dots-1367
  (:braille-dots-2367 #x1002866)  ; u+2866 braille pattern dots-2367
  (:braille-dots-12367 #x1002867)  ; u+2867 braille pattern dots-12367
  (:braille-dots-467 #x1002868)  ; u+2868 braille pattern dots-467
  (:braille-dots-1467 #x1002869)  ; u+2869 braille pattern dots-1467
  (:braille-dots-2467 #x100286a)  ; u+286a braille pattern dots-2467
  (:braille-dots-12467 #x100286b)  ; u+286b braille pattern dots-12467
  (:braille-dots-3467 #x100286c)  ; u+286c braille pattern dots-3467
  (:braille-dots-13467 #x100286d)  ; u+286d braille pattern dots-13467
  (:braille-dots-23467 #x100286e)  ; u+286e braille pattern dots-23467
  (:braille-dots-123467 #x100286f)  ; u+286f braille pattern dots-123467
  (:braille-dots-567 #x1002870)  ; u+2870 braille pattern dots-567
  (:braille-dots-1567 #x1002871)  ; u+2871 braille pattern dots-1567
  (:braille-dots-2567 #x1002872)  ; u+2872 braille pattern dots-2567
  (:braille-dots-12567 #x1002873)  ; u+2873 braille pattern dots-12567
  (:braille-dots-3567 #x1002874)  ; u+2874 braille pattern dots-3567
  (:braille-dots-13567 #x1002875)  ; u+2875 braille pattern dots-13567
  (:braille-dots-23567 #x1002876)  ; u+2876 braille pattern dots-23567
  (:braille-dots-123567 #x1002877)  ; u+2877 braille pattern dots-123567
  (:braille-dots-4567 #x1002878)  ; u+2878 braille pattern dots-4567
  (:braille-dots-14567 #x1002879)  ; u+2879 braille pattern dots-14567
  (:braille-dots-24567 #x100287a)  ; u+287a braille pattern dots-24567
  (:braille-dots-124567 #x100287b)  ; u+287b braille pattern dots-124567
  (:braille-dots-34567 #x100287c)  ; u+287c braille pattern dots-34567
  (:braille-dots-134567 #x100287d)  ; u+287d braille pattern dots-134567
  (:braille-dots-234567 #x100287e)  ; u+287e braille pattern dots-234567
  (:braille-dots-1234567 #x100287f)  ; u+287f braille pattern dots-1234567
  (:braille-dots-8 #x1002880)  ; u+2880 braille pattern dots-8
  (:braille-dots-18 #x1002881)  ; u+2881 braille pattern dots-18
  (:braille-dots-28 #x1002882)  ; u+2882 braille pattern dots-28
  (:braille-dots-128 #x1002883)  ; u+2883 braille pattern dots-128
  (:braille-dots-38 #x1002884)  ; u+2884 braille pattern dots-38
  (:braille-dots-138 #x1002885)  ; u+2885 braille pattern dots-138
  (:braille-dots-238 #x1002886)  ; u+2886 braille pattern dots-238
  (:braille-dots-1238 #x1002887)  ; u+2887 braille pattern dots-1238
  (:braille-dots-48 #x1002888)  ; u+2888 braille pattern dots-48
  (:braille-dots-148 #x1002889)  ; u+2889 braille pattern dots-148
  (:braille-dots-248 #x100288a)  ; u+288a braille pattern dots-248
  (:braille-dots-1248 #x100288b)  ; u+288b braille pattern dots-1248
  (:braille-dots-348 #x100288c)  ; u+288c braille pattern dots-348
  (:braille-dots-1348 #x100288d)  ; u+288d braille pattern dots-1348
  (:braille-dots-2348 #x100288e)  ; u+288e braille pattern dots-2348
  (:braille-dots-12348 #x100288f)  ; u+288f braille pattern dots-12348
  (:braille-dots-58 #x1002890)  ; u+2890 braille pattern dots-58
  (:braille-dots-158 #x1002891)  ; u+2891 braille pattern dots-158
  (:braille-dots-258 #x1002892)  ; u+2892 braille pattern dots-258
  (:braille-dots-1258 #x1002893)  ; u+2893 braille pattern dots-1258
  (:braille-dots-358 #x1002894)  ; u+2894 braille pattern dots-358
  (:braille-dots-1358 #x1002895)  ; u+2895 braille pattern dots-1358
  (:braille-dots-2358 #x1002896)  ; u+2896 braille pattern dots-2358
  (:braille-dots-12358 #x1002897)  ; u+2897 braille pattern dots-12358
  (:braille-dots-458 #x1002898)  ; u+2898 braille pattern dots-458
  (:braille-dots-1458 #x1002899)  ; u+2899 braille pattern dots-1458
  (:braille-dots-2458 #x100289a)  ; u+289a braille pattern dots-2458
  (:braille-dots-12458 #x100289b)  ; u+289b braille pattern dots-12458
  (:braille-dots-3458 #x100289c)  ; u+289c braille pattern dots-3458
  (:braille-dots-13458 #x100289d)  ; u+289d braille pattern dots-13458
  (:braille-dots-23458 #x100289e)  ; u+289e braille pattern dots-23458
  (:braille-dots-123458 #x100289f)  ; u+289f braille pattern dots-123458
  (:braille-dots-68 #x10028a0)  ; u+28a0 braille pattern dots-68
  (:braille-dots-168 #x10028a1)  ; u+28a1 braille pattern dots-168
  (:braille-dots-268 #x10028a2)  ; u+28a2 braille pattern dots-268
  (:braille-dots-1268 #x10028a3)  ; u+28a3 braille pattern dots-1268
  (:braille-dots-368 #x10028a4)  ; u+28a4 braille pattern dots-368
  (:braille-dots-1368 #x10028a5)  ; u+28a5 braille pattern dots-1368
  (:braille-dots-2368 #x10028a6)  ; u+28a6 braille pattern dots-2368
  (:braille-dots-12368 #x10028a7)  ; u+28a7 braille pattern dots-12368
  (:braille-dots-468 #x10028a8)  ; u+28a8 braille pattern dots-468
  (:braille-dots-1468 #x10028a9)  ; u+28a9 braille pattern dots-1468
  (:braille-dots-2468 #x10028aa)  ; u+28aa braille pattern dots-2468
  (:braille-dots-12468 #x10028ab)  ; u+28ab braille pattern dots-12468
  (:braille-dots-3468 #x10028ac)  ; u+28ac braille pattern dots-3468
  (:braille-dots-13468 #x10028ad)  ; u+28ad braille pattern dots-13468
  (:braille-dots-23468 #x10028ae)  ; u+28ae braille pattern dots-23468
  (:braille-dots-123468 #x10028af)  ; u+28af braille pattern dots-123468
  (:braille-dots-568 #x10028b0)  ; u+28b0 braille pattern dots-568
  (:braille-dots-1568 #x10028b1)  ; u+28b1 braille pattern dots-1568
  (:braille-dots-2568 #x10028b2)  ; u+28b2 braille pattern dots-2568
  (:braille-dots-12568 #x10028b3)  ; u+28b3 braille pattern dots-12568
  (:braille-dots-3568 #x10028b4)  ; u+28b4 braille pattern dots-3568
  (:braille-dots-13568 #x10028b5)  ; u+28b5 braille pattern dots-13568
  (:braille-dots-23568 #x10028b6)  ; u+28b6 braille pattern dots-23568
  (:braille-dots-123568 #x10028b7)  ; u+28b7 braille pattern dots-123568
  (:braille-dots-4568 #x10028b8)  ; u+28b8 braille pattern dots-4568
  (:braille-dots-14568 #x10028b9)  ; u+28b9 braille pattern dots-14568
  (:braille-dots-24568 #x10028ba)  ; u+28ba braille pattern dots-24568
  (:braille-dots-124568 #x10028bb)  ; u+28bb braille pattern dots-124568
  (:braille-dots-34568 #x10028bc)  ; u+28bc braille pattern dots-34568
  (:braille-dots-134568 #x10028bd)  ; u+28bd braille pattern dots-134568
  (:braille-dots-234568 #x10028be)  ; u+28be braille pattern dots-234568
  (:braille-dots-1234568 #x10028bf)  ; u+28bf braille pattern dots-1234568
  (:braille-dots-78 #x10028c0)  ; u+28c0 braille pattern dots-78
  (:braille-dots-178 #x10028c1)  ; u+28c1 braille pattern dots-178
  (:braille-dots-278 #x10028c2)  ; u+28c2 braille pattern dots-278
  (:braille-dots-1278 #x10028c3)  ; u+28c3 braille pattern dots-1278
  (:braille-dots-378 #x10028c4)  ; u+28c4 braille pattern dots-378
  (:braille-dots-1378 #x10028c5)  ; u+28c5 braille pattern dots-1378
  (:braille-dots-2378 #x10028c6)  ; u+28c6 braille pattern dots-2378
  (:braille-dots-12378 #x10028c7)  ; u+28c7 braille pattern dots-12378
  (:braille-dots-478 #x10028c8)  ; u+28c8 braille pattern dots-478
  (:braille-dots-1478 #x10028c9)  ; u+28c9 braille pattern dots-1478
  (:braille-dots-2478 #x10028ca)  ; u+28ca braille pattern dots-2478
  (:braille-dots-12478 #x10028cb)  ; u+28cb braille pattern dots-12478
  (:braille-dots-3478 #x10028cc)  ; u+28cc braille pattern dots-3478
  (:braille-dots-13478 #x10028cd)  ; u+28cd braille pattern dots-13478
  (:braille-dots-23478 #x10028ce)  ; u+28ce braille pattern dots-23478
  (:braille-dots-123478 #x10028cf)  ; u+28cf braille pattern dots-123478
  (:braille-dots-578 #x10028d0)  ; u+28d0 braille pattern dots-578
  (:braille-dots-1578 #x10028d1)  ; u+28d1 braille pattern dots-1578
  (:braille-dots-2578 #x10028d2)  ; u+28d2 braille pattern dots-2578
  (:braille-dots-12578 #x10028d3)  ; u+28d3 braille pattern dots-12578
  (:braille-dots-3578 #x10028d4)  ; u+28d4 braille pattern dots-3578
  (:braille-dots-13578 #x10028d5)  ; u+28d5 braille pattern dots-13578
  (:braille-dots-23578 #x10028d6)  ; u+28d6 braille pattern dots-23578
  (:braille-dots-123578 #x10028d7)  ; u+28d7 braille pattern dots-123578
  (:braille-dots-4578 #x10028d8)  ; u+28d8 braille pattern dots-4578
  (:braille-dots-14578 #x10028d9)  ; u+28d9 braille pattern dots-14578
  (:braille-dots-24578 #x10028da)  ; u+28da braille pattern dots-24578
  (:braille-dots-124578 #x10028db)  ; u+28db braille pattern dots-124578
  (:braille-dots-34578 #x10028dc)  ; u+28dc braille pattern dots-34578
  (:braille-dots-134578 #x10028dd)  ; u+28dd braille pattern dots-134578
  (:braille-dots-234578 #x10028de)  ; u+28de braille pattern dots-234578
  (:braille-dots-1234578 #x10028df)  ; u+28df braille pattern dots-1234578
  (:braille-dots-678 #x10028e0)  ; u+28e0 braille pattern dots-678
  (:braille-dots-1678 #x10028e1)  ; u+28e1 braille pattern dots-1678
  (:braille-dots-2678 #x10028e2)  ; u+28e2 braille pattern dots-2678
  (:braille-dots-12678 #x10028e3)  ; u+28e3 braille pattern dots-12678
  (:braille-dots-3678 #x10028e4)  ; u+28e4 braille pattern dots-3678
  (:braille-dots-13678 #x10028e5)  ; u+28e5 braille pattern dots-13678
  (:braille-dots-23678 #x10028e6)  ; u+28e6 braille pattern dots-23678
  (:braille-dots-123678 #x10028e7)  ; u+28e7 braille pattern dots-123678
  (:braille-dots-4678 #x10028e8)  ; u+28e8 braille pattern dots-4678
  (:braille-dots-14678 #x10028e9)  ; u+28e9 braille pattern dots-14678
  (:braille-dots-24678 #x10028ea)  ; u+28ea braille pattern dots-24678
  (:braille-dots-124678 #x10028eb)  ; u+28eb braille pattern dots-124678
  (:braille-dots-34678 #x10028ec)  ; u+28ec braille pattern dots-34678
  (:braille-dots-134678 #x10028ed)  ; u+28ed braille pattern dots-134678
  (:braille-dots-234678 #x10028ee)  ; u+28ee braille pattern dots-234678
  (:braille-dots-1234678 #x10028ef)  ; u+28ef braille pattern dots-1234678
  (:braille-dots-5678 #x10028f0)  ; u+28f0 braille pattern dots-5678
  (:braille-dots-15678 #x10028f1)  ; u+28f1 braille pattern dots-15678
  (:braille-dots-25678 #x10028f2)  ; u+28f2 braille pattern dots-25678
  (:braille-dots-125678 #x10028f3)  ; u+28f3 braille pattern dots-125678
  (:braille-dots-35678 #x10028f4)  ; u+28f4 braille pattern dots-35678
  (:braille-dots-135678 #x10028f5)  ; u+28f5 braille pattern dots-135678
  (:braille-dots-235678 #x10028f6)  ; u+28f6 braille pattern dots-235678
  (:braille-dots-1235678 #x10028f7)  ; u+28f7 braille pattern dots-1235678
  (:braille-dots-45678 #x10028f8)  ; u+28f8 braille pattern dots-45678
  (:braille-dots-145678 #x10028f9)  ; u+28f9 braille pattern dots-145678
  (:braille-dots-245678 #x10028fa)  ; u+28fa braille pattern dots-245678
  (:braille-dots-1245678 #x10028fb)  ; u+28fb braille pattern dots-1245678
  (:braille-dots-345678 #x10028fc)  ; u+28fc braille pattern dots-345678
  (:braille-dots-1345678 #x10028fd)  ; u+28fd braille pattern dots-1345678
  (:braille-dots-2345678 #x10028fe)  ; u+28fe braille pattern dots-2345678
  (:braille-dots-12345678 #x10028ff)  ; u+28ff braille pattern dots-12345678

  ;;; XFree86 vendor specific keysyms #x10080001 - #x1008FFFF
  (:XF86XK-Mode-Lock #x1008FF01) ; Mode Switch Lock
  ;; Backlight controls.
  (:XF86XK-Mon-Brightness-Up #x1008FF02) ; Monitor/panel brightness
  (:XF86XK-Mon-Brightness-Down #x1008FF03) ; Monitor/panel brightness
  (:XF86XK-Kbd-Light-On-Off #x1008FF04) ; Keyboards may be lit
  (:XF86XK-Kbd-Brightness-Up #x1008FF05) ; Keyboards may be lit
  (:XF86XK-Kbd-Brightness-Down #x1008FF06) ; Keyboards may be lit

  ;; Keys found on some "Internet" keyboards.
  (:XF86XK-Standby #x1008FF10) ; System into standby mode
  (:XF86XK-Audio-Lower-Volume #x1008FF11) ; Volume control down
  (:XF86XK-Audio-Mute #x1008FF12) ; Mute sound from the system
  (:XF86XK-Audio-Raise-Volume #x1008FF13) ; Volume control up
  (:XF86XK-Audio-Play #x1008FF14) ; Start playing of audio >
  (:XF86XK-Audio-Stop #x1008FF15) ; Stop playing audio
  (:XF86XK-Audio-Prev #x1008FF16) ; Previous track
  (:XF86XK-Audio-Next #x1008FF17) ; Next track
  (:XF86XK-Home-Page #x1008FF18) ; Display user's home page
  (:XF86XK-Mail #x1008FF19) ; Invoke user's mail program
  (:XF86XK-Start #x1008FF1A) ; Start application
  (:XF86XK-Search #x1008FF1B) ; Search
  (:XF86XK-Audio-Record #x1008FF1C) ; Record audio application

  ;; These are sometimes found on PDA's (e.g. Palm, PocketPC or elsewhere)
  (:XF86XK-Calculator #x1008FF1D) ; Invoke calculator program
  (:XF86XK-Memo #x1008FF1E) ; Invoke Memo taking program
  (:XF86XK-To-Do-List #x1008FF1F) ; Invoke To Do List program
  (:XF86XK-Calendar #x1008FF20) ; Invoke Calendar program
  (:XF86XK-Power-Down #x1008FF21) ; Deep sleep the system
  (:XF86XK-Contrast-Adjust #x1008FF22) ; Adjust screen contrast
  (:XF86XK-Rocker-Up #x1008FF23) ; Rocker switches exist up
  (:XF86XK-Rocker-Down #x1008FF24) ; and down
  (:XF86XK-Rocker-Enter #x1008FF25) ; and let you press them

  ;; Some more "Internet" keyboard symbols
  (:XF86XK-Back #x1008FF26) ; Like back on a browser
  (:XF86XK-Forward #x1008FF27) ; Like forward on a browser
  (:XF86XK-Stop #x1008FF28) ; Stop current operation
  (:XF86XK-Refresh #x1008FF29) ; Refresh the page
  (:XF86XK-Power-Off #x1008FF2A) ; Power off system entirely
  (:XF86XK-Wake-Up #x1008FF2B) ; Wake up system from sleep
  (:XF86XK-Eject #x1008FF2C) ; Eject device (e.g. DVD)
  (:XF86XK-Screen-Saver #x1008FF2D) ; Invoke screensaver
  (:XF86XK-WWW #x1008FF2E) ; Invoke web browser
  (:XF86XK-Sleep #x1008FF2F) ; Put system to sleep
  (:XF86XK-Favorites #x1008FF30) ; Show favorite locations
  (:XF86XK-Audio-Pause #x1008FF31) ; Pause audio playing
  (:XF86XK-Audio-Media #x1008FF32) ; Launch media collection app
  (:XF86XK-My-Computer #x1008FF33) ; Display "My Computer" window
  (:XF86XK-Vendor-Home #x1008FF34) ; Display vendor home web site
  (:XF86XK-Light-Bulb #x1008FF35) ; Light bulb keys exist
  (:XF86XK-Shop #x1008FF36) ; Display shopping web site
  (:XF86XK-History #x1008FF37) ; Show history of web surfing
  (:XF86XK-Open-URL #x1008FF38) ; Open selected URL
  (:XF86XK-Add-Favorite #x1008FF39) ; Add URL to favorites list
  (:XF86XK-Hot-Links #x1008FF3A) ; Show "hot" links
  (:XF86XK-Brightness-Adjust #x1008FF3B) ; Invoke brightness adj. UI
  (:XF86XK-Finance #x1008FF3C) ; Display financial site
  (:XF86XK-Community #x1008FF3D) ; Display user's community
  (:XF86XK-Audio-Rewind #x1008FF3E) ; "rewind" audio track
  (:XF86XK-Back-Forward #x1008FF3F) ; ???
  (:XF86XK-Launch0 #x1008FF40) ; Launch Application
  (:XF86XK-Launch1 #x1008FF41) ; Launch Application
  (:XF86XK-Launch2 #x1008FF42) ; Launch Application
  (:XF86XK-Launch3 #x1008FF43) ; Launch Application
  (:XF86XK-Launch4 #x1008FF44) ; Launch Application
  (:XF86XK-Launch5 #x1008FF45) ; Launch Application
  (:XF86XK-Launch6 #x1008FF46) ; Launch Application
  (:XF86XK-Launch7 #x1008FF47) ; Launch Application
  (:XF86XK-Launch8 #x1008FF48) ; Launch Application
  (:XF86XK-Launch9 #x1008FF49) ; Launch Application
  (:XF86XK-LaunchA #x1008FF4A) ; Launch Application
  (:XF86XK-LaunchB #x1008FF4B) ; Launch Application
  (:XF86XK-LaunchC #x1008FF4C) ; Launch Application
  (:XF86XK-LaunchD #x1008FF4D) ; Launch Application
  (:XF86XK-LaunchE #x1008FF4E) ; Launch Application
  (:XF86XK-LaunchF #x1008FF4F) ; Launch Application

  (:XF86XK-Application-Left #x1008FF50) ; switch to application, left
  (:XF86XK-Application-Right #x1008FF51) ; switch to application, righ
  (:XF86XK-Book #x1008FF52) ; Launch bookreader
  (:XF86XK-CD #x1008FF53) ; Launch CD/DVD player
  (:XF86XK-Calculater #x1008FF54) ; Launch Calculater
  (:XF86XK-Clear #x1008FF55) ; Clear window, screen
  (:XF86XK-Close #x1008FF56) ; Close window
  (:XF86XK-Copy #x1008FF57) ; Copy selection
  (:XF86XK-Cut #x1008FF58) ; Cut selection
  (:XF86XK-Display #x1008FF59) ; Output switch key
  (:XF86XK-DOS #x1008FF5A) ; Launch DOS (emulation)
  (:XF86XK-Documents #x1008FF5B) ; Open documents window
  (:XF86XK-Excel #x1008FF5C) ; Launch spread sheet
  (:XF86XK-Explorer #x1008FF5D) ; Launch file explorer
  (:XF86XK-Game #x1008FF5E) ; Launch game
  (:XF86XK-Go #x1008FF5F) ; Go to URL
  (:XF86XK-iTouch #x1008FF60) ; Logitch iTouch- don't use
  (:XF86XK-Log-Off #x1008FF61) ; Log off system
  (:XF86XK-Market #x1008FF62) ; ??
  (:XF86XK-Meeting #x1008FF63) ; enter meeting in calendar
  (:XF86XK-Menu-KB #x1008FF65) ; distingush keyboard from PB
  (:XF86XK-Menu-PB #x1008FF66) ; distinuish PB from keyboard
  (:XF86XK-MySites #x1008FF67) ; Favourites
  (:XF86XK-New #x1008FF68) ; folder, document...    */
  (:XF86XK-News #x1008FF69) ; News
  (:XF86XK-Office-Home #x1008FF6A) ; Office home (old Staroffice
  (:XF86XK-Open #x1008FF6B) ; Open
  (:XF86XK-Option #x1008FF6C) ; ??
  (:XF86XK-Paste #x1008FF6D) ; Paste
  (:XF86XK-Phone #x1008FF6E) ; Launch phone                ; dial number
  (:XF86XK-Q #x1008FF70) ; Compaq's Q - don't use
  (:XF86XK-Reply #x1008FF72) ; Reply e.g., mail
  (:XF86XK-Reload #x1008FF73) ; Reload web page, file, etc.
  (:XF86XK-Rotate-Windows #x1008FF74) ; Rotate windows e.g. xrandr
  (:XF86XK-Rotation-PB #x1008FF75) ; don't use
  (:XF86XK-Rotation-KB #x1008FF76) ; don't use
  (:XF86XK-Save #x1008FF77) ; Save (file, document, state */
  (:XF86XK-Scroll-Up #x1008FF78) ; Scroll window/contents up
  (:XF86XK-Scroll-Down #x1008FF79) ; Scrool window/contentd down
  (:XF86XK-Scroll-Click #x1008FF7A) ; Use XKB mousekeys instead
  (:XF86XK-Send #x1008FF7B) ; Send mail, file, object
  (:XF86XK-Spell #x1008FF7C) ; Spell checker
  (:XF86XK-Split-Screen #x1008FF7D) ; Split window or screen
  (:XF86XK-Support #x1008FF7E) ; Get support (??)
  (:XF86XK-Task-Pane #x1008FF7F) ; Show tasks
  (:XF86XK-Terminal #x1008FF80) ; Launch terminal emulator
  (:XF86XK-Tools #x1008FF81) ; toolbox of desktop/app.
  (:XF86XK-Travel #x1008FF82) ; ??
  (:XF86XK-User-PB #x1008FF84) ; ??
  (:XF86XK-User1-KB #x1008FF85) ; ??
  (:XF86XK-User2-KB #x1008FF86) ; ??
  (:XF86XK-Video #x1008FF87) ; Launch video player
  (:XF86XK-Wheel-Button #x1008FF88) ; button from a mouse wheel
  (:XF86XK-Word #x1008FF89) ; Launch word processor
  (:XF86XK-Xfer #x1008FF8A)
  (:XF86XK-Zoom-In #x1008FF8B) ; zoom in view, map, etc.
  (:XF86XK-Zoom-Out #x1008FF8C) ; zoom out view, map, etc.

  (:XF86XK-Away #x1008FF8D) ; mark yourself as away
  (:XF86XK-Messenger #x1008FF8E) ; as in instant messaging
  (:XF86XK-Web-Cam #x1008FF8F) ; Launch web camera app.
  (:XF86XK-Mail-Forward #x1008FF90) ; Forward in mail
  (:XF86XK-Pictures #x1008FF91) ; Show pictures
  (:XF86XK-Music #x1008FF92) ; Launch music application

  (:XF86XK-Battery #x1008FF93) ; Display battery information
  (:XF86XK-Bluetooth #x1008FF94) ; Enable/disable Bluetooth
  (:XF86XK-WLAN #x1008FF95) ; Enable/disable WLAN
  (:XF86XK-UWB #x1008FF96) ; Enable/disable UWB

  (:XF86XK-Audio-Forward #x1008FF97) ; fast-forward audio track
  (:XF86XK-Audio-Repeat #x1008FF98) ; toggle repeat mode
  (:XF86XK-Audio-Random-Play #x1008FF99) ; toggle shuffle mode
  (:XF86XK-Subtitle #x1008FF9A) ; cycle through subtitle
  (:XF86XK-Audio-Cycle-Track #x1008FF9B) ; cycle through audio tracks
  (:XF86XK-Cycle-Angle #x1008FF9C) ; cycle through angles
  (:XF86XK-Frame-Back #x1008FF9D) ; video: go one frame back
  (:XF86XK-Frame-Forward #x1008FF9E) ; video: go one frame forward
  (:XF86XK-Time #x1008FF9F) ; display, or shows an entry for time seeking
  (:XF86XK-Select #x1008FFA0) ; Select button on joypads and remotes
  (:XF86XK-View #x1008FFA1) ; Show a view options/properties
  (:XF86XK-Top-Menu #x1008FFA2) ; Go to a top-level menu in a video

  (:XF86XK-Red #x1008FFA3) ; Red button
  (:XF86XK-Green #x1008FFA4) ; Green button
  (:XF86XK-Yellow #x1008FFA5) ; Yellow button
  (:XF86XK-Blue #x1008FFA6) ; Blue button

  (:XF86XK-Suspend #x1008FFA7) ; Sleep to RAM
  (:XF86XK-Hibernate #x1008FFA8) ; Sleep to disk
  (:XF86XK-Touchpad-Toggle #x1008FFA9) ; Toggle between touchpad/trackstick
  (:XF86XK-Touchpad-On #x1008FFB0) ; The touchpad got switched on
  (:XF86XK-Touchpad-Off #x1008FFB1) ; The touchpad got switched off

  ;; Keys for special action keys (hot keys)
  ;; Virtual terminals on some operating systems
  (:XF86XK-Switch-VT-1 #x1008FE01)
  (:XF86XK-Switch-VT-2 #x1008FE02)
  (:XF86XK-Switch-VT-3 #x1008FE03)
  (:XF86XK-Switch-VT-4 #x1008FE04)
  (:XF86XK-Switch-VT-5 #x1008FE05)
  (:XF86XK-Switch-VT-6 #x1008FE06)
  (:XF86XK-Switch-VT-7 #x1008FE07)
  (:XF86XK-Switch-VT-8 #x1008FE08)
  (:XF86XK-Switch-VT-9 #x1008FE09)
  (:XF86XK-Switch-VT-10 #x1008FE0A)
  (:XF86XK-Switch-VT-11 #x1008FE0B)
  (:XF86XK-Switch-VT-12 #x1008FE0C)

  (:XF86XK-Ungrab #x1008FE20) ; force ungrab
  (:XF86XK-Clear-Grab #x1008FE21) ; kill application with grab
  (:XF86XK-Next-Video-Mode #x1008FE22) ; next video mode available
  (:XF86XK-Prev-Video-Mode #x1008FE23) ; prev. video mode available
  (:XF86XK-Log-Window-Tree #x1008FE24) ; print window tree to log
  (:XF86XK-Log-Grab-Info #x1008FE25) ; print all active grabs to log
  )
