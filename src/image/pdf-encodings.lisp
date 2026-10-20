(in-package #:borba-cad/image)

;;;; The names of glyphs and the encodings that PDF fonts use (ISO 32000-1, Annex D): the Adobe glyph names of the
;;;; Latin letters, punctuation and symbols, the Unicode value of each, and the tables that say which glyph a code of
;;;; a simple font stands for in StandardEncoding, WinAnsiEncoding and MacRomanEncoding.

(defparameter *glyph-unicode-names*
  '(("space" . 32) ("exclam" . 33) ("quotedbl" . 34) ("numbersign" . 35) ("dollar" . 36) ("percent" . 37)
    ("ampersand" . 38) ("quotesingle" . 39) ("parenleft" . 40) ("parenright" . 41) ("asterisk" . 42) ("plus" . 43)
    ("comma" . 44) ("hyphen" . 45) ("period" . 46) ("slash" . 47) ("zero" . 48) ("one" . 49) ("two" . 50)
    ("three" . 51) ("four" . 52) ("five" . 53) ("six" . 54) ("seven" . 55) ("eight" . 56) ("nine" . 57)
    ("colon" . 58) ("semicolon" . 59) ("less" . 60) ("equal" . 61) ("greater" . 62) ("question" . 63) ("at" . 64)
    ("bracketleft" . 91) ("backslash" . 92) ("bracketright" . 93) ("asciicircum" . 94) ("underscore" . 95)
    ("grave" . 96) ("braceleft" . 123) ("bar" . 124) ("braceright" . 125) ("asciitilde" . 126)
    ("exclamdown" . 161) ("cent" . 162) ("sterling" . 163) ("currency" . 164) ("yen" . 165) ("brokenbar" . 166)
    ("section" . 167) ("dieresis" . 168) ("copyright" . 169) ("ordfeminine" . 170) ("guillemotleft" . 171)
    ("logicalnot" . 172) ("registered" . 174) ("macron" . 175) ("degree" . 176) ("plusminus" . 177)
    ("twosuperior" . 178) ("threesuperior" . 179) ("acute" . 180) ("mu" . 181) ("paragraph" . 182)
    ("periodcentered" . 183) ("cedilla" . 184) ("onesuperior" . 185) ("ordmasculine" . 186)
    ("guillemotright" . 187) ("onequarter" . 188) ("onehalf" . 189) ("threequarters" . 190)
    ("questiondown" . 191) ("Agrave" . 192) ("Aacute" . 193) ("Acircumflex" . 194) ("Atilde" . 195)
    ("Adieresis" . 196) ("Aring" . 197) ("AE" . 198) ("Ccedilla" . 199) ("Egrave" . 200) ("Eacute" . 201)
    ("Ecircumflex" . 202) ("Edieresis" . 203) ("Igrave" . 204) ("Iacute" . 205) ("Icircumflex" . 206)
    ("Idieresis" . 207) ("Eth" . 208) ("Ntilde" . 209) ("Ograve" . 210) ("Oacute" . 211) ("Ocircumflex" . 212)
    ("Otilde" . 213) ("Odieresis" . 214) ("multiply" . 215) ("Oslash" . 216) ("Ugrave" . 217) ("Uacute" . 218)
    ("Ucircumflex" . 219) ("Udieresis" . 220) ("Yacute" . 221) ("Thorn" . 222) ("germandbls" . 223)
    ("agrave" . 224) ("aacute" . 225) ("acircumflex" . 226) ("atilde" . 227) ("adieresis" . 228) ("aring" . 229)
    ("ae" . 230) ("ccedilla" . 231) ("egrave" . 232) ("eacute" . 233) ("ecircumflex" . 234) ("edieresis" . 235)
    ("igrave" . 236) ("iacute" . 237) ("icircumflex" . 238) ("idieresis" . 239) ("eth" . 240) ("ntilde" . 241)
    ("ograve" . 242) ("oacute" . 243) ("ocircumflex" . 244) ("otilde" . 245) ("odieresis" . 246) ("divide" . 247)
    ("oslash" . 248) ("ugrave" . 249) ("uacute" . 250) ("ucircumflex" . 251) ("udieresis" . 252) ("yacute" . 253)
    ("thorn" . 254) ("ydieresis" . 255) ("Euro" . 8364) ("quotesinglbase" . 8218) ("florin" . 402)
    ("quotedblbase" . 8222) ("ellipsis" . 8230) ("dagger" . 8224) ("daggerdbl" . 8225) ("circumflex" . 710)
    ("perthousand" . 8240) ("Scaron" . 352) ("guilsinglleft" . 8249) ("OE" . 338) ("Zcaron" . 381)
    ("quoteleft" . 8216) ("quoteright" . 8217) ("quotedblleft" . 8220) ("quotedblright" . 8221) ("bullet" . 8226)
    ("endash" . 8211) ("emdash" . 8212) ("tilde" . 732) ("trademark" . 8482) ("scaron" . 353)
    ("guilsinglright" . 8250) ("oe" . 339) ("zcaron" . 382) ("Ydieresis" . 376) ("fi" . 64257) ("fl" . 64258)
    ("fraction" . 8260) ("minus" . 8722) ("dotlessi" . 305) ("Lslash" . 321) ("lslash" . 322) ("breve" . 728)
    ("caron" . 711) ("dotaccent" . 729) ("hungarumlaut" . 733) ("ogonek" . 731) ("ring" . 730)
    ("notequal" . 8800) ("infinity" . 8734) ("lessequal" . 8804) ("greaterequal" . 8805) ("partialdiff" . 8706)
    ("summation" . 8721) ("product" . 8719) ("pi" . 960) ("integral" . 8747) ("Omega" . 937) ("radical" . 8730)
    ("approxequal" . 8776) ("Delta" . 8710) ("lozenge" . 9674) ("apple" . 63743))
  "The glyph names that are not a single letter or digit, with their Unicode values.")

(defparameter *name-unicode*
  (let ((table (make-hash-table :test 'equal)))
    (loop for code from 65 to 90
          do (setf (gethash (string (code-char code)) table) code
                   (gethash (string (code-char (+ code 32))) table) (+ code 32)))
    (loop for (name . code) in *glyph-unicode-names*
          do (setf (gethash name table) code))
    table)
  "A table from the name of a glyph to its Unicode value.")

(defparameter *unicode-name*
  (let ((table (make-hash-table)))
    (loop for code from 65 to 90
          do (setf (gethash code table) (string (code-char code))
                   (gethash (+ code 32) table) (string (code-char (+ code 32)))))
    (loop for (name . code) in (reverse *glyph-unicode-names*)
          do (setf (gethash code table) name))
    (setf (gethash 160 table) "space"
          (gethash 173 table) "hyphen")
    table)
  "A table from a Unicode value to the name of its glyph.")

(defun glyph-name-unicode (name)
  "Return the Unicode value that the name of a glyph stands for, or NIL. The names of the Latin letters and symbols are
known, and the names made of uni and four hexadecimal digits, of u and four to six, and of a name and a suffix after a
dot are read as the standard says.

NAME: the name of the glyph."
  (or (gethash name *name-unicode*)
      (let ((dot (position #\. name)))
        (and dot (plusp dot) (glyph-name-unicode (subseq name 0 dot))))
      (and (> (length name) 6)
           (string= "uni" name :end2 3)
           (parse-integer name :start 3 :end 7 :radix 16 :junk-allowed t))
      (and (<= 5 (length name) 7)
           (char= #\u (char name 0))
           (parse-integer name :start 1 :radix 16 :junk-allowed t))))

(defun unicode-names-vector (codes)
  "Return a vector of 256 glyph names, one for each of CODES, the Unicode value of the glyph each code stands for, or
NIL.

CODES: a list of 256 Unicode values, or NIL for a code with no glyph."
  (let ((names (make-array 256 :initial-element nil)))
    (loop for code in codes
          for index from 0
          do (when code
               (setf (svref names index) (gethash code *unicode-name*))))
    names))

(defun latin-1-codes ()
  "Return the Unicode values of the 256 codes of Latin-1: each code is its own value."
  (loop for code below 256
        collect (and (>= code 32) (/= code 127) code)))

(defparameter *winansi-encoding*
  (unicode-names-vector
   (let ((codes (coerce (latin-1-codes) 'simple-vector)))
     (loop for (code . unicode) in '((128 . 8364) (130 . 8218) (131 . 402) (132 . 8222) (133 . 8230) (134 . 8224)
                                     (135 . 8225) (136 . 710) (137 . 8240) (138 . 352) (139 . 8249) (140 . 338)
                                     (142 . 381) (145 . 8216) (146 . 8217) (147 . 8220) (148 . 8221) (149 . 8226)
                                     (150 . 8211) (151 . 8212) (152 . 732) (153 . 8482) (154 . 353) (155 . 8250)
                                     (156 . 339) (158 . 382) (159 . 376))
           do (setf (svref codes code) unicode))
     (dolist (code '(129 141 143 144 157))
       (setf (svref codes code) 8226))
     (setf (svref codes 39) 39
           (svref codes 96) 96)
     (coerce codes 'list)))
  "The glyph names of WinAnsiEncoding, by code.")

(defparameter *standard-encoding*
  (let ((names (make-array 256 :initial-element nil)))
    (loop for code from 32 to 126
          do (setf (svref names code) (gethash code *unicode-name*)))
    (setf (svref names 39) "quoteright"
          (svref names 96) "quoteleft")
    (loop for (code . name) in '((161 . "exclamdown") (162 . "cent") (163 . "sterling") (164 . "fraction")
                                 (165 . "yen") (166 . "florin") (167 . "section") (168 . "currency")
                                 (169 . "quotesingle") (170 . "quotedblleft") (171 . "guillemotleft")
                                 (172 . "guilsinglleft") (173 . "guilsinglright") (174 . "fi") (175 . "fl")
                                 (177 . "endash") (178 . "dagger") (179 . "daggerdbl") (180 . "periodcentered")
                                 (182 . "paragraph") (183 . "bullet") (184 . "quotesinglbase")
                                 (185 . "quotedblbase") (186 . "quotedblright") (187 . "guillemotright")
                                 (188 . "ellipsis") (189 . "perthousand") (191 . "questiondown") (193 . "grave")
                                 (194 . "acute") (195 . "circumflex") (196 . "tilde") (197 . "macron")
                                 (198 . "breve") (199 . "dotaccent") (200 . "dieresis") (202 . "ring")
                                 (203 . "cedilla") (205 . "hungarumlaut") (206 . "ogonek") (207 . "caron")
                                 (208 . "emdash") (225 . "AE") (227 . "ordfeminine") (232 . "Lslash")
                                 (233 . "Oslash") (234 . "OE") (235 . "ordmasculine") (241 . "ae")
                                 (245 . "dotlessi") (248 . "lslash") (249 . "oslash") (250 . "oe")
                                 (251 . "germandbls"))
          do (setf (svref names code) name))
    names)
  "The glyph names of StandardEncoding, by code.")

(defparameter *mac-roman-encoding*
  (unicode-names-vector
   (append (loop for code below 128 collect (and (>= code 32) (/= code 127) code))
           '(196 197 199 201 209 214 220 225 224 226 228 227 229 231 233 232 234 235 237 236 238 239 241 243 242 244
             246 245 250 249 251 252 8224 176 162 163 167 8226 182 223 174 169 8482 180 168 8800 198 216 8734 177
             8804 8805 165 181 8706 8721 8719 960 8747 170 186 937 230 248 191 161 172 8730 402 8776 8710 171 187
             8230 160 192 195 213 338 339 8211 8212 8220 8221 8216 8217 247 9674 255 376 8260 8364 8249 8250 64257
             64258 8225 183 8218 8222 8240 194 202 193 203 200 205 206 207 204 211 212 63743 210 218 219 217 305 710
             732 175 728 729 730 184 733 731 711)))
  "The glyph names of MacRomanEncoding, by code.")

(defun named-encoding (name)
  "Return the vector of glyph names of an encoding the PDF names, or NIL for a name that is not one of those read.

NAME: WinAnsiEncoding, MacRomanEncoding or StandardEncoding."
  (cond ((string= name "WinAnsiEncoding") *winansi-encoding*)
        ((string= name "MacRomanEncoding") *mac-roman-encoding*)
        ((string= name "StandardEncoding") *standard-encoding*)))
