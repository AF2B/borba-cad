(in-package #:borba-cad/image)

;;;; The outlines of a Compact Font Format font (Adobe Technical Note 5176), which PDF files hold as Type1C and
;;;; CIDFontType0C fonts and as the CFF table of an OpenType font: the indexes and dictionaries, the character sets,
;;;; and the Type 2 charstrings that draw the glyphs, with their subroutines, hints, flex curves and the accents of
;;;; composite glyphs. A glyph comes out as the same commands the TrueType glyphs do.

(defconstant +cff-maximum-subroutine-depth+ 10
  "How deep subroutines may call each other in a charstring.")

(defconstant +cff-maximum-stack+ 96
  "How many numbers the stack of a charstring may hold.")

(defconstant +cff-maximum-operations+ 100000
  "How many operators a charstring may run before it is called endless.")

(defparameter *cff-standard-strings*
  (coerce (let ((names (uiop:split-string
                        (concatenate 'string
                                     ".notdef space exclam quotedbl numbersign dollar percent ampersand quoteright "
                                     "parenleft parenright asterisk plus comma hyphen period slash zero one two three "
                                     "four five six seven eight nine colon semicolon less equal greater question at "
                                     "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z bracketleft backslash "
                                     "bracketright asciicircum underscore quoteleft a b c d e f g h i j k l m n o p q "
                                     "r s t u v w x y z braceleft bar braceright asciitilde exclamdown cent sterling "
                                     "fraction yen florin section currency quotesingle quotedblleft guillemotleft "
                                     "guilsinglleft guilsinglright fi fl endash dagger daggerdbl periodcentered "
                                     "paragraph bullet quotesinglbase quotedblbase quotedblright guillemotright "
                                     "ellipsis perthousand questiondown grave acute circumflex tilde macron breve "
                                     "dotaccent dieresis ring cedilla hungarumlaut ogonek caron emdash AE ordfeminine "
                                     "Lslash Oslash OE ordmasculine ae dotlessi lslash oslash oe germandbls "
                                     "onesuperior logicalnot mu trademark Eth onehalf plusminus Thorn onequarter "
                                     "divide brokenbar degree thorn threequarters twosuperior registered minus eth "
                                     "multiply threesuperior copyright Aacute Acircumflex Adieresis Agrave Aring "
                                     "Atilde Ccedilla Eacute Ecircumflex Edieresis Egrave Iacute Icircumflex "
                                     "Idieresis Igrave Ntilde Oacute Ocircumflex Odieresis Ograve Otilde Scaron "
                                     "Uacute Ucircumflex Udieresis Ugrave Yacute Ydieresis Zcaron aacute acircumflex "
                                     "adieresis agrave aring atilde ccedilla eacute ecircumflex edieresis egrave "
                                     "iacute icircumflex idieresis igrave ntilde oacute ocircumflex odieresis ograve "
                                     "otilde scaron uacute ucircumflex udieresis ugrave yacute ydieresis zcaron")
                        :separator " ")))
            names)
          'simple-vector)
  "The names that the first 229 string identifiers of a CFF font stand for.")

(defconstant +cff-standard-string-count+ 391
  "How many standard strings a CFF font has before its own begin.")

(defstruct (cff-font (:conc-name cff-))
  "A CFF font read: its bytes, its charstrings and subroutines, the names of its glyphs, and for a CID-keyed font the
private data of each of its font dictionaries.

DATA: the font file.
CHARSTRINGS: a vector of the positions and lengths of the charstrings, by glyph.
GLOBAL-SUBRS: the positions and lengths of the global subroutines.
STRINGS: the strings of the font, by index from the first that is not a standard one.
CHARSET: the string identifier, or the CID, of each glyph.
CID-KEYED: true for a font whose glyphs are told apart by CID.
DICTIONARIES: for each font dictionary, a list of the local subroutines, the default width and the nominal width.
FD-SELECT: which font dictionary each glyph uses, or NIL for a font that has only one.
MATRIX: the font matrix, six numbers.
ENCODING: the built-in encoding as an alist from codes to glyphs.
NAMES: a table from the name of a glyph to its number, made when first needed.
CACHE: the outlines made so far, by glyph."
  (data (make-octets 0) :type octets)
  (charstrings #() :type simple-vector)
  (global-subrs #() :type simple-vector)
  (strings #() :type simple-vector)
  (charset #() :type simple-vector)
  (cid-keyed nil)
  (dictionaries #() :type simple-vector)
  (fd-select nil)
  (matrix (list 0.001d0 0d0 0d0 0.001d0 0d0 0d0))
  (encoding '())
  (names nil)
  (cache (make-hash-table)))

(defun read-cff-index (data
                       position)
  "Read an INDEX of a CFF font, and return its items as a vector of (position . length) and the position after it.

DATA: the font file.
POSITION: where the INDEX begins."
  (let ((count (octet-word data position)))
    (if (zerop count)
        (values (vector) (+ position 2))
        (let* ((offset-size (aref data (+ position 2)))
               (offsets-start (+ position 3))
               (data-start (+ offsets-start (* offset-size (1+ count))))
               (items (make-array count)))
          (unless (<= 1 offset-size 4)
            (font-damaged))
          (flet ((offset-at (index)
                   (let ((value 0))
                     (dotimes (byte offset-size value)
                       (setf value (+ (* value 256) (aref data (+ offsets-start (* offset-size index) byte))))))))
            (dotimes (index count)
              (let ((from (+ data-start (offset-at index) -1))
                    (to (+ data-start (offset-at (1+ index)) -1)))
                (unless (and (<= from to) (<= to (length data)))
                  (font-damaged))
                (setf (svref items index) (cons from (- to from)))))
            (values items (+ data-start (offset-at count) -1)))))))

(defun parse-cff-real (text)
  "Return the number that the text of a real of a CFF dictionary holds: digits, a point, and an exponent written e,
with a sign when negative. Anything that is not a number is zero.

TEXT: the characters read from the nibbles."
  (let* ((exponent-at (position #\e text))
         (mantissa (subseq text 0 exponent-at))
         (exponent (if exponent-at (parse-integer text :start (1+ exponent-at) :junk-allowed t) 0))
         (negative (and (plusp (length mantissa)) (char= #\- (char mantissa 0))))
         (digits (string-left-trim "-" mantissa))
         (point (position #\. digits))
         (whole (parse-integer digits :end point :junk-allowed t))
         (fraction (if point (parse-integer digits :start (1+ point) :junk-allowed t) 0))
         (fraction-digits (if point (length (string-right-trim "" (subseq digits (1+ point)))) 0)))
    (if (and whole fraction exponent)
        (let ((value (* (+ whole (/ fraction (expt 10 fraction-digits))) (expt 10 exponent))))
          (float (if negative (- value) value) 1d0))
        0d0)))

(defun read-cff-dictionary (data
                            start
                            length)
  "Read a DICT of a CFF font as an alist from the operator to its list of operands. A two-byte operator is written as
1200 plus its second byte.

DATA: the font file.
START: where the DICT begins.
LENGTH: its length."
  (let ((position start)
        (end (+ start length))
        (operands '())
        (entries '()))
    (loop while (< position end)
          do (let ((byte (aref data position)))
               (incf position)
               (cond ((<= byte 21)
                      (let ((operator (if (= byte 12)
                                          (prog1 (+ 1200 (aref data position))
                                            (incf position))
                                          byte)))
                        (push (cons operator (nreverse operands)) entries)
                        (setf operands '())))
                     ((= byte 28)
                      (push (octet-signed-word data position) operands)
                      (incf position 2))
                     ((= byte 29)
                      (let ((value (octet-long data position)))
                        (push (if (>= value #x80000000) (- value #x100000000) value) operands))
                      (incf position 4))
                     ((= byte 30)
                      (let ((text '())
                            (done nil))
                        (loop until done
                              do (let ((pair (aref data position)))
                                   (incf position)
                                   (dolist (nibble (list (ash pair -4) (logand pair 15)))
                                     (case nibble
                                       (10 (push #\. text))
                                       (11 (push #\e text))
                                       (12 (push #\e text) (push #\- text))
                                       (14 (push #\- text))
                                       (15 (setf done t))
                                       (t (when (< nibble 10) (push (digit-char nibble) text)))))))
                        (push (parse-cff-real (coerce (nreverse text) 'string)) operands)))
                     ((<= 32 byte 246)
                      (push (- byte 139) operands))
                     ((<= 247 byte 250)
                      (push (+ (* (- byte 247) 256) (aref data position) 108) operands)
                      (incf position))
                     ((<= 251 byte 254)
                      (push (- (- (* (- byte 251) 256)) (aref data position) 108) operands)
                      (incf position))
                     (t
                      nil))))
    (nreverse entries)))

(defun dictionary-operands (dictionary
                            operator)
  "Return the operands of an operator in a DICT read by READ-CFF-DICTIONARY, or NIL.

DICTIONARY: the alist.
OPERATOR: the number of the operator."
  (cdr (assoc operator dictionary)))

(defun cff-string (font
                   identifier)
  "Return the string that a string identifier stands for, or NIL for one that has no name here.

FONT: the CFF-FONT.
IDENTIFIER: the string identifier."
  (cond ((< identifier (length *cff-standard-strings*))
         (svref *cff-standard-strings* identifier))
        ((< identifier +cff-standard-string-count+)
         nil)
        (t
         (let ((entry (and (< (- identifier +cff-standard-string-count+) (length (cff-strings font)))
                           (svref (cff-strings font) (- identifier +cff-standard-string-count+)))))
           (and entry
                (map 'string #'code-char (subseq (cff-data font) (car entry) (+ (car entry) (cdr entry)))))))))

(defun read-cff-charset (data
                         position
                         count)
  "Read the character set of a CFF font as a vector of the string identifier or the CID of each glyph.

DATA: the font file.
POSITION: where the charset is, or 0, 1 or 2 for the predefined ones.
COUNT: how many glyphs the font has."
  (let ((charset (make-array count :initial-element 0)))
    (if (<= position 2)
        (dotimes (index count)
          (setf (svref charset index) index))
        (let ((format (aref data position))
              (cursor (1+ position))
              (glyph 1))
          (case format
            (0 (loop while (< glyph count)
                     do (setf (svref charset glyph) (octet-word data cursor))
                        (incf cursor 2)
                        (incf glyph)))
            ((1 2)
             (loop while (< glyph count)
                   do (let* ((first (octet-word data cursor))
                             (left (if (= format 1) (aref data (+ cursor 2)) (octet-word data (+ cursor 2)))))
                        (incf cursor (if (= format 1) 3 4))
                        (dotimes (each (1+ left))
                          (when (< glyph count)
                            (setf (svref charset glyph) (+ first each))
                            (incf glyph))))))
            (t
             (font-damaged)))))
    charset))

(defun read-fd-select (data
                       position
                       count)
  "Read which font dictionary each glyph of a CID-keyed CFF font uses.

DATA: the font file.
POSITION: where the FDSelect is.
COUNT: how many glyphs the font has."
  (let ((selected (make-array count :element-type 'fixnum :initial-element 0))
        (format (aref data position)))
    (case format
      (0 (dotimes (index count)
           (setf (aref selected index) (aref data (+ position 1 index)))))
      (3 (let* ((ranges (octet-word data (+ position 1)))
                (cursor (+ position 3)))
           (dotimes (index ranges)
             (let ((first (octet-word data cursor))
                   (fd (aref data (+ cursor 2)))
                   (next (octet-word data (+ cursor 3))))
               (loop for glyph from first below (min next count)
                     do (setf (aref selected glyph) fd))
               (incf cursor 3)))))
      (t (font-damaged)))
    selected))

(defun read-private-dictionary (data
                                private)
  "Read the private dictionary of a CFF font and return its local subroutines, default width and nominal width as a
list.

DATA: the font file.
PRIVATE: the operands of the Private operator: the size and the position."
  (if (and private (= (length private) 2) (plusp (first private)))
      (let* ((start (second private))
             (dictionary (read-cff-dictionary data
                                              start
                                              (first private)))
             (subrs-offset (first (dictionary-operands dictionary 19)))
             (default (or (first (dictionary-operands dictionary 20)) 0))
             (nominal (or (first (dictionary-operands dictionary 21)) 0)))
        (list (if subrs-offset
                  (read-cff-index data (+ start subrs-offset))
                  (vector))
              default
              nominal))
      (list (vector) 0 0)))

(defun parse-cff (data)
  "Return the CFF-FONT that a CFF file holds. Signals IMAGE-ERROR when it is damaged.

DATA: the font file."
  (declare (type octets data))
  (unless (and (>= (length data) 4) (= (aref data 0) 1))
    (font-damaged))
  (let ((position (aref data 2)))
    (multiple-value-bind (names after-names)
        (read-cff-index data position)
      (declare (ignore names))
      (multiple-value-bind (tops after-tops)
          (read-cff-index data after-names)
        (multiple-value-bind (strings after-strings)
            (read-cff-index data after-tops)
          (let* ((global-subrs (read-cff-index data after-strings))
                 (top (read-cff-dictionary data
                                           (car (svref tops 0))
                                           (cdr (svref tops 0))))
                 (charstrings-offset (first (dictionary-operands top 17)))
                 (font (make-cff-font :data
                                      data
                                      :strings
                                      strings
                                      :global-subrs
                                      global-subrs)))
            (unless charstrings-offset
              (font-damaged))
            (setf (cff-charstrings font) (read-cff-index data charstrings-offset))
            (let ((count (length (cff-charstrings font)))
                  (matrix (dictionary-operands top 1207)))
              (when (and matrix (= (length matrix) 6))
                (setf (cff-matrix font) (mapcar (lambda (value) (float value 1d0)) matrix)))
              (setf (cff-charset font)
                    (read-cff-charset data
                                      (or (first (dictionary-operands top 15)) 0)
                                      count))
              (cond ((dictionary-operands top 1230)
                     (setf (cff-cid-keyed font) t)
                     (let ((fonts (read-cff-index data (first (dictionary-operands top 1236)))))
                       (setf (cff-dictionaries font)
                             (map 'simple-vector
                                  (lambda (entry)
                                    (read-private-dictionary
                                     data
                                     (dictionary-operands (read-cff-dictionary data
                                                                               (car entry)
                                                                               (cdr entry)) 18)))
                                  fonts)))
                     (when (dictionary-operands top 1237)
                       (setf (cff-fd-select font)
                             (read-fd-select data
                                             (first (dictionary-operands top 1237))
                                             count))))
                    (t
                     (setf (cff-dictionaries font)
                           (vector (read-private-dictionary data (dictionary-operands top 18))))
                     (setf (cff-encoding font)
                           (read-cff-encoding data
                                              (or (first (dictionary-operands top 16)) 0)
                                              count))))
              font)))))))

(defun read-cff-encoding (data
                          position
                          count)
  "Read the built-in encoding of a CFF font as an alist from character codes to glyph numbers; a predefined encoding
(position 0 or 1) gives NIL, and the standard encoding is taken from the PDF encodings.

DATA: the font file.
POSITION: where the encoding is, or 0 or 1.
COUNT: how many glyphs the font has."
  (if (<= position 1)
      '()
      (let* ((format (aref data position))
             (cursor (1+ position))
             (entries '()))
        (case (logand format 127)
          (0 (let ((codes (aref data cursor)))
               (incf cursor)
               (dotimes (index codes)
                 (push (cons (aref data (+ cursor index)) (1+ index)) entries))
               (incf cursor codes)))
          (1 (let ((ranges (aref data cursor))
                   (glyph 1))
               (incf cursor)
               (dotimes (index ranges)
                 (let ((first (aref data cursor))
                       (left (aref data (1+ cursor))))
                   (incf cursor 2)
                   (dotimes (each (1+ left))
                     (when (< glyph count)
                       (push (cons (+ first each) glyph) entries)
                       (incf glyph)))))))
          (t (font-damaged)))
        (nreverse entries))))

(defun cff-glyph-names (font)
  "Return the table from the name of a glyph to its number, made on first use.

FONT: the CFF-FONT."
  (or (cff-names font)
      (setf (cff-names font)
            (let ((table (make-hash-table :test 'equal)))
              (unless (cff-cid-keyed font)
                (loop for glyph from (1- (length (cff-charset font))) downto 0
                      do (let ((name (cff-string font (svref (cff-charset font) glyph))))
                           (when name
                             (setf (gethash name table) glyph)))))
              table))))

(defun cff-glyph-named (font
                        name)
  "Return the number of the glyph that has a name, or NIL.

FONT: the CFF-FONT.
NAME: the name of the glyph."
  (values (gethash name (cff-glyph-names font))))

(defun cff-glyph-of-cid (font
                         cid)
  "Return the number of the glyph that has a CID in a CID-keyed font, or, in a font that is not, CID as the glyph.

FONT: the CFF-FONT.
CID: the CID."
  (if (cff-cid-keyed font)
      (position cid (cff-charset font))
      (and (< cid (length (cff-charstrings font))) cid)))

;;; Charstrings

(defun subroutine-bias (count)
  "Return the bias that is added to the number given to a call of a subroutine.

COUNT: how many subroutines there are."
  (cond ((< count 1240) 107)
        ((< count 33900) 1131)
        (t 32768)))

(defstruct (charstring-state (:conc-name cs-))
  "What a Type 2 charstring being run keeps: the stack, the point it is at, the commands drawn, and how many stems and
whether the width has been taken."
  (stack '())
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (commands '())
  (open nil)
  (stems 0 :type fixnum)
  (width-taken nil)
  (width nil)
  (operations 0 :type fixnum)
  (finished nil))

(defun cs-push (state
                value)
  "Put a number on the stack of a charstring.

STATE: the charstring state.
VALUE: the number."
  (when (>= (length (cs-stack state)) +cff-maximum-stack+)
    (font-damaged))
  (setf (cs-stack state) (append (cs-stack state) (list value))))

(defun cs-move (state
                dx
                dy)
  "Close the contour that is open, and begin another at a place relative to where the pen is.

STATE: the charstring state.
DX: how far right.
DY: how far up."
  (when (cs-open state)
    (push (list :close) (cs-commands state)))
  (incf (cs-x state) dx)
  (incf (cs-y state) dy)
  (push (list :move (cs-x state) (cs-y state)) (cs-commands state))
  (setf (cs-open state) t))

(defun cs-line (state
                dx
                dy)
  "Draw a line to a place relative to where the pen is.

STATE: the charstring state.
DX: how far right.
DY: how far up."
  (incf (cs-x state) dx)
  (incf (cs-y state) dy)
  (push (list :line (cs-x state) (cs-y state)) (cs-commands state)))

(defun cs-curve (state
                 dx1
                 dy1
                 dx2
                 dy2
                 dx3
                 dy3)
  "Draw a curve whose control points and end are placed relative to each other from where the pen is.

STATE: the charstring state.
DX1: x of the first control point, from the pen.
DY1: y of it.
DX2: x of the second control point, from the first.
DY2: y of it.
DX3: x of the end, from the second.
DY3: y of it."
  (let* ((x1 (+ (cs-x state) dx1))
         (y1 (+ (cs-y state) dy1))
         (x2 (+ x1 dx2))
         (y2 (+ y1 dy2))
         (x3 (+ x2 dx3))
         (y3 (+ y2 dy3)))
    (push (list :curve x1 y1 x2 y2 x3 y3) (cs-commands state))
    (setf (cs-x state) x3
          (cs-y state) y3)))

(defun cs-take-width (state
                      expected)
  "Take the width off the stack of a charstring when the first operator that clears it has one number more than it
needs.

STATE: the charstring state.
EXPECTED: how many numbers the operator needs, or NIL when any even number is right (stems)."
  (unless (cs-width-taken state)
    (setf (cs-width-taken state) t)
    (let ((count (length (cs-stack state))))
      (when (if expected
                (> count expected)
                (oddp count))
        (setf (cs-width state) (first (cs-stack state))
              (cs-stack state) (rest (cs-stack state)))))))

(defun run-charstring (font
                       state
                       from
                       length
                       local-subrs
                       depth)
  "Run a Type 2 charstring, or a subroutine, into a charstring state.

FONT: the CFF-FONT.
STATE: the charstring state.
FROM: the position of the code.
LENGTH: its length.
LOCAL-SUBRS: the local subroutines of the glyph.
DEPTH: how many subroutines enclose this code."
  (when (> depth +cff-maximum-subroutine-depth+)
    (font-damaged))
  (let ((data (cff-data font))
        (position from)
        (end (+ from length)))
    (loop while (and (< position end) (not (cs-finished state)))
          do (when (> (incf (cs-operations state)) +cff-maximum-operations+)
               (font-damaged))
             (let ((byte (aref data position)))
               (incf position)
               (cond ((= byte 28)
                      (cs-push state (octet-signed-word data position))
                      (incf position 2))
                     ((<= 32 byte 246)
                      (cs-push state (- byte 139)))
                     ((<= 247 byte 250)
                      (cs-push state (+ (* (- byte 247) 256) (aref data position) 108))
                      (incf position))
                     ((<= 251 byte 254)
                      (cs-push state (- (- (* (- byte 251) 256)) (aref data position) 108))
                      (incf position))
                     ((= byte 255)
                      (let ((value (octet-long data position)))
                        (cs-push state (/ (if (>= value #x80000000) (- value #x100000000) value) 65536d0)))
                      (incf position 4))
                     (t
                      (setf position (run-charstring-operator font
                                                              state
                                                              byte
                                                              data
                                                              position
                                                              local-subrs
                                                              depth))))))))

(defun alternating-curves (state
                           stack
                           horizontal)
  "Draw the curves of hvcurveto and vhcurveto, which alternate between starting level and starting upright.

STATE: the charstring state.
STACK: the numbers.
HORIZONTAL: true to start level."
  (loop while (>= (length stack) 4)
        do (let* ((last-extra (and (= (length stack) 5) (fifth stack)))
                  (a (first stack))
                  (b (second stack))
                  (c (third stack))
                  (d (fourth stack)))
             (setf stack (nthcdr 4 stack))
             (if horizontal
                 (cs-curve state
                           a
                           0
                           b
                           c
                           (or last-extra 0)
                           d)
                 (cs-curve state
                           0
                           a
                           b
                           c
                           d
                           (or last-extra 0)))
             (when last-extra
               (setf stack '()))
             (setf horizontal (not horizontal)))))

(defun run-charstring-operator (font
                                state
                                operator
                                data
                                position
                                local-subrs
                                depth)
  "Do one operator of a Type 2 charstring. Returns the position after it, which only the operators that skip bytes
change.

FONT: the CFF-FONT.
STATE: the charstring state.
OPERATOR: the number of the operator; for a two-byte operator, 1200 plus the second byte is looked up here.
DATA: the font file.
POSITION: the position just after the operator byte.
LOCAL-SUBRS: the local subroutines.
DEPTH: how many subroutines enclose this code."
  (let ((stack (cs-stack state)))
    (flet ((clear () (setf (cs-stack state) '())))
      (case operator
        ((1 3 18 23)
         (cs-take-width state nil)
         (incf (cs-stems state) (floor (length (cs-stack state)) 2))
         (clear))
        ((19 20)
         (cs-take-width state nil)
         (incf (cs-stems state) (floor (length (cs-stack state)) 2))
         (clear)
         (incf position (ceiling (cs-stems state) 8)))
        (21 (cs-take-width state 2)
         (let ((stack (cs-stack state)))
           (cs-move state
                    (or (first stack) 0)
                    (or (second stack) 0)))
         (clear))
        (22 (cs-take-width state 1)
         (cs-move state
                  (or (first (cs-stack state)) 0)
                  0)
         (clear))
        (4 (cs-take-width state 1)
         (cs-move state
                  0
                  (or (first (cs-stack state)) 0))
         (clear))
        (5 (loop for (dx dy) on stack by #'cddr
                 while dy
                 do (cs-line state
                             dx
                             dy))
         (clear))
        ((6 7)
         (let ((horizontal (= operator 6)))
           (dolist (delta stack)
             (if horizontal
                 (cs-line state
                          delta
                          0)
                 (cs-line state
                          0
                          delta))
             (setf horizontal (not horizontal))))
         (clear))
        (8 (loop while (>= (length stack) 6)
                 do (apply #'cs-curve state (subseq stack 0 6))
                    (setf stack (nthcdr 6 stack)))
         (clear))
        (24 (loop while (>= (length stack) 8)
                  do (apply #'cs-curve state (subseq stack 0 6))
                     (setf stack (nthcdr 6 stack)))
         (when (>= (length stack) 2)
           (cs-line state
                    (first stack)
                    (second stack)))
         (clear))
        (25 (loop while (>= (length stack) 8)
                  do (cs-line state
                              (first stack)
                              (second stack))
                     (setf stack (nthcdr 2 stack)))
         (when (>= (length stack) 6)
           (apply #'cs-curve state (subseq stack 0 6)))
         (clear))
        (26 (let ((extra (if (oddp (length stack)) (pop stack) 0)))
              (loop while (>= (length stack) 4)
                    do (cs-curve state
                                 extra
                                 (first stack)
                                 (second stack)
                                 (third stack)
                                 0
                                 (fourth stack))
                       (setf extra 0
                             stack (nthcdr 4 stack))))
         (clear))
        (27 (let ((extra (if (oddp (length stack)) (pop stack) 0)))
              (loop while (>= (length stack) 4)
                    do (cs-curve state
                                 (first stack)
                                 extra
                                 (second stack)
                                 (third stack)
                                 (fourth stack)
                                 0)
                       (setf extra 0
                             stack (nthcdr 4 stack))))
         (clear))
        (30 (alternating-curves state
                                stack
                                nil)
         (clear))
        (31 (alternating-curves state
                                stack
                                t)
         (clear))
        (10 (let* ((index (+ (car (last stack)) (subroutine-bias (length local-subrs))))
                   (entry (and (< -1 index (length local-subrs)) (svref local-subrs index))))
              (setf (cs-stack state) (butlast stack))
              (when entry
                (run-charstring font
                                state
                                (car entry)
                                (cdr entry)
                                local-subrs
                                (1+ depth)))))
        (29 (let* ((subrs (cff-global-subrs font))
                   (index (+ (car (last stack)) (subroutine-bias (length subrs))))
                   (entry (and (< -1 index (length subrs)) (svref subrs index))))
              (setf (cs-stack state) (butlast stack))
              (when entry
                (run-charstring font
                                state
                                (car entry)
                                (cdr entry)
                                local-subrs
                                (1+ depth)))))
        (11 nil)
        (14 (cs-take-width state (if (>= (length stack) 4) 4 0))
         (when (cs-open state)
           (push (list :close) (cs-commands state))
           (setf (cs-open state) nil))
         (setf (cs-finished state) t)
         (let ((stack (cs-stack state)))
           (when (>= (length stack) 4)
             (setf (cs-stack state) (last stack 4)))))
        (12 (setf position (run-escaped-operator state
                                                 (aref data position)
                                                 (1+ position)))
         (setf position position))
        (t (clear))))
    position))

(defun run-escaped-operator (state
                             code
                             position)
  "Do a two-byte operator of a Type 2 charstring: the flex curves and a few of the arithmetic operators.

STATE: the charstring state.
CODE: the second byte of the operator.
POSITION: the position after it."
  (let ((stack (cs-stack state)))
    (flet ((clear () (setf (cs-stack state) '())))
      (case code
        (34 (destructuring-bind (dx1 dx2 dy2 dx3 dx4 dx5 dx6)
                (append stack (make-list (max 0 (- 7 (length stack))) :initial-element 0))
              (cs-curve state
                        dx1
                        0
                        dx2
                        dy2
                        dx3
                        0)
              (cs-curve state
                        dx4
                        0
                        dx5
                        (- dy2)
                        dx6
                        0))
         (clear))
        (35 (when (>= (length stack) 12)
              (apply #'cs-curve state (subseq stack 0 6))
              (apply #'cs-curve state (subseq stack 6 12)))
         (clear))
        (36 (destructuring-bind (dx1 dy1 dx2 dy2 dx3 dx4 dx5 dy5 dx6)
                (append stack (make-list (max 0 (- 9 (length stack))) :initial-element 0))
              (cs-curve state
                        dx1
                        dy1
                        dx2
                        dy2
                        dx3
                        0)
              (cs-curve state
                        dx4
                        0
                        dx5
                        dy5
                        dx6
                        (- (+ dy1 dy2 dy5))))
         (clear))
        (37 (when (>= (length stack) 11)
              (let* ((dx (+ (nth 0 stack) (nth 2 stack) (nth 4 stack) (nth 6 stack) (nth 8 stack)))
                     (dy (+ (nth 1 stack) (nth 3 stack) (nth 5 stack) (nth 7 stack) (nth 9 stack)))
                     (last (nth 10 stack)))
                (apply #'cs-curve state (subseq stack 0 6))
                (if (> (abs dx) (abs dy))
                    (cs-curve state
                              (nth 6 stack)
                              (nth 7 stack)
                              (nth 8 stack)
                              (nth 9 stack)
                              last
                              (- dy))
                    (cs-curve state
                              (nth 6 stack)
                              (nth 7 stack)
                              (nth 8 stack)
                              (nth 9 stack)
                              (- dx)
                              last))))
         (clear))
        (t
         (clear)))))
  position)

(defun cff-glyph-dictionary (font
                             glyph)
  "Return the private data, as a list of local subroutines, default width and nominal width, that a glyph uses.

FONT: the CFF-FONT.
GLYPH: the glyph number."
  (let ((selected (and (cff-fd-select font) (aref (cff-fd-select font) glyph))))
    (svref (cff-dictionaries font)
           (min (or selected 0) (1- (length (cff-dictionaries font)))))))

(defun cff-outline (font
                    glyph)
  "Return the commands that draw a glyph of a CFF font, in units of the font, kept for the next time.

FONT: the CFF-FONT.
GLYPH: the glyph number."
  (multiple-value-bind (commands found)
      (gethash glyph (cff-cache font))
    (if found
        commands
        (setf (gethash glyph (cff-cache font))
              (if (not (< -1 glyph (length (cff-charstrings font))))
                  '()
                  (handler-case
                      (let ((state (make-charstring-state))
                            (entry (svref (cff-charstrings font) glyph))
                            (dictionary (cff-glyph-dictionary font glyph)))
                        (run-charstring font
                                        state
                                        (car entry)
                                        (cdr entry)
                                        (first dictionary)
                                        0)
                        (when (cs-open state)
                          (push (list :close) (cs-commands state)))
                        (nreverse (cs-commands state)))
                    (base:image-error ()
                      '())))))))
