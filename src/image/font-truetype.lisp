(in-package #:borba-cad/image)

;;;; The outlines of a TrueType font (the glyf table of an sfnt file), which is what the text of most PDF files from
;;;; plotters and office programs is drawn from: the tables the glyphs need, the glyphs themselves, simple and
;;;; composite, as lines and curves in the units of the font, and the character maps that say which glyph a code or a
;;;; Unicode value stands for. A glyph is a list of commands, (:move x y), (:line x y), (:curve x1 y1 x2 y2 x3 y3)
;;;; and (:close), the same for every kind of font.

(defconstant +truetype-maximum-glyph-depth+ 6
  "How deep composite glyphs may nest.")

(defconstant +truetype-maximum-points+ 20000
  "The most points the contours of one glyph may have.")

(defconstant +composite-words+ #x0001
  "The flag of a component of a composite glyph that says its two arguments are 16 bits wide.")

(defconstant +composite-offsets+ #x0002
  "The flag that says the arguments of a component are an offset.")

(defconstant +composite-scale+ #x0008
  "The flag that says a component has one scale.")

(defconstant +composite-more+ #x0020
  "The flag that says another component follows.")

(defconstant +composite-two-scales+ #x0040
  "The flag that says a component has a scale for x and one for y.")

(defconstant +composite-matrix+ #x0080
  "The flag that says a component has a two by two matrix.")

(defstruct (truetype-font (:conc-name truetype-))
  "A TrueType font read: its bytes, where its tables are, and what the glyphs need to be drawn and spaced.

DATA: the font file.
TABLES: an alist from the tag of a table, as a string, to its position and length.
UNITS-PER-EM: how many units the em square has.
GLYPH-COUNT: how many glyphs the font has.
OFFSETS: the position in the glyf table of each glyph, with one more at the end.
ADVANCES: the advance width of each glyph that has one of its own, in units.
CMAPS: the character maps, as a list of (platform encoding lookup-function).
CACHE: the outlines made so far, by glyph number."
  (data (make-octets 0) :type octets)
  (tables '())
  (units-per-em 1000 :type fixnum)
  (glyph-count 0 :type fixnum)
  (offsets (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (advances (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (cmaps '())
  (cache (make-hash-table)))

(defun font-damaged ()
  "Signal IMAGE-ERROR for a font that does not follow its format."
  (image-failure "A font of this PDF is damaged."))

(defun octet-signed-word (octets
                          position)
  "Return the big-endian signed 16-bit number at POSITION.

OCTETS: the vector.
POSITION: where the number starts."
  (let ((value (octet-word octets position)))
    (if (>= value #x8000) (- value #x10000) value)))

(defun signed-byte-of (byte)
  "Return a byte as a number from -128 to 127.

BYTE: the byte."
  (if (>= byte 128) (- byte 256) byte))

(defun table-of (font
                 tag)
  "Return the position and the length of a table of a font, as two values, or NIL when it has none.

FONT: the TRUETYPE-FONT.
TAG: the four letters of the table."
  (let ((entry (cdr (assoc tag (truetype-tables font) :test #'string=))))
    (when entry
      (values (first entry) (second entry)))))

(defun read-table-directory (data)
  "Return the tables of an sfnt file as an alist from the tag to (position length).

DATA: the font file."
  (let ((count (octet-word data 4))
        (tables '()))
    (when (> count 256)
      (font-damaged))
    (dotimes (index count)
      (let* ((at (+ 12 (* 16 index)))
             (tag (map 'string #'code-char (subseq data at (+ at 4))))
             (position (octet-long data (+ at 8)))
             (length (octet-long data (+ at 12))))
        (when (<= (+ position length) (length data))
          (push (list tag position length) tables))))
    (mapcar (lambda (entry) (cons (first entry) (rest entry))) (nreverse tables))))

(defun read-horizontal-metrics (font)
  "Read the advance widths of the glyphs of a font into it.

FONT: the TRUETYPE-FONT."
  (multiple-value-bind (hhea)
      (table-of font "hhea")
    (multiple-value-bind (hmtx hmtx-length)
        (table-of font "hmtx")
      (when (and hhea hmtx)
        (let* ((data (truetype-data font))
               (count (min (octet-word data (+ hhea 34)) (truetype-glyph-count font) (floor hmtx-length 4)))
               (advances (make-array (max 1 count) :element-type 'fixnum :initial-element 0)))
          (dotimes (index count)
            (setf (aref advances index) (octet-word data (+ hmtx (* 4 index)))))
          (setf (truetype-advances font) advances))))))

(defun read-glyph-offsets (font)
  "Read where each glyph is in the glyf table into a font.

FONT: the TRUETYPE-FONT."
  (multiple-value-bind (head)
      (table-of font "head")
    (multiple-value-bind (loca loca-length)
        (table-of font "loca")
      (unless (and head loca)
        (font-damaged))
      (let* ((data (truetype-data font))
             (long (plusp (octet-signed-word data (+ head 50))))
             (entry-size (if long 4 2))
             (count (min (1+ (truetype-glyph-count font)) (floor loca-length entry-size)))
             (offsets (make-array count :element-type 'fixnum)))
        (dotimes (index count)
          (setf (aref offsets index)
                (if long
                    (octet-long data (+ loca (* 4 index)))
                    (* 2 (octet-word data (+ loca (* 2 index)))))))
        (setf (truetype-offsets font) offsets)))))

;;; Character maps

(defun format-4-lookup (data
                        start)
  "Return the function of a character code that a cmap subtable of format 4 stands for.

DATA: the font file.
START: the position of the subtable."
  (let* ((segments (floor (octet-word data (+ start 6)) 2))
         (ends (+ start 14))
         (starts (+ ends (* 2 segments) 2))
         (deltas (+ starts (* 2 segments)))
         (ranges (+ deltas (* 2 segments))))
    (lambda (code)
      (dotimes (index segments 0)
        (when (>= (octet-word data (+ ends (* 2 index))) code)
          (return
            (let ((first (octet-word data (+ starts (* 2 index)))))
              (if (> first code)
                  0
                  (let ((offset (octet-word data (+ ranges (* 2 index))))
                        (delta (octet-word data (+ deltas (* 2 index)))))
                    (if (zerop offset)
                        (logand #xFFFF (+ code delta))
                        (let* ((at (+ ranges (* 2 index) offset (* 2 (- code first))))
                               (glyph (if (<= (+ at 2) (length data)) (octet-word data at) 0)))
                          (if (zerop glyph) 0 (logand #xFFFF (+ glyph delta))))))))))))))

(defun cmap-lookup (data
                    start)
  "Return the function of a character code that a cmap subtable stands for, or NIL for a format that is not read.

DATA: the font file.
START: the position of the subtable."
  (case (octet-word data start)
    (0 (lambda (code)
         (if (< code 256) (aref data (+ start 6 code)) 0)))
    (4 (format-4-lookup data start))
    (6 (let ((first (octet-word data (+ start 6)))
             (count (octet-word data (+ start 8))))
         (lambda (code)
           (if (< -1 (- code first) count)
               (octet-word data (+ start 10 (* 2 (- code first))))
               0))))
    (12 (let ((groups (octet-long data (+ start 12))))
          (lambda (code)
            (dotimes (index (min groups 100000) 0)
              (let* ((at (+ start 16 (* 12 index)))
                     (first (octet-long data at))
                     (last (octet-long data (+ at 4))))
                (when (<= first code last)
                  (return (+ (octet-long data (+ at 8)) (- code first)))))))))
    (t nil)))

(defun read-cmaps (font)
  "Read the character maps of a font that this program can use into it.

FONT: the TRUETYPE-FONT."
  (multiple-value-bind (cmap cmap-length)
      (table-of font "cmap")
    (when cmap
      (let* ((data (truetype-data font))
             (count (min (octet-word data (+ cmap 2)) 64))
             (maps '()))
        (dotimes (index count)
          (let* ((at (+ cmap 4 (* 8 index)))
                 (offset (octet-long data (+ at 4))))
            (when (< (+ offset 6) cmap-length)
              (let ((lookup (cmap-lookup data (+ cmap offset))))
                (when lookup
                  (push (list (octet-word data at) (octet-word data (+ at 2)) lookup) maps))))))
        (setf (truetype-cmaps font) (nreverse maps))))))

(defun parse-truetype (data)
  "Return the TRUETYPE-FONT that a TrueType file holds. Signals IMAGE-ERROR when it is damaged.

DATA: the font file."
  (declare (type octets data))
  (unless (>= (length data) 12)
    (font-damaged))
  (let ((font (make-truetype-font :data data
                                  :tables (read-table-directory data))))
    (multiple-value-bind (head)
        (table-of font "head")
      (multiple-value-bind (maxp)
          (table-of font "maxp")
        (unless (and head maxp)
          (font-damaged))
        (setf (truetype-units-per-em font) (max 16 (octet-word data (+ head 18)))
              (truetype-glyph-count font) (octet-word data (+ maxp 4)))))
    (read-glyph-offsets font)
    (read-horizontal-metrics font)
    (read-cmaps font)
    font))

(defun truetype-advance (font
                         glyph)
  "Return the advance width of a glyph in units of the font.

FONT: the TRUETYPE-FONT.
GLYPH: the glyph number."
  (let ((advances (truetype-advances font)))
    (if (zerop (length advances))
        0
        (aref advances (min glyph (1- (length advances)))))))

(defun truetype-glyph-for (font
                           platform
                           encoding
                           code)
  "Return the glyph that a character code has in the character map of a platform and an encoding, or NIL when the
font has no such map or the map has nothing for the code.

FONT: the TRUETYPE-FONT.
PLATFORM: the platform of the map.
ENCODING: its encoding.
CODE: the character code."
  (let ((map (find-if (lambda (entry) (and (= (first entry) platform) (= (second entry) encoding)))
                      (truetype-cmaps font))))
    (when map
      (let ((glyph (funcall (third map) code)))
        (and (plusp glyph) glyph)))))

;;; Outlines

(defun glyph-points (data
                     start
                     contours)
  "Read the points of a simple glyph as a list of contours, each a list of (x y on-curve), in units of the font.

DATA: the font file.
START: the position of the glyph.
CONTOURS: how many contours it has."
  (let* ((ends (loop for index below contours
                     collect (octet-word data (+ start 10 (* 2 index)))))
         (count (if ends (1+ (car (last ends))) 0))
         (position (+ start 10 (* 2 contours)))
         (instructions (octet-word data position)))
    (when (> count +truetype-maximum-points+)
      (font-damaged))
    (incf position (+ 2 instructions))
    (let ((flags (make-array count :element-type '(unsigned-byte 8)))
          (xs (make-array count :element-type 'fixnum))
          (ys (make-array count :element-type 'fixnum))
          (index 0))
      (loop while (< index count)
            do (let ((flag (aref data position)))
                 (incf position)
                 (setf (aref flags index) flag)
                 (incf index)
                 (when (logtest flag 8)
                   (let ((repeat (aref data position)))
                     (incf position)
                     (dotimes (each repeat)
                       (when (< index count)
                         (setf (aref flags index) flag)
                         (incf index)))))))
      (let ((value 0))
        (dotimes (index count)
          (let ((flag (aref flags index)))
            (cond ((logtest flag 2)
                   (let ((delta (aref data position)))
                     (incf position)
                     (incf value (if (logtest flag 16) delta (- delta)))))
                  ((not (logtest flag 16))
                   (incf value (octet-signed-word data position))
                   (incf position 2)))
            (setf (aref xs index) value))))
      (let ((value 0))
        (dotimes (index count)
          (let ((flag (aref flags index)))
            (cond ((logtest flag 4)
                   (let ((delta (aref data position)))
                     (incf position)
                     (incf value (if (logtest flag 32) delta (- delta)))))
                  ((not (logtest flag 32))
                   (incf value (octet-signed-word data position))
                   (incf position 2)))
            (setf (aref ys index) value))))
      (let ((first 0)
            (result '()))
        (dolist (end ends (nreverse result))
          (push (loop for index from first to end
                      collect (list (aref xs index) (aref ys index) (logtest (aref flags index) 1)))
                result)
          (setf first (1+ end)))))))

(defun expanded-contour (points)
  "Return the points of a contour with an on-curve point between every two off-curve points that follow each other,
which is where the curve of a TrueType glyph passes between them, as a vector of (x y on-curve).

POINTS: a list of (x y on-curve)."
  (let* ((vector (coerce points 'simple-vector))
         (count (length vector))
         (result '()))
    (dotimes (index count)
      (let ((point (svref vector index))
            (next (svref vector (mod (1+ index) count))))
        (push point result)
        (unless (or (third point) (third next))
          (push (list (/ (+ (first point) (first next)) 2) (/ (+ (second point) (second next)) 2) t) result))))
    (coerce (nreverse result) 'simple-vector)))

(defun contour-commands (points
                         transform)
  "Return the commands that draw a closed contour of on-curve and off-curve points, as a list.

POINTS: a list of (x y on-curve) of one contour.
TRANSFORM: a function of x and y that returns the point they are carried to, as two values."
  (when (< (length points) 2)
    (return-from contour-commands '()))
  (let* ((vector (expanded-contour points))
         (count (length vector))
         (start (position-if #'third vector))
         (commands '()))
    (flet ((carried (x
                     y)
             (multiple-value-list (funcall transform x y)))
           (point (index)
             (svref vector (mod index count))))
      (let* ((first (point start))
             (here-x (first first))
             (here-y (second first))
             (k 1))
        (push (cons :move (carried here-x here-y)) commands)
        (loop while (<= k count)
              do (let ((point (point (+ start k))))
                   (if (third point)
                       (progn (push (cons :line (carried (first point) (second point))) commands)
                              (setf here-x (first point)
                                    here-y (second point))
                              (incf k))
                       (let ((end (point (+ start k 1))))
                         (push (cons :curve
                                     (append (carried (+ here-x (* 2/3 (- (first point) here-x)))
                                                      (+ here-y (* 2/3 (- (second point) here-y))))
                                             (carried (+ (first end) (* 2/3 (- (first point) (first end))))
                                                      (+ (second end) (* 2/3 (- (second point) (second end)))))
                                             (carried (first end) (second end))))
                               commands)
                         (setf here-x (first end)
                               here-y (second end))
                         (incf k 2)))))
        (push (list :close) commands)
        (nreverse commands)))))

(defun glyph-commands (font
                       glyph
                       transform
                       depth)
  "Return the commands that draw a glyph of a TrueType font, in units of the font carried by a transform.

FONT: the TRUETYPE-FONT.
GLYPH: the glyph number.
TRANSFORM: a function of x and y that returns the point they are carried to, as two values.
DEPTH: how many composite glyphs enclose this one."
  (let ((offsets (truetype-offsets font)))
    (when (or (> depth +truetype-maximum-glyph-depth+)
              (not (< -1 glyph (1- (length offsets)))))
      (return-from glyph-commands '()))
    (multiple-value-bind (glyf glyf-length)
        (table-of font "glyf")
      (let ((from (aref offsets glyph))
            (to (aref offsets (1+ glyph))))
        (if (or (null glyf) (<= to from) (> to glyf-length))
            '()
            (let* ((data (truetype-data font))
                   (start (+ glyf from))
                   (contours (octet-signed-word data start)))
              (if (>= contours 0)
                  (loop for points in (glyph-points data
                                                    start
                                                    contours)
                        append (contour-commands points transform))
                  (let ((position (+ start 10))
                        (commands '()))
                    (loop
                      (let* ((flags (octet-word data position))
                             (component (octet-word data (+ position 2)))
                             (words (logtest flags +composite-words+))
                             (dx 0)
                             (dy 0)
                             (a 1)
                             (b 0)
                             (c 0)
                             (d 1))
                        (incf position 4)
                        (if words
                            (progn (when (logtest flags +composite-offsets+)
                                     (setf dx (octet-signed-word data position)
                                           dy (octet-signed-word data (+ position 2))))
                                   (incf position 4))
                            (progn (when (logtest flags +composite-offsets+)
                                     (setf dx (signed-byte-of (aref data position))
                                           dy (signed-byte-of (aref data (1+ position)))))
                                   (incf position 2)))
                        (flet ((fixed (at)
                                 (/ (octet-signed-word data at) 16384d0)))
                          (cond ((logtest flags +composite-scale+)
                                 (setf a (fixed position)
                                       d a)
                                 (incf position 2))
                                ((logtest flags +composite-two-scales+)
                                 (setf a (fixed position)
                                       d (fixed (+ position 2)))
                                 (incf position 4))
                                ((logtest flags +composite-matrix+)
                                 (setf a (fixed position)
                                       b (fixed (+ position 2))
                                       c (fixed (+ position 4))
                                       d (fixed (+ position 6)))
                                 (incf position 8))))
                        (setf commands
                              (append commands
                                      (glyph-commands font
                                                      component
                                                      (lambda (x y)
                                                        (funcall transform
                                                                 (+ (* a x) (* c y) dx)
                                                                 (+ (* b x) (* d y) dy)))
                                                      (1+ depth))))
                        (unless (logtest flags +composite-more+)
                          (return))))
                    commands))))))))

(defun truetype-outline (font
                         glyph)
  "Return the commands that draw a glyph, in units of the font, kept for the next time.

FONT: the TRUETYPE-FONT.
GLYPH: the glyph number."
  (multiple-value-bind (commands found)
      (gethash glyph (truetype-cache font))
    (if found
        commands
        (setf (gethash glyph (truetype-cache font))
              (handler-case (glyph-commands font
                                            glyph
                                            (lambda (x y) (values x y))
                                            0)
                (base:image-error ()
                  '()))))))
