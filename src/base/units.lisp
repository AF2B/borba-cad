(in-package #:borba-cad/base)

;;;; Units. World lengths are stored in millimetres; this file is the only place where unit
;;;; factors and unit names appear. Conversions happen at the user-interface boundary.

(deftype length-unit ()
  "A unit in which lengths may be displayed and typed."
  '(member :millimeter :centimeter :meter))

(defconstant +degrees-per-half-turn+ 180d0
  "Degrees in half a turn (pi radians).")

(defconstant +maximum-input-digits+ 32
  "Longest digit sequence accepted when parsing a typed number; bounds the size of the
rational intermediate value.")

(defparameter *length-units*
  '((:millimeter "mm" 1d0 0)
    (:centimeter "cm" 10d0 1)
    (:meter "m" 1000d0 2))
  "Table of supported length units: keyword, abbreviation, millimetres per unit, and the
default number of decimals shown when formatting lengths in that unit.")

(defparameter *superscript-two* (string (code-char 178))
  "The superscript-two character used in area unit names.")

(defun supported-length-units ()
  "Return the list of supported length unit keywords."
  (mapcar #'first *length-units*))

(defun length-unit-p (object)
  "Return true when OBJECT is a supported length unit keyword.

OBJECT: any object."
  (and (assoc object *length-units*) t))

(defun unit-entry (unit)
  "Return the table entry of UNIT, signaling a type error for unsupported units.

UNIT: a length unit keyword."
  (check-type unit length-unit)
  (assoc unit *length-units*))

(defun unit-abbreviation (unit)
  "Return the abbreviation of UNIT, for example \"mm\".

UNIT: a length unit keyword."
  (second (unit-entry unit)))

(defun unit-area-abbreviation (unit)
  "Return the area abbreviation of UNIT, for example \"m2\" with a superscript two.

UNIT: a length unit keyword."
  (concatenate 'string (unit-abbreviation unit) *superscript-two*))

(defun unit-from-abbreviation (text)
  "Return the unit keyword whose abbreviation equals TEXT (case-insensitive), or NIL.

TEXT: an abbreviation such as \"cm\"."
  (loop for (unit abbreviation) in *length-units*
        when (string-equal text abbreviation)
          return unit))

(defun millimeters-per-unit (unit)
  "Return the number of millimetres in one UNIT.

UNIT: a length unit keyword."
  (third (unit-entry unit)))

(defun millimeters->unit (millimeters
                          unit)
  "Convert a length from millimetres to UNIT.

MILLIMETERS: length in millimetres.
UNIT: target length unit keyword."
  (/ millimeters (millimeters-per-unit unit)))

(defun unit->millimeters (value
                          unit)
  "Convert a length from UNIT to millimetres.

VALUE: length expressed in UNIT.
UNIT: source length unit keyword."
  (* value (millimeters-per-unit unit)))

(defun square-millimeters->unit (square-millimeters
                                 unit)
  "Convert an area from square millimetres to square UNIT.

SQUARE-MILLIMETERS: area in square millimetres.
UNIT: length unit keyword whose square is the target area unit."
  (/ square-millimeters (expt (millimeters-per-unit unit) 2)))

(defun default-display-precision (unit)
  "Return the default number of decimals used when showing lengths in UNIT.

UNIT: a length unit keyword."
  (fourth (unit-entry unit)))

(defun format-decimal (value
                       precision)
  "Format VALUE with exactly PRECISION decimals, never in exponent notation.
Rounds half away from zero on the exact binary value and never prints a negative zero.

VALUE: a real number.
PRECISION: number of digits after the decimal point, 0 or more."
  (check-type precision (integer 0))
  (let* ((scale (expt 10 precision))
         (scaled (floor (+ (* (abs (rational value)) scale) 1/2)))
         (whole (floor scaled scale))
         (fraction (mod scaled scale))
         (text (if (zerop precision)
                   (format nil "~d" whole)
                   (format nil "~d.~v,'0d" whole precision fraction))))
    (if (and (minusp value) (plusp scaled))
        (concatenate 'string "-" text)
        text)))

(defun format-length (millimeters
                      unit
                      &key (precision (default-display-precision unit))
                           with-unit)
  "Format a length for display.

MILLIMETERS: length in millimetres.
UNIT: display unit keyword.
PRECISION: decimals to show; defaults to the unit's default precision.
WITH-UNIT: when true, append the unit abbreviation."
  (let ((number (format-decimal (millimeters->unit millimeters unit) precision)))
    (if with-unit
        (format nil "~a ~a" number (unit-abbreviation unit))
        number)))

(defun format-area (square-millimeters
                    unit
                    &key (precision (default-display-precision unit))
                         with-unit)
  "Format an area for display.

SQUARE-MILLIMETERS: area in square millimetres.
UNIT: display length unit keyword (the area is shown in its square).
PRECISION: decimals to show; defaults to the unit's default precision.
WITH-UNIT: when true, append the area unit abbreviation."
  (let ((number (format-decimal (square-millimeters->unit square-millimeters unit) precision)))
    (if with-unit
        (format nil "~a ~a" number (unit-area-abbreviation unit))
        number)))

(defconstant +thousands-group-size+ 3
  "Digits in each group after the first when thousands are separated, as in 1.250.000.")

(defun digit-string-p (text)
  "Return true when TEXT is one or more digits.

TEXT: a string."
  (and (plusp (length text))
       (every #'digit-char-p text)))

(defun without-thousands-separators (text
                                     mark)
  "Return TEXT, an integer written with MARK between its thousands, as plain digits, or NIL when
it is not so written: the first group has one to three digits and every other has exactly three.

TEXT: the integer part of a number.
MARK: the thousands separator, #\\. or #\\,."
  (let ((groups (uiop:split-string text :separator (list mark))))
    (and (every #'digit-string-p groups)
         (<= 1 (length (first groups)) +thousands-group-size+)
         (every (lambda (group) (= (length group) +thousands-group-size+))
                (rest groups))
         (apply #'concatenate 'string groups))))

(defun normalize-separators (text)
  "Return TEXT, a number as typed, in a form with at most one separator, a decimal mark: when it
has more, they are thousands separators as Brazilian and American writing have them (1.250,5 and
1,250.5 are both 1250.5; 1.250.000 is a million and a quarter). The decimal mark is the last
separator when two kinds appear. Returns NIL when the separators make no sense (1.2.3, 1,5,5).

TEXT: the number text without surrounding whitespace or unit."
  (let* ((sign-length (if (and (plusp (length text)) (find (char text 0) "+-")) 1 0))
         (sign (subseq text 0 sign-length))
         (body (subseq text sign-length))
         (last-dot (position #\. body :from-end t))
         (last-comma (position #\, body :from-end t)))
    (if (<= (+ (count #\. body) (count #\, body)) 1)
        text
        (let* ((decimal-mark (and last-dot
                                  last-comma
                                  (if (> last-dot last-comma) #\. #\,)))
               (thousands-mark (cond (decimal-mark (if (char= decimal-mark #\.) #\, #\.))
                                     (last-dot #\.)
                                     (t #\,)))
               (decimal-position (and decimal-mark (position decimal-mark body :from-end t)))
               (digits (without-thousands-separators (if decimal-position
                                                         (subseq body 0 decimal-position)
                                                         body)
                                                     thousands-mark))
               (fraction (and decimal-position (subseq body (1+ decimal-position)))))
          (and digits
               (or (null decimal-mark) (= 1 (count decimal-mark body)))
               (or (null fraction) (zerop (length fraction)) (digit-string-p fraction))
               (concatenate 'string sign digits (if fraction (format nil ".~a" fraction) "")))))))

(defun parse-decimal-rational (text)
  "Parse TEXT as a decimal number and return it as an exact rational, or NIL when malformed.
Accepts an optional sign, digits, an optional fraction introduced by . or , and thousands
separators (see NORMALIZE-SEPARATORS).

TEXT: the number text without surrounding whitespace or unit."
  (let* ((text (or (normalize-separators text)
                   (return-from parse-decimal-rational nil)))
         (length (length text))
        (position 0)
        (sign 1)
        (digits 0)
        (integer-part 0)
        (fraction-part 0)
        (fraction-digits 0)
        (seen-separator nil))
    (when (and (< position length) (find (char text position) "+-"))
      (when (char= (char text position) #\-)
        (setf sign -1))
      (incf position))
    (loop while (< position length)
          do (let ((character (char text position)))
               (cond ((digit-char-p character)
                      (incf digits)
                      (when (> digits +maximum-input-digits+)
                        (return-from parse-decimal-rational nil))
                      (if seen-separator
                          (setf fraction-part (+ (* fraction-part 10) (digit-char-p character))
                                fraction-digits (1+ fraction-digits))
                          (setf integer-part (+ (* integer-part 10) (digit-char-p character)))))
                     ((and (not seen-separator) (find character ".,"))
                      (setf seen-separator t))
                     (t (return-from parse-decimal-rational nil))))
             (incf position))
    (and (plusp digits)
         (* sign (+ integer-part (/ fraction-part (expt 10 fraction-digits)))))))

(defun parse-decimal (text)
  "Parse TEXT as a plain decimal number and return a double-float.
Signals INVALID-INPUT when TEXT is not a number.

TEXT: user-typed text such as \"12.5\" or \"-3,25\"."
  (let ((value (parse-decimal-rational (string-trim " " text))))
    (unless value
      (error 'invalid-input
             :text text
             :reason (i18n:tr "not a number; enter digits with an optional sign and decimal point")))
    (float value 1d0)))

(defun parse-length (text
                     default-unit)
  "Parse a typed length, with an optional unit suffix, into millimetres (double-float).
Signals INVALID-INPUT when TEXT is malformed or names an unknown unit.

TEXT: user-typed text such as \"2.5\", \"150 mm\" or \"1,2m\".
DEFAULT-UNIT: unit assumed when TEXT has no suffix."
  (let* ((trimmed (string-trim " " text))
         (suffix-start (or (position-if-not #'alpha-char-p trimmed :from-end t) -1))
         (suffix (subseq trimmed (1+ suffix-start)))
         (number-text (string-trim " " (subseq trimmed 0 (1+ suffix-start))))
         (value (handler-case (parse-decimal number-text)
                  (invalid-input (condition)
                    ;; Say what was typed, not the part left after the unit was cut off.
                    (error 'invalid-input
                           :text text
                           :reason (invalid-input-reason condition)))))
         (unit (if (zerop (length suffix))
                   default-unit
                   (unit-from-abbreviation suffix))))
    (unless unit
      (error 'invalid-input
             :text text
             :reason (i18n:trf "unknown unit ~s; use mm, cm or m" suffix)))
    (unit->millimeters value unit)))

(declaim (inline radians->degrees degrees->radians))
(defun radians->degrees (radians)
  "Convert an angle from radians to degrees.

RADIANS: angle in radians."
  (* radians (/ +degrees-per-half-turn+ (float pi 1d0))))

(defun degrees->radians (degrees)
  "Convert an angle from degrees to radians.

DEGREES: angle in degrees."
  (* degrees (/ (float pi 1d0) +degrees-per-half-turn+)))

(defconstant +bytes-per-kilobyte+ 1024
  "Bytes in one KB as file sizes are shown.")

(defun format-byte-size (bytes)
  "Format a file size for people: bytes below one KB, then KB, MB and GB with one decimal.

BYTES: a non-negative integer."
  (let ((units '("KB" "MB" "GB"))
        (value (float bytes 1d0)))
    (if (< bytes +bytes-per-kilobyte+)
        (format nil "~d B" bytes)
        (progn
          (loop do (setf value (/ value +bytes-per-kilobyte+))
                while (and (>= value +bytes-per-kilobyte+) (rest units))
                do (pop units))
          (format nil "~a ~a" (format-decimal value 1) (first units))))))
