(in-package #:borba-cad/image)

;;;; The syntax of PDF (ISO 32000-1, section 7.3): numbers, names, strings, arrays, dictionaries, indirect references
;;;; and, in a page's content, operators. A name is a Lisp string, a string of the file is a vector of bytes, a number
;;;; is an integer or a double-float, an array is a vector, a dictionary is an EQUAL hash table keyed by the names,
;;;; a reference is (:REF number generation) and an operator is (:OP . "name"). What is read comes from a file the
;;;; program did not write, so every read is bounded and the depth of nesting is limited.

(defconstant +pdf-maximum-depth+ 48
  "How deep arrays and dictionaries may nest.")

(defconstant +pdf-maximum-integer-digits+ 18
  "How many digits an integer of a PDF may have before the program calls it damaged.")

(defstruct (pdf-reader (:conc-name reader-))
  "A place in a vector of bytes that PDF syntax is being read from."
  (data (make-octets 0) :type octets)
  (position 0 :type fixnum)
  (end 0 :type fixnum))

(defun pdf-damaged ()
  "Signal IMAGE-ERROR for a PDF file that does not follow the format."
  (image-failure "This PDF file is damaged."))

(declaim (inline pdf-white-p pdf-delimiter-p pdf-regular-p digit-byte-p))

(defun pdf-white-p (byte)
  "Return true for the bytes the PDF format calls white space.

BYTE: the byte."
  (declare (type (unsigned-byte 8) byte))
  (or (= byte 32) (= byte 10) (= byte 13) (= byte 9) (= byte 12) (= byte 0)))

(defun pdf-delimiter-p (byte)
  "Return true for the bytes that end a name or a number without white space: ( ) < > [ ] { } / %.

BYTE: the byte."
  (declare (type (unsigned-byte 8) byte))
  (member byte '(40 41 60 62 91 93 123 125 47 37)))

(defun pdf-regular-p (byte)
  "Return true for a byte that is neither white space nor a delimiter.

BYTE: the byte."
  (declare (type (unsigned-byte 8) byte))
  (not (or (pdf-white-p byte) (pdf-delimiter-p byte))))

(defun digit-byte-p (byte)
  "Return true for the bytes of the ten digits.

BYTE: the byte."
  (declare (type (unsigned-byte 8) byte))
  (<= 48 byte 57))

(defun make-reader-on (data
                       &key (start 0)
                            (end (length data)))
  "Return a reader of the bytes from START to END of DATA.

DATA: the bytes.
START: where reading begins.
END: the position after the last byte that may be read."
  (make-pdf-reader :data
                   data
                   :position
                   start
                   :end
                   end))

(defun reader-byte (reader
                    &optional (offset 0))
  "Return the byte OFFSET places after the position of the reader, or NIL when that is past the end.

READER: the reader.
OFFSET: how many bytes ahead."
  (declare (type pdf-reader reader)
           (type fixnum offset))
  (let ((at (+ (reader-position reader) offset)))
    (and (< at (reader-end reader))
         (aref (reader-data reader) at))))

(defun skip-pdf-white (reader)
  "Move the reader past white space and comments.

READER: the reader."
  (declare (type pdf-reader reader)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((data (reader-data reader))
        (position (reader-position reader))
        (end (reader-end reader)))
    (declare (type fixnum position end))
    (loop while (< position end)
          do (let ((byte (aref data position)))
               (cond ((pdf-white-p byte)
                      (incf position))
                     ((= byte 37)
                      (loop while (and (< position end)
                                       (/= (aref data position) 10)
                                       (/= (aref data position) 13))
                            do (incf position)))
                     (t
                      (return)))))
    (setf (reader-position reader) position)))

(defun read-pdf-integer-token (reader)
  "Read a run of digits at the reader and return it as an integer, or NIL when there is none.

READER: the reader."
  (let ((start (reader-position reader))
        (value 0))
    (loop while (and (reader-byte reader) (digit-byte-p (reader-byte reader)))
          do (setf value (+ (* value 10) (- (reader-byte reader) 48)))
             (incf (reader-position reader))
             (when (> (- (reader-position reader) start) +pdf-maximum-integer-digits+)
               (pdf-damaged)))
    (and (> (reader-position reader) start) value)))

(defun read-pdf-number (reader)
  "Read a number at the reader: an optional sign, digits, and an optional point with digits. Anything else the number
runs into is left for the next token.

READER: the reader."
  (let ((sign 1)
        (whole 0)
        (fraction 0)
        (scale 1)
        (digits 0)
        (real nil))
    (loop while (member (reader-byte reader) '(43 45))
          do (when (= (reader-byte reader) 45)
               (setf sign (- sign)))
             (incf (reader-position reader)))
    (loop while (and (reader-byte reader) (digit-byte-p (reader-byte reader)))
          do (when (< digits +pdf-maximum-integer-digits+)
               (setf whole (+ (* whole 10) (- (reader-byte reader) 48))))
             (incf digits)
             (incf (reader-position reader)))
    (when (eql (reader-byte reader) 46)
      (setf real t)
      (incf (reader-position reader))
      (loop while (and (reader-byte reader) (digit-byte-p (reader-byte reader)))
            do (when (< scale (expt 10 +pdf-maximum-integer-digits+))
                 (setf fraction (+ (* fraction 10) (- (reader-byte reader) 48))
                       scale (* scale 10)))
               (incf (reader-position reader))))
    (if real
        (* sign (+ (coerce whole 'double-float) (/ (coerce fraction 'double-float) scale)))
        (* sign whole))))

(defun read-pdf-name (reader)
  "Read a name at the reader, which is at its slash, and return it as a string without the slash.

READER: the reader."
  (incf (reader-position reader))
  (let ((characters '()))
    (loop while (and (reader-byte reader) (pdf-regular-p (reader-byte reader)))
          do (let ((byte (reader-byte reader)))
               (incf (reader-position reader))
               (if (and (= byte 35)
                        (reader-byte reader)
                        (reader-byte reader 1)
                        (digit-char-p (code-char (reader-byte reader)) 16)
                        (digit-char-p (code-char (reader-byte reader 1)) 16))
                   (progn (push (code-char (+ (* 16 (digit-char-p (code-char (reader-byte reader)) 16))
                                              (digit-char-p (code-char (reader-byte reader 1)) 16)))
                                characters)
                          (incf (reader-position reader) 2))
                   (push (code-char byte) characters))))
    (coerce (nreverse characters) 'string)))

(defun read-pdf-literal-string (reader)
  "Read a string in parentheses at the reader and return its bytes, with the escapes resolved.

READER: the reader."
  (incf (reader-position reader))
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (depth 1))
    (loop
      (let ((byte (reader-byte reader)))
        (unless byte
          (pdf-damaged))
        (incf (reader-position reader))
        (cond ((= byte 92)
               (let ((next (reader-byte reader)))
                 (unless next
                   (pdf-damaged))
                 (incf (reader-position reader))
                 (case next
                   (110 (vector-push-extend 10 bytes))
                   (114 (vector-push-extend 13 bytes))
                   (116 (vector-push-extend 9 bytes))
                   (98 (vector-push-extend 8 bytes))
                   (102 (vector-push-extend 12 bytes))
                   (13 (when (eql (reader-byte reader) 10)
                         (incf (reader-position reader))))
                   (10)
                   (t (if (<= 48 next 55)
                          (let ((value (- next 48)))
                            (loop repeat 2
                                  while (and (reader-byte reader) (<= 48 (reader-byte reader) 55))
                                  do (setf value (+ (* value 8) (- (reader-byte reader) 48)))
                                     (incf (reader-position reader)))
                            (vector-push-extend (logand value 255) bytes))
                          (vector-push-extend next bytes))))))
              ((= byte 40)
               (incf depth)
               (vector-push-extend byte bytes))
              ((= byte 41)
               (decf depth)
               (when (zerop depth)
                 (return))
               (vector-push-extend byte bytes))
              (t
               (vector-push-extend byte bytes)))))
    (coerce bytes 'octets)))

(defun read-pdf-hex-string (reader)
  "Read a string in angle brackets at the reader and return its bytes.

READER: the reader."
  (incf (reader-position reader))
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (high nil))
    (loop
      (let ((byte (reader-byte reader)))
        (unless byte
          (pdf-damaged))
        (incf (reader-position reader))
        (cond ((= byte 62)
               (when high
                 (vector-push-extend (* 16 high) bytes))
               (return))
              ((pdf-white-p byte))
              ((digit-char-p (code-char byte) 16)
               (let ((digit (digit-char-p (code-char byte) 16)))
                 (if high
                     (progn (vector-push-extend (+ (* 16 high) digit) bytes)
                            (setf high nil))
                     (setf high digit))))
              (t
               (pdf-damaged)))))
    (coerce bytes 'octets)))

(defun read-pdf-word (reader)
  "Read a run of regular bytes at the reader and return it as a string.

READER: the reader."
  (let ((start (reader-position reader)))
    (loop while (and (reader-byte reader) (pdf-regular-p (reader-byte reader)))
          do (incf (reader-position reader)))
    (map 'string #'code-char (subseq (reader-data reader) start (reader-position reader)))))

(defun maybe-read-reference (reader
                             number)
  "When the reader is at a generation number and the letter R, read them and return the reference to NUMBER;
otherwise leave the reader where it was and return NIL.

READER: the reader, just after an integer.
NUMBER: that integer."
  (let ((saved (reader-position reader)))
    (skip-pdf-white reader)
    (let ((generation (and (reader-byte reader)
                           (digit-byte-p (reader-byte reader))
                           (read-pdf-integer-token reader))))
      (when generation
        (skip-pdf-white reader)
        (when (and (eql (reader-byte reader) 82)
                   (let ((after (reader-byte reader 1)))
                     (or (null after) (not (pdf-regular-p after)))))
          (incf (reader-position reader))
          (return-from maybe-read-reference (list :ref number generation)))))
    (setf (reader-position reader) saved)
    nil))

(defun read-pdf-value (reader
                       &key (references t)
                            (depth 0))
  "Read the value at the reader and return it, or :EOF when only white space is left. Words that are not numbers,
booleans or null come back as operators.

READER: the reader.
REFERENCES: true to read `12 0 R` as a reference; content streams have none.
DEPTH: how many arrays and dictionaries enclose this value."
  (when (> depth +pdf-maximum-depth+)
    (pdf-damaged))
  (skip-pdf-white reader)
  (let ((byte (reader-byte reader)))
    (cond ((null byte)
           :eof)
          ((or (digit-byte-p byte) (member byte '(43 45 46)))
           (let ((number (read-pdf-number reader)))
             (cond ((and references
                         (digit-byte-p byte)
                         (integerp number)
                         (maybe-read-reference reader number)))
                   (t number))))
          ((= byte 47)
           (read-pdf-name reader))
          ((= byte 40)
           (read-pdf-literal-string reader))
          ((and (= byte 60) (eql (reader-byte reader 1) 60))
           (incf (reader-position reader) 2)
           (let ((dictionary (make-hash-table :test 'equal)))
             (loop
               (skip-pdf-white reader)
               (cond ((and (eql (reader-byte reader) 62) (eql (reader-byte reader 1) 62))
                      (incf (reader-position reader) 2)
                      (return dictionary))
                     ((null (reader-byte reader))
                      (pdf-damaged))
                     (t
                      (let ((key (read-pdf-value reader
                                                 :references
                                                 references
                                                 :depth
                                                 (1+ depth))))
                        (unless (stringp key)
                          (pdf-damaged))
                        (let ((value (read-pdf-value reader
                                                     :references
                                                     references
                                                     :depth
                                                     (1+ depth))))
                          (when (or (eq value :eof)
                                    (and (consp value) (eq (car value) :op)))
                            (pdf-damaged))
                          (setf (gethash key dictionary) value))))))))
          ((= byte 60)
           (read-pdf-hex-string reader))
          ((= byte 91)
           (incf (reader-position reader))
           (let ((items '()))
             (loop
               (skip-pdf-white reader)
               (cond ((eql (reader-byte reader) 93)
                      (incf (reader-position reader))
                      (return (coerce (nreverse items) 'simple-vector)))
                     ((null (reader-byte reader))
                      (pdf-damaged))
                     (t
                      (let ((item (read-pdf-value reader
                                                  :references
                                                  references
                                                  :depth
                                                  (1+ depth))))
                        (when (eq item :eof)
                          (pdf-damaged))
                        (push item items)))))))
          ((member byte '(41 62 93 123 125))
           (incf (reader-position reader))
           (cons :op (string (code-char byte))))
          (t
           (let ((word (read-pdf-word reader)))
             (cond ((zerop (length word))
                    (incf (reader-position reader))
                    (cons :op "?"))
                   ((string= word "true") :true)
                   ((string= word "false") :false)
                   ((string= word "null") :null)
                   (t (cons :op word))))))))

(defun pdf-operator-p (value
                       &optional name)
  "Return true when VALUE is an operator, and when NAME is given, that one.

VALUE: what the reader returned.
NAME: the word of the operator, or NIL for any."
  (and (consp value)
       (eq (car value) :op)
       (or (null name)
           (string= (cdr value) name))))

(defun pdf-reference-p (value)
  "Return true when VALUE is an indirect reference.

VALUE: what the reader returned."
  (and (consp value)
       (eq (car value) :ref)))

(defun pdf-number-p (value)
  "Return true when VALUE is a number of the PDF.

VALUE: what the reader returned."
  (realp value))

(defun pdf-dictionary-p (value)
  "Return true when VALUE is a dictionary.

VALUE: what the reader returned."
  (hash-table-p value))
