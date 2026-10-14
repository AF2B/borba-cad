(in-package #:borba-cad/persistence)

;;;; A small, safe S-expression reader. It never evaluates anything, never interns symbols and
;;;; bounds every resource: nesting depth, token length and numeric exponents. Every error
;;;; carries the line and column where it was detected.
;;;;
;;;; Grammar (whitespace separates tokens; ; starts a comment to the end of the line):
;;;;   node   = list | string | number | word
;;;;   list   = "(" node* ")"
;;;;   string = '"' (any character except control characters, \" and \\ , or \" or \\ ) '"'
;;;;   number = [+-] digits [ "." digits ] [ ("e"|"E") [+-] digits ]    (also ".5" style)
;;;;   word   = letter (letter | digit | "-")*

(defconstant +maximum-nesting-depth+ 8
  "Deepest list nesting accepted. The format needs 4; the limit protects the stack.")

(defconstant +maximum-token-length+ 64
  "Longest word or number accepted, in characters. Doubles print in at most 24.")

(defconstant +maximum-decimal-exponent+ 330
  "Largest decimal exponent accepted in a number; doubles stop at about 308.")

(defvar *file-path* nil
  "Path of the file being read, used only to label error messages. Bound by the loader.")

(defstruct (source (:constructor make-source (text)))
  "A cursor over the text being read.

TEXT: the characters.
POSITION: index of the next character.
LINE: current line, from 1.
LINE-START: index of the first character of the current line."
  (text "" :type (simple-array character (*)) :read-only t)
  (position 0 :type fixnum)
  (line 1 :type fixnum)
  (line-start 0 :type fixnum))

(defstruct (node (:constructor make-node (kind
                                          value
                                          line
                                          column)))
  "One element read from the text.

KIND: :list, :word, :string, :integer or :real.
VALUE: list of nodes, string, string, integer or double-float respectively.
LINE: line where the element starts.
COLUMN: column where the element starts."
  (kind :word :type keyword :read-only t)
  value
  (line 1 :type fixnum :read-only t)
  (column 1 :type fixnum :read-only t))

(defun source-column (source)
  "Return the 1-based column of the cursor.

SOURCE: the source."
  (1+ (- (source-position source) (source-line-start source))))

(defun syntax-failure (source
                       control
                       &rest arguments)
  "Signal a DRAWING-FILE-SYNTAX-ERROR at the cursor.

SOURCE: the source, for the position.
CONTROL: format control string.
ARGUMENTS: format arguments."
  (error 'base:drawing-file-syntax-error
         :path *file-path*
         :line (source-line source)
         :column (source-column source)
         :reason (apply #'format nil control arguments)))

(defun content-failure (node
                        control
                        &rest arguments)
  "Signal a DRAWING-FILE-CONTENT-ERROR at the position of NODE.

NODE: the offending element.
CONTROL: format control string.
ARGUMENTS: format arguments."
  (error 'base:drawing-file-content-error
         :path *file-path*
         :line (node-line node)
         :column (node-column node)
         :reason (apply #'format nil control arguments)))

(declaim (inline source-peek))
(defun source-peek (source)
  "Return the next character without consuming it, or NIL at the end.

SOURCE: the source."
  (let ((text (source-text source))
        (position (source-position source)))
    (and (< position (length text))
         (schar text position))))

(defun source-advance (source)
  "Consume one character, tracking line and column.

SOURCE: the source."
  (let ((text (source-text source))
        (position (source-position source)))
    (when (char= (schar text position) #\Newline)
      (incf (source-line source))
      (setf (source-line-start source) (1+ position)))
    (incf (source-position source))))

(defun blank-character-p (character)
  "Return true for characters that separate tokens.

CHARACTER: a character."
  (member character '(#\Space #\Tab #\Newline #\Return)))

(defun skip-blanks (source)
  "Consume whitespace and comments.

SOURCE: the source."
  (loop
    (let ((character (source-peek source)))
      (cond ((null character) (return))
            ((blank-character-p character) (source-advance source))
            ((char= character #\;)
             (loop for next = (source-peek source)
                   while (and next (char/= next #\Newline))
                   do (source-advance source)))
            (t (return))))))

(defun delimiter-p (character)
  "Return true for characters that end a word or number.

CHARACTER: a character or NIL."
  (or (null character)
      (blank-character-p character)
      (member character '(#\( #\) #\" #\;))))

(defun read-string-node (source)
  "Read a quoted string; the cursor is on the opening quote.

SOURCE: the source."
  (let ((line (source-line source))
        (column (source-column source))
        (characters (make-string-output-stream)))
    (source-advance source)
    (loop
      (let ((character (source-peek source)))
        (cond ((null character)
               (syntax-failure source
                               "the string opened at line ~d, column ~d is never closed"
                               line
                               column))
              ((char= character #\")
               (source-advance source)
               (return (make-node :string
                                  (get-output-stream-string characters)
                                  line
                                  column)))
              ((char= character #\\)
               (source-advance source)
               (let ((escaped (source-peek source)))
                 (unless (and escaped (member escaped '(#\" #\\)))
                   (syntax-failure source "a backslash must be followed by a quote or a backslash"))
                 (write-char escaped characters)
                 (source-advance source)))
              ((< (char-code character) 32)
               (syntax-failure source "control characters are not allowed inside strings"))
              (t (write-char character characters)
                 (source-advance source)))))))

(defun parse-number-token (token)
  "Interpret TOKEN as a number. Returns two values: the number (an integer or a double-float)
and NIL, or NIL and a reason string when TOKEN is not a valid number.

TOKEN: the characters of one word-like element."
  (let ((length (length token))
        (position 0)
        (negative nil)
        (mantissa 0)
        (fraction-digits 0)
        (digit-count 0)
        (seen-point nil)
        (exponent 0)
        (exponent-negative nil)
        (seen-exponent nil))
    (flet ((fail (reason)
             (return-from parse-number-token (values nil reason))))
      (when (and (< position length) (find (char token position) "+-"))
        (setf negative (char= (char token position) #\-))
        (incf position))
      (loop while (and (< position length)
                       (or (digit-char-p (char token position))
                           (and (not seen-point) (char= (char token position) #\.))))
            do (let ((character (char token position)))
                 (if (char= character #\.)
                     (setf seen-point t)
                     (progn (setf mantissa (+ (* mantissa 10) (digit-char-p character)))
                            (incf digit-count)
                            (when seen-point (incf fraction-digits)))))
               (incf position))
      (when (zerop digit-count)
        (fail "it is not a number"))
      (when (and (< position length) (find (char token position) "eE"))
        (setf seen-exponent t)
        (incf position)
        (when (and (< position length) (find (char token position) "+-"))
          (setf exponent-negative (char= (char token position) #\-))
          (incf position))
        (let ((exponent-digits 0))
          (loop while (and (< position length) (digit-char-p (char token position)))
                do (setf exponent (+ (* exponent 10) (digit-char-p (char token position))))
                   (incf exponent-digits)
                   (incf position))
          (when (zerop exponent-digits)
            (fail "the exponent has no digits"))))
      (when (< position length)
        (fail "it is not a number"))
      (when (> exponent +maximum-decimal-exponent+)
        (fail "the exponent is out of range"))
      (if (and (not seen-point) (not seen-exponent))
          (values (if negative (- mantissa) mantissa) nil)
          (let* ((decimal-exponent (- (if exponent-negative (- exponent) exponent) fraction-digits))
                 (value (* mantissa (expt 10 decimal-exponent))))
            (when (> (abs value) (load-time-value (rational most-positive-double-float)))
              (fail "the number is too large for a double-float"))
            (values (if (zerop value)
                        (if negative -0d0 0d0)
                        (handler-case (let ((double (coerce value 'double-float)))
                                        (if negative (- double) double))
                          (arithmetic-error () (fail "the number cannot be represented"))))
                    nil))))))

(defun read-atom-node (source)
  "Read a word or number; the cursor is on its first character.

SOURCE: the source."
  (let ((line (source-line source))
        (column (source-column source))
        (start (source-position source)))
    (loop until (delimiter-p (source-peek source))
          do (source-advance source)
             (when (> (- (source-position source) start) +maximum-token-length+)
               (syntax-failure source
                               "an element is longer than ~d characters"
                               +maximum-token-length+)))
    (let* ((token (subseq (source-text source) start (source-position source)))
           (first (char token 0)))
      (cond ((or (digit-char-p first) (find first "+-."))
             (multiple-value-bind (number reason) (parse-number-token token)
               (when reason
                 (error 'base:drawing-file-syntax-error
                        :path *file-path*
                        :line line
                        :column column
                        :reason (format nil "~s: ~a" token reason)))
               (make-node (if (integerp number) :integer :real)
                          number
                          line
                          column)))
            ((and (alpha-char-p first)
                  (every (lambda (character)
                           (or (alphanumericp character) (char= character #\-)))
                         token)
                  (every (lambda (character) (< (char-code character) 128)) token))
             (make-node :word
                        token
                        line
                        column))
            (t (error 'base:drawing-file-syntax-error
                      :path *file-path*
                      :line line
                      :column column
                      :reason (format nil "unexpected character sequence ~s" token)))))))

(declaim (ftype function read-node))

(defun read-list-tail (source
                       line
                       column
                       depth)
  "Read the elements of a list whose opening parenthesis was already consumed, up to and
including the closing parenthesis; returns the list node.

SOURCE: the source.
LINE: line of the opening parenthesis.
COLUMN: column of the opening parenthesis.
DEPTH: nesting depth of the list being read."
  (when (> depth +maximum-nesting-depth+)
    (syntax-failure source
                    "lists are nested deeper than ~d levels"
                    +maximum-nesting-depth+))
  (let ((elements '()))
    (loop
      (skip-blanks source)
      (let ((character (source-peek source)))
        (cond ((null character)
               (syntax-failure source
                               "the list opened at line ~d, column ~d is never closed"
                               line
                               column))
              ((char= character #\))
               (source-advance source)
               (return (make-node :list
                                  (nreverse elements)
                                  line
                                  column)))
              (t (push (read-node source depth) elements)))))))

(defun read-node (source
                  depth)
  "Read one element at the cursor.

SOURCE: the source.
DEPTH: nesting depth of the enclosing list, 0 at the top."
  (skip-blanks source)
  (let ((character (source-peek source)))
    (cond ((null character)
           (syntax-failure source "unexpected end of the text"))
          ((char= character #\()
           (let ((line (source-line source))
                 (column (source-column source)))
             (source-advance source)
             (read-list-tail source
                             line
                             column
                             (1+ depth))))
          ((char= character #\))
           (syntax-failure source "unexpected closing parenthesis"))
          ((char= character #\")
           (read-string-node source))
          (t (read-atom-node source)))))
