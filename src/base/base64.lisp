(in-package #:borba-cad/base)

;;;; Base64 (RFC 4648): bytes written as text, so that a picture can sit inside a drawing file that is text.

(defconstant +base64-padding+ #\=
  "The character that fills the last group of a text whose bytes do not divide by three.")

(defparameter *base64-alphabet*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The sixty-four characters that stand for the values of six bits.")

(declaim (type (simple-array fixnum (128)) *base64-values*))

(defparameter *base64-values*
  (let ((table (make-array 128 :element-type 'fixnum :initial-element -1)))
    (dotimes (index 64 table)
      (setf (aref table (char-code (char *base64-alphabet* index))) index)))
  "The value of each character of the alphabet by its code, and -1 for the others.")

(defun base64-encode (octets)
  "Return the Base64 text of a vector of bytes.

OCTETS: the bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((size (length octets))
         (text (make-string (* 4 (ceiling size 3)) :element-type 'base-char :initial-element +base64-padding+))
         (out 0))
    (declare (type fixnum size out))
    (flet ((emit (value)
             (declare (type fixnum value))
             (setf (schar text out) (schar *base64-alphabet* (logand value 63)))
             (incf out)))
      (declare (inline emit))
      (loop for index of-type fixnum from 0 below (- size (mod size 3)) by 3
            do (let ((group (logior (ash (aref octets index) 16)
                                    (ash (aref octets (+ index 1)) 8)
                                    (aref octets (+ index 2)))))
                 (emit (ash group -18))
                 (emit (ash group -12))
                 (emit (ash group -6))
                 (emit group)))
      (case (mod size 3)
        (1 (let ((group (ash (aref octets (- size 1)) 16)))
             (emit (ash group -18))
             (emit (ash group -12))))
        (2 (let ((group (logior (ash (aref octets (- size 2)) 16)
                                (ash (aref octets (- size 1)) 8))))
             (emit (ash group -18))
             (emit (ash group -12))
             (emit (ash group -6))))))
    text))

(defun base64-decode (text)
  "Return the bytes that a Base64 text stands for, or NIL when the text is not valid Base64. White space is ignored.

TEXT: the Base64 string."
  (declare (type string text)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((text (coerce text '(simple-array character (*))))
         (groups (make-array (length text) :element-type '(unsigned-byte 8)))
         (count 0)
         (padding 0))
    (declare (type fixnum count padding)
             (type (simple-array character (*)) text))
    (loop for character across text
          for code = (char-code character)
          do (cond ((member character '(#\Space #\Newline #\Return #\Tab)))
                   ((char= character +base64-padding+)
                    (incf padding))
                   ((and (< code 128)
                         (zerop padding)
                         (>= (aref *base64-values* code) 0))
                    (setf (aref groups count) (aref *base64-values* code))
                    (incf count))
                   (t
                    (return-from base64-decode nil))))
    (unless (and (<= padding 2)
                 (/= (mod count 4) 1)
                 (zerop (mod (+ count padding) 4)))
      (return-from base64-decode nil))
    (let* ((size (- (floor (* 3 (+ count padding)) 4) padding))
           (octets (make-array size :element-type '(unsigned-byte 8)))
           (out 0))
      (declare (type fixnum size out))
      (loop for index of-type fixnum from 0 below count by 4
            do (let ((group 0))
                 (declare (type fixnum group))
                 (dotimes (each 4)
                   (setf group (logior (ash group 6)
                                       (if (< (+ index each) count) (aref groups (+ index each)) 0))))
                 (loop for shift of-type fixnum in '(16 8 0)
                       while (< out size)
                       do (setf (aref octets out) (logand #xFF (ash group (- shift))))
                          (incf out))))
      octets)))
