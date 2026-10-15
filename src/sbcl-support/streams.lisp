(in-package #:borba-cad/sbcl-support)

;;;; An output stream that keeps only the start of what is written to it, so that a command that prints
;;;; without end cannot fill the memory of the program.

(defclass limited-output-stream (sb-gray:fundamental-character-output-stream)
  ((limit :initarg :limit :reader stream-limit)
   (sink :initform (make-string-output-stream) :reader stream-sink)
   (count :initform 0 :accessor stream-count)
   (truncated :initform nil :accessor stream-truncated))
  (:documentation "A character output stream that keeps the first LIMIT characters written to it and
drops the rest."))

(defun make-limited-output-stream (limit)
  "Return an output stream that keeps the first LIMIT characters written to it.

LIMIT: the most characters to keep."
  (make-instance 'limited-output-stream :limit limit))

(defmethod sb-gray:stream-write-char ((stream limited-output-stream)
                                      character)
  (cond ((< (stream-count stream) (stream-limit stream))
         (write-char character (stream-sink stream))
         (incf (stream-count stream)))
        (t
         (setf (stream-truncated stream) t)))
  character)

(defmethod sb-gray:stream-line-column ((stream limited-output-stream))
  nil)

(defun limited-output-text (stream)
  "Return what a limited stream kept, and true when it dropped some of what was written to it.

STREAM: a limited output stream."
  (values (let ((text (get-output-stream-string (stream-sink stream))))
            (setf (stream-count stream) 0)
            text)
          (stream-truncated stream)))
