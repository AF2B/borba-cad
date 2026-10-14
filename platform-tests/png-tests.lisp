(in-package #:borba-cad/platform-tests)

(def-suite png
  :description "The screenshot encoder."
  :in all)

(in-suite png)

(defun read-octets (path)
  "Return the contents of the binary file PATH as an octet vector.

PATH: the file."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun big-endian-at (octets
                      position)
  "Return the unsigned 32-bit big-endian integer starting at POSITION.

OCTETS: the octet vector.
POSITION: index of the first octet."
  (reduce (lambda (accumulated index) (+ (* 256 accumulated) (aref octets index)))
          (loop for index from position below (+ position 4) collect index)
          :initial-value 0))

(defun png-chunks (octets)
  "Split a PNG file into a list of (type data crc-ok) after checking its signature.

OCTETS: the file contents."
  (is (equalp #(137 80 78 71 13 10 26 10) (subseq octets 0 8)))
  (loop with position = 8
        while (< position (length octets))
        collect (let* ((length (big-endian-at octets position))
                       (type (map 'string #'code-char (subseq octets (+ position 4) (+ position 8))))
                       (data (subseq octets (+ position 8) (+ position 8 length)))
                       (stored-crc (big-endian-at octets (+ position 8 length))))
                  (incf position (+ 12 length))
                  (list type
                        data
                        (= stored-crc (platform::crc32 (concatenate '(vector (unsigned-byte 8))
                                                                    (map 'vector #'char-code type)
                                                                    data)))))))

(defun inflate-stored (zlib-stream)
  "Return the payload of a zlib stream made only of stored blocks, after verifying the block
structure and the Adler-32 trailer.

ZLIB-STREAM: the octets of the stream."
  (is (= #x78 (aref zlib-stream 0)))
  (let ((position 2)
        (payload (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop
      (let* ((final (logbitp 0 (aref zlib-stream position)))
             (length (+ (aref zlib-stream (+ position 1)) (* 256 (aref zlib-stream (+ position 2)))))
             (complement (+ (aref zlib-stream (+ position 3)) (* 256 (aref zlib-stream (+ position 4))))))
        (is (= length (logxor complement #xFFFF)) "stored block length and its complement disagree")
        (loop for index from (+ position 5) below (+ position 5 length)
              do (vector-push-extend (aref zlib-stream index) payload))
        (incf position (+ 5 length))
        (when final
          (return))))
    (is (= (platform::adler32 payload) (big-endian-at zlib-stream position))
        "the Adler-32 trailer does not match the payload")
    (coerce payload '(simple-array (unsigned-byte 8) (*)))))

(test known-checksums-match-their-published-values
  (is (= #xCBF43926 (platform::crc32 (map 'vector #'char-code "123456789"))))
  (is (= #x091E01DE (platform::adler32 (map 'vector #'char-code "123456789")))))

(test a-written-png-decodes-back-to-the-source-pixels
  (let* ((width 5)
         (height 3)
         (pixels (make-array (* width height 3) :element-type '(unsigned-byte 8)))
         (path (merge-pathnames "borba-cad-png-test.png" (uiop:temporary-directory))))
    (dotimes (index (length pixels))
      (setf (aref pixels index) (mod (* 37 index) 256)))
    (unwind-protect
         (progn
           (platform:write-rgb-png path width height pixels :flip-vertical nil)
           (let* ((chunks (png-chunks (read-octets path)))
                  (types (mapcar #'first chunks)))
             (is (equal '("IHDR" "IDAT" "IEND") types))
             (is (every #'third chunks) "a chunk has a wrong CRC")
             (let ((header (second (first chunks))))
               (is (= width (big-endian-at header 0)))
               (is (= height (big-endian-at header 4)))
               (is (equalp #(8 2 0 0 0) (subseq header 8))))
             (let* ((raw (inflate-stored (second (second chunks))))
                    (stride (* width 3)))
               (is (= (* height (1+ stride)) (length raw)))
               (dotimes (row height)
                 (is (zerop (aref raw (* row (1+ stride)))) "filter byte of row ~d" row)
                 (is (equalp (subseq pixels (* row stride) (* (1+ row) stride))
                             (subseq raw (1+ (* row (1+ stride))) (* (1+ row) (1+ stride)))))))))
      (uiop:delete-file-if-exists path))))

(test flipping-vertically-reverses-the-row-order
  (let* ((pixels (coerce '(1 1 1  2 2 2) '(simple-array (unsigned-byte 8) (*))))
         (path (merge-pathnames "borba-cad-png-flip-test.png" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (platform:write-rgb-png path 1 2 pixels :flip-vertical t)
           (let ((raw (inflate-stored (second (second (png-chunks (read-octets path)))))))
             (is (equalp #(0 2 2 2 0 1 1 1) raw))))
      (uiop:delete-file-if-exists path))))

(test an-image-larger-than-one-stored-block-is-split-and-still-valid
  (let* ((width 200)
         (height 120)
         (pixels (make-array (* width height 3) :element-type '(unsigned-byte 8) :initial-element 7))
         (path (merge-pathnames "borba-cad-png-large-test.png" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (platform:write-rgb-png path width height pixels :flip-vertical nil)
           (let ((raw (inflate-stored (second (second (png-chunks (read-octets path)))))))
             (is (= (* height (1+ (* width 3))) (length raw)))
             (is (> (length raw) 65535) "the test image must need several stored blocks")))
      (uiop:delete-file-if-exists path))))
