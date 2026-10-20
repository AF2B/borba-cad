(in-package #:borba-cad/tests)

(def-suite pictures
  :description "Reading pictures: DEFLATE streams, PNG and JPEG files of every kind, and Base64."
  :in all)

(in-suite pictures)

(defun picture-fixture (name)
  "Return the bytes of the picture fixture called NAME."
  (with-open-file (stream (fixture-path (format nil "images/~a" name)) :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun picture-fixture-names (prefix extension)
  "Return the names, without the extension, of the picture fixtures that begin with PREFIX and end with EXTENSION."
  (sort (mapcar #'pathname-name
                (directory (merge-pathnames (format nil "~a*.~a" prefix extension)
                                            (fixture-path "images/"))))
        #'string<))

(defun octets-of (&rest bytes)
  "Return a vector of bytes holding BYTES."
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun largest-difference (raster
                           expected)
  "Return the largest difference between a pixel byte of RASTER and the one of EXPECTED, or :SIZE when the vectors do
not have the same length."
  (let ((pixels (image::raster-pixels raster)))
    (if (/= (length pixels) (length expected))
        :size
        (loop for index below (length pixels)
              maximize (abs (- (aref pixels index) (aref expected index)))))))

(defun image-error-p (thunk)
  "Return true when calling THUNK signals IMAGE-ERROR, and false when it returns; any other condition goes on."
  (handler-case (progn (funcall thunk)
                       nil)
    (base:image-error ()
      t)))

(defun damaged-copies (bytes
                       count
                       seed)
  "Return COUNT copies of BYTES each with a few bytes changed at random, and some of them cut short, from a generator
started with SEED."
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat count
          collect (let ((copy (copy-seq bytes)))
                    (dotimes (each (1+ (random 4 state)))
                      (setf (aref copy (random (length copy) state)) (random 256 state)))
                    (if (zerop (random 3 state))
                        (subseq copy 0 (random (length copy) state))
                        copy)))))

;;; DEFLATE

(test inflate-reads-a-stored-block
  (is (equalp (map 'vector #'char-code "hello hello hello hello hello")
              (image::inflate (octets-of #x78 #x01 #x01 #x1D #x00 #xE2 #xFF #x68 #x65 #x6C #x6C #x6F #x20 #x68 #x65
                                         #x6C #x6C #x6F #x20 #x68 #x65 #x6C #x6C #x6F #x20 #x68 #x65 #x6C #x6C #x6F
                                         #x20 #x68 #x65 #x6C #x6C #x6F #xA3 #x10 #x0A #xE5)))))

(test inflate-reads-a-block-with-the-fixed-codes
  (is (equalp (map 'vector #'char-code "hello hello hello hello hello")
              (image::inflate (octets-of #x78 #x01 #xCB #x48 #xCD #xC9 #xC9 #x57 #xC8 #xC0 #x4E #x02 #x00 #xA3 #x10
                                         #x0A #xE5)))))

(test inflate-says-where-the-stream-ends
  (let ((stream (octets-of #x78 #x9C #xCB #x48 #xCD #xC9 #xC9 #x57 #xC8 #xC0 #x4E #x02 #x00 #xA3 #x10 #x0A #xE5
                           #xFF #xFF)))
    (multiple-value-bind (bytes next)
        (image::inflate stream)
      (is (= 29 (length bytes)))
      (is (= 17 next)))))

(test inflate-reads-the-picture-data-of-every-png-fixture
  (dolist (name (picture-fixture-names "png-" "png"))
    (is (not (image-error-p (lambda () (image::read-png (picture-fixture (format nil "~a.png" name))))))
        "~a" name)))

(test inflate-refuses-a-damaged-or-cut-stream
  (is (image-error-p (lambda () (image::inflate (octets-of #x78 #x9C #xCB #x48 #xCD #xC9)))))
  (is (image-error-p (lambda () (image::inflate (octets-of #x79 #x9C #xCB #x48)))))
  (is (image-error-p (lambda () (image::inflate (octets-of #x78 #x9C #x07 #x00)))))
  (is (image-error-p (lambda () (image::inflate (octets-of #x78 #x01 #x01 #x1D #x00 #x00 #x00 #x68)))))
  (is (image-error-p (lambda () (image::inflate (octets-of))))))

(test inflate-stops-at-the-limit-it-is-given
  (let ((stream (octets-of #x78 #x9C #xCB #x48 #xCD #xC9 #xC9 #x57 #xC8 #xC0 #x4E #x02 #x00 #xA3 #x10 #x0A #xE5)))
    (is (= 29 (length (image::inflate stream :limit 29))))
    (is (image-error-p (lambda () (image::inflate stream :limit 28))))))

(test inflate-meets-damage-only-with-image-errors
  (let ((stream (octets-of #x78 #x9C #xCB #x48 #xCD #xC9 #xC9 #x57 #xC8 #xC0 #x4E #x02 #x00 #xA3 #x10 #x0A #xE5)))
    (dolist (copy (damaged-copies stream 400 11))
      (finishes (image-error-p (lambda () (image::inflate copy :limit 4096)))))))

;;; PNG

(test every-png-fixture-decodes-to-the-pixels-the-reference-decoder-gives
  (let ((names (picture-fixture-names "png-" "png")))
    (is (>= (length names) 20))
    (dolist (name names)
      (let ((raster (image::read-picture (picture-fixture (format nil "~a.png" name)))))
        (is (= 13 (image::raster-width raster)) "~a" name)
        (is (= 11 (image::raster-height raster)) "~a" name)
        (is (eql 0 (largest-difference raster (picture-fixture (format nil "~a.rgba" name)))) "~a" name)))))

(test a-png-with-a-transparent-key-or-palette-entry-has-clear-pixels
  (let ((palette (image::raster-pixels (image::read-picture (picture-fixture "png-pal8-trns.png"))))
        (opaque (image::raster-pixels (image::read-picture (picture-fixture "png-pal8.png")))))
    (is (some (lambda (index) (< (aref palette (+ 3 (* 4 index))) 255))
              (loop for index below (floor (length palette) 4) collect index)))
    (is (every (lambda (index) (= 255 (aref opaque (+ 3 (* 4 index)))))
               (loop for index below (floor (length opaque) 4) collect index)))))

(defun patch-png-chunk (bytes
                        kind
                        offset
                        value
                        &key (width 4))
  "Return a copy of the PNG BYTES with the number VALUE written, WIDTH bytes wide, at OFFSET in the first chunk of
type KIND, and the CRC of the chunk made right again."
  (let ((copy (copy-seq bytes))
        (position 8))
    (loop
      (let ((size (image::octet-long copy position))
            (type (map 'string #'code-char (subseq copy (+ position 4) (+ position 8)))))
        (when (string= type kind)
          (dotimes (index width)
            (setf (aref copy (+ position 8 offset index)) (ldb (byte 8 (* 8 (- width index 1))) value)))
          (let ((crc (image::crc-32 copy (+ position 4) (+ position 8 size))))
            (dotimes (index 4)
              (setf (aref copy (+ position 8 size index)) (ldb (byte 8 (* 8 (- 3 index))) crc))))
          (return copy))
        (incf position (+ 12 size))))))

(test a-png-that-is-not-whole-is-refused-with-a-reason
  (let ((bytes (picture-fixture "png-rgb8.png")))
    (is (image-error-p (lambda () (image::read-png (subseq bytes 0 (floor (length bytes) 2))))))
    (is (image-error-p (lambda () (image::read-png (subseq bytes 0 20)))))
    (is (image-error-p (lambda () (image::read-png (octets-of)))))
    (let ((corrupt (copy-seq bytes))
          (data (+ 8 (search (octets-of #x49 #x44 #x41 #x54) bytes))))
      (setf (aref corrupt data) (logxor 255 (aref corrupt data)))
      (is (image-error-p (lambda () (image::read-png corrupt)))))
    (is (image-error-p (lambda () (image::read-png (patch-png-chunk bytes "IHDR" 8 7 :width 1)))))
    (is (image-error-p (lambda () (image::read-png (patch-png-chunk bytes "IHDR" 9 5 :width 1)))))
    (is (image-error-p (lambda () (image::read-png (patch-png-chunk bytes "IHDR" 12 2 :width 1)))))
    (is (image-error-p (lambda () (image::read-png (patch-png-chunk bytes "IHDR" 0 0)))))))

(test a-png-that-claims-an-enormous-size-is-refused-before-any-memory-is-given
  (let ((bytes (picture-fixture "png-rgb8.png")))
    (is (image-error-p (lambda () (image::read-png (patch-png-chunk bytes "IHDR" 0 60000)))))
    (is (image-error-p (lambda () (image::read-png (patch-png-chunk bytes "IHDR" 0 #x7FFFFFFF)))))
    (let ((tall (patch-png-chunk (patch-png-chunk bytes "IHDR" 0 20000) "IHDR" 4 20000)))
      (handler-case (progn (image::read-png tall)
                           (fail "A picture of 400 million pixels was read."))
        (base:image-error (condition)
          (is (search "larger" (princ-to-string condition))))))))

(test damaged-pngs-are-met-only-with-image-errors
  (dolist (name '("png-rgb8" "png-pal4-adam7" "png-gray1" "png-rgba16"))
    (dolist (copy (damaged-copies (picture-fixture (format nil "~a.png" name)) 150 5))
      (finishes (image-error-p (lambda () (image::read-png copy)))))))

;;; JPEG

(test every-jpeg-fixture-decodes-to-the-pixels-the-reference-decoder-gives
  (let ((names (picture-fixture-names "jpeg-" "jpg")))
    (is (>= (length names) 15))
    (dolist (name names)
      (let* ((raster (image::read-picture (picture-fixture (format nil "~a.jpg" name))))
             (difference (largest-difference raster (picture-fixture (format nil "~a.rgba" name)))))
        (is (= 13 (image::raster-width raster)) "~a" name)
        (is (= 11 (image::raster-height raster)) "~a" name)
        ;; The reference of the CMYK file comes from another program that rounds the conversion to RGB its own way.
        (is (and (integerp difference)
                 (<= difference (if (string= name "jpeg-cmyk") 2 0)))
            "~a differs by ~a" name difference)))))

(test a-jpeg-that-is-not-whole-is-refused-with-a-reason
  (let ((bytes (picture-fixture "jpeg-s420.jpg")))
    (is (image-error-p (lambda () (image::read-jpeg (subseq bytes 0 (floor (length bytes) 2))))))
    (is (image-error-p (lambda () (image::read-jpeg (subseq bytes 0 30)))))
    (is (image-error-p (lambda () (image::read-jpeg (octets-of #xFF #xD8 #xFF #xD9)))))
    (is (image-error-p (lambda () (image::read-jpeg (octets-of 1 2 3 4 5)))))))

(defun position-of-marker (bytes
                           marker)
  "Return the position of the first segment of type MARKER in the JPEG BYTES."
  (loop for index from 2 below (1- (length bytes))
        when (and (= (aref bytes index) #xFF)
                  (= (aref bytes (1+ index)) marker))
          do (return index)))

(test a-jpeg-of-a-kind-that-is-not-read-says-so
  (let ((bytes (picture-fixture "jpeg-base444.jpg")))
    (let ((arithmetic (copy-seq bytes)))
      (setf (aref arithmetic (1+ (position-of-marker arithmetic #xC0))) #xC9)
      (handler-case (progn (image::read-jpeg arithmetic)
                           (fail "An arithmetic-coded file was read."))
        (base:image-error (condition)
          (is (search "arithmetic" (princ-to-string condition))))))
    (let ((twelve-bit (copy-seq bytes)))
      (setf (aref twelve-bit (+ 4 (position-of-marker twelve-bit #xC0))) 12)
      (is (image-error-p (lambda () (image::read-jpeg twelve-bit)))))))

(test a-jpeg-that-claims-an-enormous-size-is-refused-before-any-memory-is-given
  (let ((bytes (copy-seq (picture-fixture "jpeg-base444.jpg"))))
    (let ((at (position-of-marker bytes #xC0)))
      (setf (aref bytes (+ at 5)) #xFF
            (aref bytes (+ at 6)) #xFF
            (aref bytes (+ at 7)) #xFF
            (aref bytes (+ at 8)) #xFF))
    (handler-case (progn (image::read-jpeg bytes)
                         (fail "A picture of four billion pixels was read."))
      (base:image-error (condition)
        (is (search "larger" (princ-to-string condition)))))))

(test damaged-jpegs-are-met-only-with-image-errors
  (dolist (name '("jpeg-s420" "jpeg-prog420" "jpeg-progrst" "jpeg-rst420" "jpeg-cmyk" "jpeg-gray"))
    (dolist (copy (damaged-copies (picture-fixture (format nil "~a.jpg" name)) 150 9))
      (finishes (image-error-p (lambda () (image::read-jpeg copy)))))))

;;; Kinds

(test the-kind-of-a-picture-is-told-by-how-the-file-begins
  (is (eq :png (image:picture-kind (picture-fixture "png-rgb8.png"))))
  (is (eq :jpeg (image:picture-kind (picture-fixture "jpeg-s420.jpg"))))
  (is (eq :pdf (image:picture-kind (octets-of #x25 #x50 #x44 #x46 #x2D #x31))))
  (is (null (image:picture-kind (octets-of 1 2 3))))
  (is (null (image:picture-kind (octets-of)))))

(test a-file-of-another-kind-is-refused-by-the-reader-of-pictures
  (is (image-error-p (lambda () (image:read-picture (octets-of 1 2 3 4)))))
  (is (image-error-p (lambda () (image:read-picture (octets-of))))))

;;; Base64

(test base64-matches-the-test-vectors-of-the-standard
  (loop for (text encoded) in '(("" "") ("f" "Zg==") ("fo" "Zm8=") ("foo" "Zm9v") ("foob" "Zm9vYg==")
                                ("fooba" "Zm9vYmE=") ("foobar" "Zm9vYmFy"))
        do (let ((bytes (map '(simple-array (unsigned-byte 8) (*)) #'char-code text)))
             (is (string= encoded (base:base64-encode bytes)))
             (is (equalp bytes (base:base64-decode encoded))))))

(test base64-round-trips-every-length-and-every-byte
  (let ((state (sb-ext:seed-random-state 3)))
    (dotimes (length 70)
      (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
        (dotimes (index length)
          (setf (aref bytes index) (random 256 state)))
        (is (equalp bytes (base:base64-decode (base:base64-encode bytes))))))
    (let ((all (make-array 256 :element-type '(unsigned-byte 8))))
      (dotimes (index 256)
        (setf (aref all index) index))
      (is (equalp all (base:base64-decode (base:base64-encode all)))))))

(test base64-ignores-white-space-and-refuses-what-is-not-base64
  (is (equalp (octets-of 102 111 111) (base:base64-decode (format nil "Zm9v~%"))))
  (is (equalp (octets-of 102 111 111 98) (base:base64-decode (format nil "Zm9v Yg=~c=" #\Newline))))
  (is (null (base:base64-decode "Zm9")))
  (is (null (base:base64-decode "Zm9v!")))
  (is (null (base:base64-decode "Z===")))
  (is (null (base:base64-decode "Zg=a")))
  (is (null (base:base64-decode "A"))))
