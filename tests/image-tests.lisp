(in-package #:borba-cad/tests)

(def-suite image
  :description "The JPEG encoder: its structure, what a decoder gets back from it, and its tables."
  :in all)

(in-suite image)

;;; A decoder, to check the encoder against: baseline, three components, no subsampling, as the encoder writes.

(defstruct jpeg-picture
  "What the test decoder found in a JPEG file."
  width height pixels (tables '()) (segments '()))

(defun jpeg-word (octets index)
  "Return the 16-bit number at INDEX."
  (+ (ash (aref octets index) 8) (aref octets (1+ index))))

(defun huffman-decoding-table (counts symbols)
  "Return a hash table from a code, written as its length and its bits, to its symbol."
  (let ((table (make-hash-table))
        (code 0)
        (remaining (coerce symbols 'list)))
    (loop for length from 1 to 16
          do (dotimes (index (aref counts (1- length)))
               (setf (gethash (+ (ash length 16) code) table) (pop remaining))
               (incf code))
             (setf code (ash code 1)))
    table))

(defun decode-jpeg (octets)
  "Decode a baseline JPEG that the encoder wrote, and return a JPEG-PICTURE with the pixels as an array of
bytes, row by row from the top, three each."
  (let ((index 2)
        (quantization (make-hash-table))
        (huffman (make-hash-table :test #'equal))
        (width 0)
        (height 0)
        (segments '()))
    (assert (and (= #xFF (aref octets 0)) (= #xD8 (aref octets 1))))
    (loop
      (assert (= #xFF (aref octets index)))
      (let* ((marker (aref octets (1+ index)))
             (length (jpeg-word octets (+ index 2)))
             (start (+ index 4)))
        (push marker segments)
        (case marker
          (#xDB (let ((id (logand 15 (aref octets start)))
                      (table (make-array 64)))
                  (dotimes (position 64)
                    (setf (aref table (aref image::*zigzag* position)) (aref octets (+ start 1 position))))
                  (setf (gethash id quantization) table)))
          (#xC0 (setf height (jpeg-word octets (+ start 1))
                      width (jpeg-word octets (+ start 3))))
          (#xC4 (let* ((class-and-id (aref octets start))
                       (counts (subseq octets (+ start 1) (+ start 17)))
                       (total (reduce #'+ counts))
                       (symbols (subseq octets (+ start 17) (+ start 17 total))))
                  (setf (gethash (list (ash class-and-id -4) (logand 15 class-and-id)) huffman)
                        (huffman-decoding-table counts symbols))))
          (#xDA (return (decode-jpeg-scan octets (+ index 2 length) quantization huffman width height
                                          (reverse segments)))))
        (incf index (+ 2 length))))))

(defun decode-jpeg-scan (octets start quantization huffman width height segments)
  "Decode the entropy-coded data that begins at START into a JPEG-PICTURE."
  (let ((position start)
        (buffer 0)
        (count 0)
        (columns (ceiling width 8))
        (rows (ceiling height 8))
        (planes (list (make-array (* (ceiling width 8) (ceiling height 8) 64))
                      (make-array (* (ceiling width 8) (ceiling height 8) 64))
                      (make-array (* (ceiling width 8) (ceiling height 8) 64)))))
    (labels ((next-bit ()
               (when (zerop count)
                 (let ((byte (aref octets position)))
                   (incf position)
                   (when (= byte #xFF)
                     (let ((next (aref octets position)))
                       (if (zerop next)
                           (incf position)
                           (error "A marker where data was expected: ~x" next))))
                   (setf buffer byte
                         count 8)))
               (decf count)
               (logand 1 (ash buffer (- count))))
             (bits (n)
               (let ((value 0))
                 (dotimes (each n value)
                   (setf value (+ (* 2 value) (next-bit))))))
             (decode (table)
               (let ((code 0))
                 (loop for length from 1 to 16
                       do (setf code (+ (* 2 code) (next-bit)))
                          (let ((symbol (gethash (+ (ash length 16) code) table)))
                            (when symbol
                              (return-from decode symbol))))
                 (error "No such code")))
             (extend (value size)
               (if (and (plusp size) (< value (ash 1 (1- size))))
                   (+ value (- (ash 1 size)) 1)
                   value)))
      (let ((previous (list 0 0 0)))
        (dotimes (block-index (* columns rows))
          (dotimes (component 3)
            (let* ((table-id (if (zerop component) 0 1))
                   (quant (gethash table-id quantization))
                   (dc-table (gethash (list 0 table-id) huffman))
                   (ac-table (gethash (list 1 table-id) huffman))
                   (coefficients (make-array 64 :initial-element 0))
                   (size (decode dc-table)))
              (incf (nth component previous) (extend (bits size) size))
              (setf (aref coefficients 0) (* (nth component previous) (aref quant 0)))
              (let ((k 1))
                (loop while (< k 64)
                      do (let* ((symbol (decode ac-table))
                                (run (ash symbol -4))
                                (size (logand 15 symbol)))
                           (cond ((zerop size)
                                  (if (= run 15)
                                      (incf k 16)
                                      (return)))
                                 (t
                                  (incf k run)
                                  (let ((place (aref image::*zigzag* k)))
                                    (setf (aref coefficients place) (* (extend (bits size) size) (aref quant place))))
                                  (incf k))))))
              (replace (nth component planes) coefficients :start1 (* block-index 64))))))
      (let ((pixels (make-array (* 3 width height) :element-type '(unsigned-byte 8))))
        (dotimes (y height)
          (dotimes (x width)
            (let* ((block-index (+ (* (floor y 8) columns) (floor x 8)))
                   (values (loop for plane in planes
                                 collect (let ((sum 0d0))
                                           (dotimes (u 8)
                                             (dotimes (v 8)
                                               (incf sum (* (aref image::*cosines* u (mod y 8))
                                                            (aref image::*cosines* v (mod x 8))
                                                            (aref plane (+ (* block-index 64) (* u 8) v))))))
                                           (+ sum 128d0))))
                   (luma (first values))
                   (blue (- (second values) 128d0))
                   (red (- (third values) 128d0))
                   (offset (* 3 (+ (* y width) x))))
              (flet ((clamp (value) (max 0 (min 255 (round value)))))
                (setf (aref pixels offset) (clamp (+ luma (* 1.402d0 red)))
                      (aref pixels (+ offset 1)) (clamp (- luma (* 0.344136d0 blue) (* 0.714136d0 red)))
                      (aref pixels (+ offset 2)) (clamp (+ luma (* 1.772d0 blue))))))))
        (make-jpeg-picture :width width :height height :pixels pixels :segments segments)))))

(defun psnr (width height original decoded)
  "Return the peak signal to noise ratio, in decibels, between two pictures; the more, the closer."
  (let ((sum 0d0))
    (dotimes (index (* 3 width height))
      (incf sum (expt (- (aref original index) (aref decoded index)) 2)))
    (if (zerop sum)
        200d0
        (* 10 (log (/ (* 255d0 255d0) (/ sum (* 3 width height))) 10)))))

(defun test-picture (width height)
  "Return a picture like a drawing: a dark background, a grid, a circle and a gradient."
  (let ((rgb (make-array (* 3 width height) :element-type '(unsigned-byte 8))))
    (dotimes (y height)
      (dotimes (x width)
        (let ((offset (* 3 (+ (* y width) x)))
              (red 20)
              (green 22)
              (blue 30))
          (when (< x (floor width 3))
            (setf red (floor (* 255 x) (max 1 (floor width 3)))
                  green (floor (* 255 y) height)
                  blue 128))
          (when (or (zerop (mod x 25)) (zerop (mod y 25)))
            (setf red 60 green 70 blue 90))
          (when (< (abs (- (sqrt (+ (expt (- x (* 0.7 width)) 2) (expt (- y (/ height 2)) 2))) (/ height 3))) 1.5)
            (setf red 255 green 80 blue 80))
          (setf (aref rgb offset) red
                (aref rgb (+ offset 1)) green
                (aref rgb (+ offset 2)) blue))))
    rgb))

(defun noise-picture (width height seed)
  "Return a picture of random pixels, which has every kind of symbol and many bytes of all ones."
  (let ((prng (rng:make-prng seed))
        (rgb (make-array (* 3 width height) :element-type '(unsigned-byte 8))))
    (dotimes (index (length rgb) rgb)
      (setf (aref rgb index) (rng:prng-next-below prng 256)))))

;;; Structure

(test a-jpeg-has-the-markers-of-a-baseline-file-and-says-its-size
  (let* ((octets (image:encode-jpeg 203 117 (test-picture 203 117)))
         (picture (decode-jpeg octets)))
    (is (equal '(#xFF #xD8) (list (aref octets 0) (aref octets 1))) "it begins with SOI")
    (is (equal '(#xFF #xD9) (list (aref octets (- (length octets) 2)) (aref octets (1- (length octets)))))
        "and ends with EOI")
    (is (equal '(#xE0 #xDB #xDB #xC0 #xC4 #xC4 #xC4 #xC4 #xDA) (jpeg-picture-segments picture))
        "a JFIF header, two quantization tables, the frame, four Huffman tables, the scan")
    (is (= 203 (jpeg-picture-width picture)))
    (is (= 117 (jpeg-picture-height picture)))
    (is (search (map 'vector #'char-code "JFIF") octets) "it says what it is")))

(test the-bytes-of-a-jpeg-are-an-array-of-bytes
  (is (typep (image:encode-jpeg 8 8 (test-picture 8 8)) '(simple-array (unsigned-byte 8) (*)))))

;;; What comes back

(test a-decoder-gets-the-picture-back-at-every-size
  (dolist (size '((1 1) (7 9) (8 8) (9 8) (16 16) (31 5) (203 117) (320 200)))
    (destructuring-bind (width height) size
      (let* ((original (test-picture width height))
             (picture (decode-jpeg (image:encode-jpeg width height original))))
        (is (= width (jpeg-picture-width picture)))
        (is (= height (jpeg-picture-height picture)))
        (is (> (psnr width height original (jpeg-picture-pixels picture)) 30d0) "~dx~d" width height)))))

(test a-flat-picture-comes-back-as-it-was
  (dolist (color '((0 0 0) (255 255 255) (20 22 30) (200 30 90)))
    (let ((rgb (make-array (* 3 40 40) :element-type '(unsigned-byte 8))))
      (dotimes (index (* 40 40))
        (replace rgb color :start1 (* 3 index)))
      (let ((picture (decode-jpeg (image:encode-jpeg 40 40 rgb))))
        (is (> (psnr 40 40 rgb (jpeg-picture-pixels picture)) 40d0) "~s" color)))))

(test the-quality-trades-the-size-of-the-file-for-the-fidelity-of-the-picture
  (let* ((original (test-picture 203 117))
         (low (image:encode-jpeg 203 117 original :quality 10))
         (middle (image:encode-jpeg 203 117 original :quality 60))
         (high (image:encode-jpeg 203 117 original :quality 95))
         (best (image:encode-jpeg 203 117 original :quality 100)))
    (is (< (length low) (length middle) (length high) (length best)))
    (flet ((fidelity (octets)
             (psnr 203 117 original (jpeg-picture-pixels (decode-jpeg octets)))))
      (is (< (fidelity low) (fidelity middle) (fidelity high) (fidelity best)))
      (is (> (fidelity best) 40d0)))))

(test a-quality-out-of-range-is-taken-to-the-nearest
  (let ((original (test-picture 40 40)))
    (is (equalp (image:encode-jpeg 40 40 original :quality 1) (image:encode-jpeg 40 40 original :quality -5)))
    (is (equalp (image:encode-jpeg 40 40 original :quality 100) (image:encode-jpeg 40 40 original :quality 500)))))

(test noise-has-every-kind-of-symbol-and-survives-the-bytes-of-all-ones
  (dolist (seed '(1 2 3))
    (let* ((original (noise-picture 64 48 seed))
           (octets (image:encode-jpeg 64 48 original :quality 95))
           (picture (decode-jpeg octets)))
      (is (> (psnr 64 48 original (jpeg-picture-pixels picture)) 20d0) "seed ~d" seed)
      (is-true (search #(#xFF #x00) octets) "a byte of all ones was written with its zero after it"))))

(test a-picture-with-long-runs-of-zeros-uses-the-sixteen-zero-symbol
  (let* ((rgb (make-array (* 3 64 8) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (x 64)
      (setf (aref rgb (* 3 (+ (* 3 64) x))) (if (evenp x) 255 0)))
    (let ((picture (decode-jpeg (image:encode-jpeg 64 8 rgb :quality 100))))
      (is (> (psnr 64 8 rgb (jpeg-picture-pixels picture)) 25d0)))))

(test a-picture-that-does-not-fit-its-size-is-refused
  (signals error (image:encode-jpeg 0 10 (make-array 0 :element-type '(unsigned-byte 8))))
  (signals error (image:encode-jpeg 10 10 (make-array 299 :element-type '(unsigned-byte 8))))
  (signals error (image:encode-jpeg 70000 1 (make-array 210000 :element-type '(unsigned-byte 8))))
  (signals error (image:encode-jpeg 2 2 (make-array 12 :element-type '(unsigned-byte 16)))))

;;; The Huffman tables

(defun frequencies-of (&rest counts)
  "Return a vector of 256 frequencies with COUNTS from symbol 0 on."
  (let ((frequencies (make-array 256 :initial-element 0)))
    (loop for count in counts
          for symbol from 0
          do (setf (aref frequencies symbol) count))
    frequencies))

(defun check-huffman (table)
  "Check that a Huffman table is one a decoder can read: the codes are prefix free, none is longer than 16
bits, none is all ones, and every symbol has one."
  (let ((used '()))
    (dolist (symbol (image::huffman-symbols table))
      (let ((size (aref (image::huffman-sizes table) symbol))
            (code (aref (image::huffman-codes table) symbol)))
        (is (<= 1 size 16))
        (is (/= code (1- (ash 1 size))) "the code of all ones is not used")
        (push (cons size code) used)))
    (is (= (length used) (reduce #'+ (image::huffman-counts table))))
    (dolist (a used)
      (dolist (b used)
        (unless (eq a b)
          (when (<= (car a) (car b))
            (is (/= (cdr a) (ash (cdr b) (- (- (car b) (car a))))) "no code begins another")))))))

(test huffman-tables-are-prefix-free-short-and-never-all-ones
  (dolist (frequencies (list (frequencies-of 5)
                             (frequencies-of 100 1)
                             (frequencies-of 1 1 1 1 1 1 1 1 1 1 1 1)
                             (frequencies-of 1000 500 250 125 60 30 15 8 4 2 1)
                             (apply #'frequencies-of (loop for index below 256 collect (1+ (* index index))))
                             (apply #'frequencies-of
                                    (loop for index below 256 collect (expt 2 (min 40 (floor index 7)))))))
    (check-huffman (image::build-huffman frequencies))))

(test the-commonest-symbol-gets-the-shortest-code
  (let ((table (image::build-huffman (frequencies-of 1000 10 10 10 1))))
    (is (= 1 (aref (image::huffman-sizes table) 0)))
    (is (> (aref (image::huffman-sizes table) 4) (aref (image::huffman-sizes table) 0)))))

;;; Exporting what the canvas shows

(defun image-app (directory
                  &key (width 64)
                       (height 48))
  "Return an application, with a drawing that has a file in DIRECTORY, that takes pictures of WIDTH x HEIGHT.

DIRECTORY: a folder made for the test.
WIDTH: width of the pictures.
HEIGHT: height of the pictures."
  (let ((application (dialog-app directory))
        (pixels (test-picture width height)))
    (setf (app:app-image-capturer application)
          (lambda ()
            (values pixels width height)))
    application))

(defun jpeg-file-octets (path)
  "Return the bytes of the file at PATH."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(test exporting-an-image-writes-a-jpeg-of-what-the-canvas-shows-and-says-so-in-a-modal
  (with-temporary-directory (directory)
    (let ((application (image-app directory)))
      (app:operation-export-image application)
      (is (string= "Export as a JPEG image" (dialog-title-text application)))
      (is (string= "current.jpg" (ui:edit-state-text (app:file-dialog-name-edit (app:app-dialog application))))
          "it offers the name of the drawing")
      (press application :enter)
      (is (null (app:app-dialog application)))
      (let ((path (merge-pathnames "current.jpg" directory)))
        (is (probe-file path))
        (let ((picture (decode-jpeg (jpeg-file-octets path))))
          (is (= 64 (jpeg-picture-width picture)))
          (is (= 48 (jpeg-picture-height picture)))
          (is (> (psnr 64 48 (test-picture 64 48) (jpeg-picture-pixels picture)) 30d0)
              "it is the picture that was taken")))
      (is (string= "Image exported" (prompt-title-of application)))
      (is (search "current.jpg was saved" (app:prompt-message (app:app-prompt application))))
      (is (search "64 x 48 pixels" (app:prompt-message (app:app-prompt application))))
      (is (equal '("OK") (prompt-labels application)))
      (press application :enter)
      (is (null (app:app-prompt application))))))

(test a-picture-does-not-change-the-drawing-or-the-file-it-came-from
  (with-temporary-directory (directory)
    (let* ((application (image-app directory))
           (revision (dom:drawing-revision (app:app-drawing application)))
           (path (app:app-file-path application)))
      (app:export-image-from-app application (uiop:native-namestring (merge-pathnames "shot.jpg" directory)))
      (is (= revision (dom:drawing-revision (app:app-drawing application))))
      (is (equal path (app:app-file-path application)))
      (is (null (app:app-dirty-p application))))))

(test the-name-of-a-picture-gets-the-extension-unless-it-has-one-of-a-jpeg
  (with-temporary-directory (directory)
    (let ((application (image-app directory)))
      (dolist (name '("plain" "kept.jpg" "kept-too.JPEG" "v1.2 final"))
        (app:export-image-from-app application (uiop:native-namestring (merge-pathnames name directory)))
        (press application :enter))
      (is (probe-file (merge-pathnames "plain.jpg" directory)))
      (is (probe-file (merge-pathnames "kept.jpg" directory)))
      (is (probe-file (merge-pathnames "kept-too.JPEG" directory)))
      (is (probe-file (merge-pathnames "v1.2 final.jpg" directory)) "a dot that is not an extension"))))

(test a-window-that-cannot-make-pictures-says-so-in-a-modal
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory)))
      (is (null (app:app-image-capturer application)))
      (app:operation-export-image application)
      (is (null (app:export-image-from-app application (uiop:native-namestring (merge-pathnames "x.jpg" directory)))))
      (is (string= "Could not export" (prompt-title-of application)))
      (is (search "cannot make pictures" (app:prompt-message (app:app-prompt application))))
      (is (null (probe-file (merge-pathnames "x.jpg" directory))))
      (press application :enter)
      (is-true (app:app-dialog application) "the dialog stays"))))

(test a-canvas-with-no-picture-and-a-folder-that-is-not-there-are-refused-in-a-modal
  (with-temporary-directory (directory)
    (let ((application (image-app directory)))
      (setf (app:app-image-capturer application) (lambda () nil))
      (is (null (app:export-image-from-app application (uiop:native-namestring (merge-pathnames "x.jpg" directory)))))
      (is (search "nothing on the canvas" (app:prompt-message (app:app-prompt application))))
      (press application :enter))
    (let ((application (image-app directory)))
      (is (null (app:export-image-from-app application
                                           (uiop:native-namestring (merge-pathnames "no-such-folder/x.jpg"
                                                                                    directory)))))
      (is (string= "Could not export" (prompt-title-of application)))
      (is (search "x.jpg" (app:prompt-message (app:app-prompt application)))))))

(test an-existing-picture-is-asked-about-before-it-is-replaced
  (with-temporary-directory (directory)
    (touch-text (merge-pathnames "plan.jpg" directory) "old")
    (let ((application (image-app directory)))
      (app:operation-export-image application)
      (type-text application "plan")
      (press application :enter)
      (is (string= "Replace file?" (prompt-title-of application)))
      (choose-in-prompt application 1)
      (is (equal "old" (fixture-like-text (merge-pathnames "plan.jpg" directory))) "cancelled, untouched"))))

(test the-more-menu-offers-the-picture-and-it-speaks-portuguese
  (let ((application (make-test-app)))
    (click-widget application :more)
    (is-true (find-widget application (list :menu-item :export-image))))
  (with-temporary-directory (directory)
    (let ((application (image-app directory)))
      (i18n:with-language (:pt-br)
        (app:operation-export-image application)
        (is (string= (format nil "Exportar como imagem JPEG") (dialog-title-text application)))
        (app:close-file-dialog application)
        (app:export-image-from-app application (uiop:native-namestring (merge-pathnames "p.jpg" directory)))
        (is (string= "Imagem exportada" (prompt-title-of application)))
        (is (search "foi salvo" (app:prompt-message (app:app-prompt application))))))))
