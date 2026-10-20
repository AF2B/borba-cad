(in-package #:borba-cad/image)

;;;; The pictures inside a PDF page (ISO 32000-1, section 8.9): color spaces, the samples of an image stored raw or as
;;;; a JPEG, stencil masks and soft masks, and drawing an image into a page under the transform it is placed with.
;;;; Pictures kept as CCITT fax, JBIG2 or JPEG 2000 are named and refused.

(defstruct (color-space (:conc-name space-))
  "A color space reduced to what drawing needs: how its components make RGB."
  (kind :gray :type keyword)
  (components 1 :type fixnum)
  (palette nil)
  (high 0 :type fixnum))

(defstruct (pdf-image (:conc-name image-))
  "An image of a page decoded: its pixels, and its soft mask as a raster of grays when it has one."
  (raster nil)
  (mask nil))

(defparameter *device-spaces*
  '(("DeviceGray" . (:gray 1)) ("G" . (:gray 1)) ("CalGray" . (:gray 1))
    ("DeviceRGB" . (:rgb 3)) ("RGB" . (:rgb 3)) ("CalRGB" . (:rgb 3))
    ("DeviceCMYK" . (:cmyk 4)) ("CMYK" . (:cmyk 4))
    ("Pattern" . (:pattern 0)))
  "The color spaces that have a name and need no more: (name kind components).")

(defun space-from-count (count)
  "Return the device color space that has COUNT components.

COUNT: 1, 3 or 4."
  (case count
    (1 (make-color-space :kind :gray :components 1))
    (4 (make-color-space :kind :cmyk :components 4))
    (t (make-color-space :kind :rgb :components 3))))

(defun cmyk-to-rgb (cyan
                    magenta
                    yellow
                    black)
  "Return the red, green and blue, from 0 to 255, that inks of those amounts, each from 0 to 255, make.

CYAN: the cyan.
MAGENTA: the magenta.
YELLOW: the yellow.
BLACK: the black."
  (flet ((channel (ink)
           (floor (* (- 255 ink) (- 255 black)) 255)))
    (values (channel cyan) (channel magenta) (channel yellow))))

(defun parse-color-space (document
                          value
                          resources
                          &optional (depth 0))
  "Return the color space that VALUE names or defines.

DOCUMENT: the document.
VALUE: a name, an array, or a reference to one.
RESOURCES: the resources dictionary of the page, where names that are not of a device space are looked up.
DEPTH: how many color spaces enclose this one."
  (when (> depth 6)
    (pdf-damaged))
  (let ((value (pdf-resolve document value)))
    (cond ((stringp value)
           (let ((device (cdr (assoc value *device-spaces* :test #'string=))))
             (cond (device
                    (make-color-space :kind (first device) :components (second device)))
                   ((and resources (pdf-get document
                                            (pdf-get document
                                                     resources
                                                     "ColorSpace")
                                            value))
                    (parse-color-space document
                                       (pdf-get document
                                                (pdf-get document
                                                         resources
                                                         "ColorSpace")
                                                value)
                                       resources
                                       (1+ depth)))
                   (t
                    (make-color-space)))))
          ((and (vectorp value) (plusp (length value)))
           (let ((head (pdf-resolve document (aref value 0))))
             (cond ((not (stringp head))
                    (make-color-space))
                   ((string= head "ICCBased")
                    (let* ((object (and (> (length value) 1) (pdf-object-of document (aref value 1))))
                           (count (and object (pdf-get document
                                                       (object-value object)
                                                       "N"))))
                      (space-from-count (if (integerp count) count 3))))
                   ((assoc head *device-spaces* :test #'string=)
                    (parse-color-space document
                                       head
                                       resources
                                       (1+ depth)))
                   ((string= head "Lab")
                    (make-color-space :kind :lab :components 3))
                   ((member head '("Indexed" "I") :test #'string=)
                    (parse-indexed-space document
                                         value
                                         resources
                                         depth))
                   ((string= head "Separation")
                    (make-color-space :kind :tint :components 1))
                   ((string= head "DeviceN")
                    (let ((names (and (> (length value) 1) (pdf-resolve document (aref value 1)))))
                      (make-color-space :kind :tint :components (if (vectorp names) (max 1 (length names)) 1))))
                   (t
                    (make-color-space)))))
          (t
           (make-color-space)))))

(defun parse-indexed-space (document
                            value
                            resources
                            depth)
  "Return the color space of an Indexed array, with its palette made into RGB.

DOCUMENT: the document.
VALUE: the array [/Indexed base hival lookup].
RESOURCES: the resources dictionary of the page.
DEPTH: how many color spaces enclose this one."
  (unless (>= (length value) 4)
    (pdf-damaged))
  (let* ((base (parse-color-space document
                                  (aref value 1)
                                  resources
                                  (1+ depth)))
         (high (let ((number (pdf-resolve document (aref value 2))))
                 (if (integerp number) (min 255 (max 0 number)) 255)))
         (lookup (let* ((entry (aref value 3))
                        (resolved (pdf-resolve document entry))
                        (object (pdf-object-of document entry)))
                   (cond ((typep resolved 'octets) resolved)
                         (object (object-stream-bytes document object))
                         (t (make-octets 0)))))
         (components (space-components base))
         (palette (make-octets (* 3 (1+ high)))))
    (dotimes (index (1+ high))
      (let ((base-offset (* index components)))
        (flet ((component (offset)
                 (if (< (+ base-offset offset) (length lookup))
                     (aref lookup (+ base-offset offset))
                     0)))
          (multiple-value-bind (red green blue)
              (case (space-kind base)
                (:gray (values (component 0) (component 0) (component 0)))
                (:cmyk (cmyk-to-rgb (component 0)
                                    (component 1)
                                    (component 2)
                                    (component 3)))
                (:tint (let ((gray (- 255 (component 0))))
                         (values gray gray gray)))
                (t (values (component 0) (component 1) (component 2))))
            (setf (aref palette (* 3 index)) red
                  (aref palette (+ 1 (* 3 index))) green
                  (aref palette (+ 2 (* 3 index))) blue)))))
    (make-color-space :kind
                      :indexed
                      :components
                      1
                      :palette
                      palette
                      :high
                      high)))

(defun color-from-operands (space
                            operands)
  "Return the red, green and blue, from 0 to 255, of a color given by the operands of a color operator.

SPACE: the color space.
OPERANDS: the numbers, each from 0 to 1 (an index for an Indexed space)."
  (flet ((byte-of (value)
           (max 0 (min 255 (round (* 255 (if (realp value) value 0)))))))
    (let ((numbers (remove-if-not #'realp operands)))
      (ecase (space-kind space)
        (:gray (let ((gray (byte-of (or (first numbers) 0))))
                 (values gray gray gray)))
        (:rgb (values (byte-of (or (first numbers) 0))
                      (byte-of (or (second numbers) 0))
                      (byte-of (or (third numbers) 0))))
        (:cmyk (cmyk-to-rgb (byte-of (or (first numbers) 0))
                            (byte-of (or (second numbers) 0))
                            (byte-of (or (third numbers) 0))
                            (byte-of (or (fourth numbers) 0))))
        (:indexed (let* ((index (max 0 (min (space-high space) (round (or (first numbers) 0)))))
                         (palette (space-palette space)))
                    (values (aref palette (* 3 index))
                            (aref palette (+ 1 (* 3 index)))
                            (aref palette (+ 2 (* 3 index))))))
        (:tint (let ((gray (- 255 (byte-of (if numbers (reduce #'max numbers) 0)))))
                 (values gray gray gray)))
        (:lab (let ((gray (max 0 (min 255 (round (* 255 (/ (or (first numbers) 0) 100)))))))
                (values gray gray gray)))
        (:pattern (values 0 0 0))))))

;;; Reading the samples

(defun unsupported-picture-filter (filter)
  "Signal IMAGE-ERROR for a picture of a PDF stored in a way that is not read.

FILTER: the name of the filter."
  (if (string= filter "JPXDecode")
      (image-failure "This PDF keeps a picture as JPEG 2000, which the program does not read.")
      (image-failure "This PDF keeps a picture as a scanned fax (CCITT or JBIG2), which the program does not read.")))

(defun sample-lookup (bits
                      low
                      high)
  "Return a table that turns a raw sample of BITS bits into a byte, mapping the range of raw values onto LOW to HIGH,
as the Decode array of an image does.

BITS: bits per sample.
LOW: the value the lowest raw sample stands for, from 0 to 1.
HIGH: the value the highest raw sample stands for."
  (let* ((top (1- (ash 1 bits)))
         (table (make-octets (1+ top))))
    (dotimes (raw (1+ top) table)
      (setf (aref table raw)
            (max 0 (min 255 (round (* 255 (+ low (* (/ raw top) (- high low)))))))))))

(defun decode-array-of (document
                        dictionary
                        count
                        indexed)
  "Return the Decode array of an image as a list of COUNT (low high) pairs, with the default for a missing one.

DOCUMENT: the document.
DICTIONARY: the dictionary of the image.
COUNT: the number of components.
INDEXED: true for an Indexed image, whose raw samples are indexes and not scaled."
  (let ((decode (pdf-get document
                         dictionary
                         "Decode")))
    (loop for index below count
          collect (if (and (vectorp decode) (>= (length decode) (* 2 (1+ index))) (not indexed))
                      (flet ((bound (position default)
                               (let ((value (pdf-resolve document (aref decode position))))
                                 (if (realp value) value default))))
                        (list (bound (* 2 index) 0)
                              (bound (1+ (* 2 index)) 1)))
                      (list 0 1)))))

(defun image-dimension (document
                        dictionary
                        key)
  "Return the width or the height of an image, or signal IMAGE-ERROR when it is missing or unreasonable.

DOCUMENT: the document.
DICTIONARY: the dictionary of the image.
KEY: \"Width\" or \"Height\"."
  (let ((value (pdf-get document
                        dictionary
                        key)))
    (unless (and (integerp value) (plusp value))
      (pdf-damaged))
    value))

(defun raw-image-raster (document
                         dictionary
                         bytes
                         width
                         height
                         resources
                         fill)
  "Return the raster of an image whose samples are stored raw (after the filters that code bytes).

DOCUMENT: the document.
DICTIONARY: the dictionary of the image.
BYTES: the decoded samples.
WIDTH: width in pixels.
HEIGHT: height in pixels.
RESOURCES: the resources dictionary of the page.
FILL: the color, as (red green blue), that a stencil mask paints with."
  (check-picture-size width height)
  (let* ((stencil (eq (pdf-get document
                               dictionary
                               "ImageMask") :true))
         (bits (if stencil 1 (let ((value (pdf-get document
                                                   dictionary
                                                   "BitsPerComponent")))
                               (if (member value '(1 2 4 8 16)) value 8))))
         (space (if stencil
                    (make-color-space)
                    (parse-color-space document
                                       (or (gethash "ColorSpace" dictionary) (gethash "CS" dictionary))
                                       resources)))
         (count (max 1 (space-components space)))
         (indexed (eq (space-kind space) :indexed))
         (row-bytes (ceiling (* width count bits) 8))
         (raster (make-raster width height))
         (pixels (raster-pixels raster))
         (decode (decode-array-of document
                                  dictionary
                                  count
                                  (or indexed stencil)))
         (tables (mapcar (lambda (pair) (sample-lookup (min bits 8)
                                                       (first pair)
                                                       (second pair))) decode))
         (stencil-paints (if (and stencil (vectorp (pdf-get document
                                                            dictionary
                                                            "Decode"))
                                  (let ((first (pdf-resolve document (aref (pdf-get document
                                                                                    dictionary
                                                                                    "Decode") 0))))
                                    (and (realp first) (>= first 1))))
                             1
                             0)))
    (when (< (length bytes) (* height row-bytes))
      (image-failure "This picture file ends before the picture does."))
    (dotimes (row height)
      (let ((start (* row row-bytes))
            (out (* row width +raster-channels+)))
        (dotimes (column width)
          (flet ((component (index)
                   (let ((raw (sample-at bytes
                                         start
                                         bits
                                         (+ index (* column count)))))
                     (if (> bits 8)
                         (aref (nth index tables) (ash raw -8))
                         (aref (nth index tables) raw)))))
            (multiple-value-bind (red green blue alpha)
                (cond (stencil
                       (values (first fill) (second fill) (third fill)
                               (if (= (sample-at bytes
                                                 start
                                                 1
                                                 column) stencil-paints) +opaque+ 0)))
                      (indexed
                       (let* ((index (min (space-high space) (sample-at bytes
                                                                        start
                                                                        bits
                                                                        column)))
                              (palette (space-palette space)))
                         (values (aref palette (* 3 index))
                                 (aref palette (+ 1 (* 3 index)))
                                 (aref palette (+ 2 (* 3 index)))
                                 +opaque+)))
                      (t
                       (case (space-kind space)
                         (:gray (let ((gray (component 0)))
                                  (values gray gray gray +opaque+)))
                         (:cmyk (multiple-value-bind (red green blue)
                                    (cmyk-to-rgb (component 0)
                                                 (component 1)
                                                 (component 2)
                                                 (component 3))
                                  (values red green blue +opaque+)))
                         (:tint (let ((gray (- 255 (loop for index below count maximize (component index)))))
                                  (values gray gray gray +opaque+)))
                         (:lab (let ((gray (component 0)))
                                 (values gray gray gray +opaque+)))
                         (t (values (component 0) (component 1) (component 2) +opaque+)))))
              (setf (aref pixels out) red
                    (aref pixels (+ out 1)) green
                    (aref pixels (+ out 2)) blue
                    (aref pixels (+ out 3)) alpha)
              (incf out +raster-channels+))))))
    raster))

(defun decode-pdf-image (document
                         object
                         resources
                         fill)
  "Return the PDF-IMAGE of an image XObject. Signals IMAGE-ERROR when the picture is kept in a way the program does not
read.

DOCUMENT: the document.
OBJECT: the indirect object of the image.
RESOURCES: the resources dictionary of the page it is drawn in.
FILL: the color, as (red green blue), that a stencil mask paints with."
  (let* ((dictionary (object-value object))
         (width (image-dimension document
                                 dictionary
                                 "Width"))
         (height (image-dimension document
                                  dictionary
                                  "Height")))
    (multiple-value-bind (bytes filter)
        (object-stream-bytes document object)
      (let ((raster (cond ((null bytes)
                           (pdf-damaged))
                          ((equal filter "DCTDecode")
                           (read-jpeg bytes))
                          (filter
                           (unsupported-picture-filter filter))
                          (t
                           (raw-image-raster document
                                             dictionary
                                             bytes
                                             width
                                             height
                                             resources
                                             fill))))
            (mask-object (pdf-object-of document (gethash "SMask" dictionary))))
        (make-pdf-image :raster raster
                        :mask (and mask-object
                                   (handler-case (image-raster (decode-pdf-image document
                                                                                 mask-object
                                                                                 resources
                                                                                 fill))
                                     (base:image-error ()
                                       nil))))))))

;;; Drawing an image

(defun reduce-raster (raster
                      factor)
  "Return a smaller raster in which each pixel is the mean of FACTOR by FACTOR pixels of RASTER.

RASTER: the raster.
FACTOR: how many pixels to take in each direction, at least 2."
  (let* ((width (raster-width raster))
         (height (raster-height raster))
         (new-width (ceiling width factor))
         (new-height (ceiling height factor))
         (reduced (make-raster new-width new-height))
         (source (raster-pixels raster))
         (target (raster-pixels reduced)))
    (declare (type octets source target)
             (type fixnum width height new-width new-height factor)
             (optimize (speed 3) (safety 1))
             (sb-ext:muffle-conditions sb-ext:compiler-note))
    (dotimes (row new-height)
      (dotimes (column new-width)
        (let ((sums (make-array 4 :element-type 'fixnum :initial-element 0))
              (taken 0))
          (declare (type fixnum taken))
          (loop for y of-type fixnum from (* row factor) below (min height (* (1+ row) factor))
                do (loop for x of-type fixnum from (* column factor) below (min width (* (1+ column) factor))
                         do (let ((at (* +raster-channels+ (+ x (* y width)))))
                              (dotimes (channel 4)
                                (incf (aref sums channel) (aref source (+ at channel))))
                              (incf taken))))
          (let ((out (* +raster-channels+ (+ column (* row new-width)))))
            (dotimes (channel 4)
              (setf (aref target (+ out channel)) (floor (aref sums channel) taken)))))))
    reduced))

(defun sample-raster (raster
                      column
                      row
                      result)
  "Put the four channels of a raster at a fractional position into RESULT, mixing the four pixels around the position.

RASTER: the raster.
COLUMN: the position along the width, in pixels, where 0.5 is the middle of the first pixel.
ROW: the position along the height.
RESULT: a vector of four doubles that receives the red, green, blue and opacity."
  (declare (type raster raster)
           (type double-float column row)
           (type (simple-array double-float (4)) result)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((width (raster-width raster))
         (height (raster-height raster))
         (pixels (raster-pixels raster))
         (x (- column 0.5d0))
         (y (- row 0.5d0))
         (x0 (the fixnum (floor x)))
         (y0 (the fixnum (floor y)))
         (fx (- x x0))
         (fy (- y y0))
         (left (max 0 (min (1- width) x0)))
         (right (max 0 (min (1- width) (1+ x0))))
         (top (max 0 (min (1- height) y0)))
         (bottom (max 0 (min (1- height) (1+ y0))))
         (top-left (* +raster-channels+ (+ left (* width top))))
         (top-right (* +raster-channels+ (+ right (* width top))))
         (bottom-left (* +raster-channels+ (+ left (* width bottom))))
         (bottom-right (* +raster-channels+ (+ right (* width bottom)))))
    (declare (type fixnum width height x0 y0 left right top bottom top-left top-right bottom-left bottom-right)
             (type double-float x y fx fy)
             (type octets pixels))
    (dotimes (channel 4)
      (setf (aref result channel)
            (+ (* (- 1 fy) (+ (* (- 1 fx) (aref pixels (+ top-left channel)))
                              (* fx (aref pixels (+ top-right channel)))))
               (* fy (+ (* (- 1 fx) (aref pixels (+ bottom-left channel)))
                        (* fx (aref pixels (+ bottom-right channel))))))))
    nil))

(defun draw-image (target
                   image
                   matrix
                   alpha
                   clip)
  "Draw an image into a raster. The image fills the unit square, which the matrix carries to the raster.

TARGET: the raster drawn into.
IMAGE: the PDF-IMAGE.
MATRIX: the six numbers (a b c d e f) that carry the unit square to the raster.
ALPHA: the opacity of the whole image, 0 to 1.
CLIP: the FILL-CLIP the drawing may not leave."
  (destructuring-bind (a b c d e f)
      (coerce matrix 'list)
    (declare (type double-float a b c d e f))
    (let ((determinant (- (* a d) (* b c))))
      (when (< (abs determinant) 1d-12)
        (return-from draw-image nil))
      (let* ((original (image-raster image))
             (mask (image-mask image))
             (across (sqrt (+ (* a a) (* b b))))
             (down (sqrt (+ (* c c) (* d d))))
             (factor (floor (max (/ (raster-width original) (max across 1d-6))
                                 (/ (raster-height original) (max down 1d-6)))))
             (source (if (>= factor 2) (reduce-raster original factor) original))
             (corners (list (cons e f) (cons (+ a e) (+ b f)) (cons (+ c e) (+ d f)) (cons (+ a c e) (+ b d f))))
             (x-low (max (clip-x0 clip) 0 (floor (reduce #'min corners :key #'car))))
             (x-high (min (clip-x1 clip) (raster-width target) (ceiling (reduce #'max corners :key #'car))))
             (y-low (max (clip-y0 clip) 0 (floor (reduce #'min corners :key #'cdr))))
             (y-high (min (clip-y1 clip) (raster-height target) (ceiling (reduce #'max corners :key #'cdr))))
             (width (raster-width source))
             (height (raster-height source))
             (pixels (raster-pixels target))
             (target-width (raster-width target))
             (color (make-array 4 :element-type 'double-float :initial-element 0d0))
             (soft (make-array 4 :element-type 'double-float :initial-element 0d0)))
        (declare (type fixnum x-low x-high y-low y-high width height target-width)
                 (type octets pixels)
                 (type double-float alpha))
        (loop for py of-type fixnum from y-low below y-high
              do (loop for px of-type fixnum from x-low below x-high
                       do (let* ((dx (- (+ px 0.5d0) e))
                                 (dy (- (+ py 0.5d0) f))
                                 (u (/ (- (* d dx) (* c dy)) determinant))
                                 (v (/ (- (* a dy) (* b dx)) determinant)))
                            (declare (type double-float dx dy u v))
                            (when (and (<= 0d0 u 1d0) (<= 0d0 v 1d0))
                              (sample-raster source
                                             (* u width)
                                             (* (- 1d0 v) height)
                                             color)
                              (let ((opacity (* alpha (/ (aref color 3) 255d0)))
                                    (at (* +raster-channels+ (+ px (* py target-width)))))
                                (declare (type double-float opacity)
                                         (type fixnum at))
                                (when mask
                                  (sample-raster mask
                                                 (* u (raster-width mask))
                                                 (* (- 1d0 v) (raster-height mask))
                                                 soft)
                                  (setf opacity (* opacity (/ (aref soft 0) 255d0))))
                                (when (> opacity 0d0)
                                  (dotimes (channel 3)
                                    (let* ((old (aref pixels (+ at channel)))
                                           (mixed (+ old (* opacity (- (aref color channel) old)))))
                                      (setf (aref pixels (+ at channel))
                                            (max 0 (min 255 (round mixed))))))))))))))
    nil))
