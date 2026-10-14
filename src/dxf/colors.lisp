(in-package #:borba-cad/dxf)

;;;; The AutoCAD color index (ACI): 256 numbered colors. A layer in a DXF file has one of them, so a
;;;; layer color is brought to the nearest of these when a drawing is exported, and an index is turned
;;;; into a color when one is imported. The first nine and the last six are fixed; the ones between
;;;; are ten shades of each of 24 hues, 15 degrees apart.

(defparameter *standard-colors*
  '((1 255 0 0) (2 255 255 0) (3 0 255 0) (4 0 255 255) (5 0 0 255) (6 255 0 255) (7 255 255 255)
    (8 65 65 65) (9 128 128 128)
    (250 51 51 51) (251 91 91 91) (252 132 132 132) (253 173 173 173) (254 214 214 214) (255 255 255 255))
  "The colors of the indexes 1 to 9 and 250 to 255, as (index red green blue).")

(defparameter *shade-values*
  '(255 204 153 127 76)
  "The brightest channel of the five levels of brightness of a hue, from the first index of the hue on, each
level in two shades: the pure one and the one washed out to half.")

(defparameter *first-hued-index* 10
  "The first index of the ramp of hues.")

(defparameter *last-hued-index* 249
  "The last index of the ramp of hues.")

(defparameter *degrees-between-hues* 15
  "How far apart the hues of the ramp are, in degrees.")

(defparameter *degrees-per-sector* 60
  "Degrees of the color wheel in which one channel stays at its largest and another one rises or falls.")

(defun hue-channels (hue
                     brightest
                     dimmest)
  "Return the red, green and blue of the color of a hue whose largest channel is BRIGHTEST and whose smallest
is DIMMEST, the one between them truncated as AutoCAD's table is.

HUE: the hue in degrees.
BRIGHTEST: the largest channel, from 0 to 255.
DIMMEST: the smallest channel."
  (let* ((sector (floor (mod hue 360) *degrees-per-sector*))
         (fraction (/ (- (mod hue 360) (* sector *degrees-per-sector*)) *degrees-per-sector*))
         (rising (+ dimmest (floor (* (- brightest dimmest) fraction))))
         (falling (+ dimmest (floor (* (- brightest dimmest) (- 1 fraction))))))
    (ecase sector
      (0 (list brightest rising dimmest))
      (1 (list falling brightest dimmest))
      (2 (list dimmest brightest rising))
      (3 (list dimmest falling brightest))
      (4 (list rising dimmest brightest))
      (5 (list brightest dimmest falling)))))

(defun build-color-table ()
  "Return the table of the 256 colors as a vector, index 0 (BYBLOCK) and 256 (BYLAYER) not in it,
whose element at an index is a list of red, green and blue, or NIL where the index has no color."
  (let ((table (make-array 256 :initial-element nil)))
    (loop for (index red green blue) in *standard-colors*
          do (setf (aref table index) (list red green blue)))
    (loop for index from *first-hued-index* to *last-hued-index*
          do (let* ((offset (- index *first-hued-index*))
                    (hue (* *degrees-between-hues* (floor offset 10)))
                    (brightest (nth (floor (mod offset 10) 2) *shade-values*))
                    (dimmest (if (evenp offset) 0 (floor brightest 2))))
               (setf (aref table index) (hue-channels hue
                                                      brightest
                                                      dimmest))))
    table))

(defparameter *color-table* (build-color-table)
  "The 256 colors of the index.")

(defun aci->rgb (index)
  "Return the color of an AutoCAD color index as an RGB of the domain, or NIL for an index that has
none (0 and 256 mean the color of the block or of the layer, and a negative one is a layer that is off,
whose color is that of its absolute value).

INDEX: an integer."
  (let* ((absolute (abs index))
         (channels (and (<= 1 absolute 255) (aref *color-table* absolute))))
    (and channels
         (dom:make-rgb (first channels)
                       (second channels)
                       (third channels)))))

(defun rgb->aci (color)
  "Return the index of the color of the AutoCAD color index that is nearest to COLOR. White stays
7, which is white on a dark background and black on a light one, as the program draws.

COLOR: an RGB of the domain."
  (let ((best 7)
        (best-distance most-positive-fixnum)
        (red (dom:rgb-red color))
        (green (dom:rgb-green color))
        (blue (dom:rgb-blue color)))
    (loop for index from 1 to 255
          do (let ((channels (aref *color-table* index)))
               (when channels
                 (let ((distance (+ (expt (- red (first channels)) 2)
                                    (expt (- green (second channels)) 2)
                                    (expt (- blue (third channels)) 2))))
                   (when (< distance best-distance)
                     (setf best index
                           best-distance distance))))))
    best))
