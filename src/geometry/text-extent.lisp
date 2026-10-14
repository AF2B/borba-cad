(in-package #:borba-cad/geometry)

;;;; Text extent model. Text in a drawing uses a fixed-advance layout so that its bounds depend
;;;; only on the character count and the text height, never on a font file. The renderer
;;;; positions every glyph on the same grid, and the bundled monospace fonts advance exactly
;;;; 0.6 em (verified for Noto Sans Mono in spike S3).

(defconstant +text-advance-ratio+ 0.6d0
  "Horizontal advance of one character, as a fraction of the text height (em size).")

(defconstant +text-ascent-ratio+ 0.95d0
  "Height of the nominal text box above the baseline, as a fraction of the text height.
Tall enough for accented capital letters.")

(defconstant +text-descent-ratio+ 0.25d0
  "Depth of the nominal text box below the baseline, as a fraction of the text height.")

(defun text-width (character-count
                   height)
  "Return the width of a text line in world units.

CHARACTER-COUNT: number of characters (one cell per code point).
HEIGHT: text height (em size) in world units."
  (declare (type double-float height))
  (* character-count +text-advance-ratio+ height))

(defun oriented-box-bounds (origin-x
                            origin-y
                            length
                            ascent
                            descent
                            angle)
  "Return the axis-aligned bounds of a rotated box.
The box spans 0..LENGTH along the direction ANGLE from the origin, and -DESCENT..ASCENT
perpendicular to it (left of the direction is positive).

ORIGIN-X: x of the box origin (baseline start).
ORIGIN-Y: y of the box origin.
LENGTH: extent along the direction.
ASCENT: extent on the left side of the direction.
DESCENT: extent on the right side of the direction.
ANGLE: direction in radians, counter-clockwise from +x."
  (declare (type double-float origin-x origin-y length ascent descent angle))
  (let* ((cosine (cos angle))
         (sine (sin angle))
         (min-x most-positive-double-float)
         (min-y most-positive-double-float)
         (max-x most-negative-double-float)
         (max-y most-negative-double-float))
    (declare (type double-float cosine sine min-x min-y max-x max-y))
    (dolist (along (list 0d0 length))
      (dolist (across (list (- descent) ascent))
        (let ((x (+ origin-x (* along cosine) (- (* across sine))))
              (y (+ origin-y (* along sine) (* across cosine))))
          (setf min-x (min min-x x)
                max-x (max max-x x)
                min-y (min min-y y)
                max-y (max max-y y)))))
    (make-bounds min-x
                 min-y
                 max-x
                 max-y)))
