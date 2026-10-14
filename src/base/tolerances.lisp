(in-package #:borba-cad/base)

;;;; Geometric tolerance policy. All world lengths are double-float millimetres.
;;;;
;;;; Three numbers frame the policy:
;;;;   * +MAXIMUM-COORDINATE-MAGNITUDE+ bounds the coordinates that may exist at all.
;;;;   * +DRAWING-RESOLUTION+ is the smallest difference a drafter can mean.
;;;;   * +LENGTH-TOLERANCE+ sits strictly between floating-point noise and that resolution.
;;;; Pixel-based radii (pick, snap) are user-interface constants and live in the app module.

(defconstant +maximum-coordinate-magnitude+ 1d7
  "Largest absolute coordinate value accepted, in millimetres (10 km).
Site plans fit comfortably inside it. A double has 53 significant bits, so the spacing
between adjacent doubles at this magnitude is 2^-29 mm (about 1.9e-9 mm), which is the
worst-case representation error of any stored coordinate.")

(defconstant +drawing-resolution+ 1d-3
  "Smallest difference a drafter can mean, in millimetres (1 micrometre). It is also the
finest zoom level: one screen pixel per resolution step.")

(defconstant +length-tolerance+ 1d-6
  "Lengths at or below this value, in millimetres (1 nanometre), count as zero.
It is 500 times larger than the worst-case representation error at the maximum magnitude
(1.9e-9 mm), so rounding noise never makes a valid segment degenerate, and 1000 times smaller
than +DRAWING-RESOLUTION+, so no intentional feature is ever treated as degenerate.")

(defconstant +angle-tolerance+ 1d-9
  "Angles that differ by no more than this, in radians, count as equal. An arc must sweep more than
this and less than a whole turn by more than this; at the largest radius an arc of this sweep is
0.01 mm long, so no arc a drafter means is ever read as degenerate.")

(defconstant +maximum-length+ 3d7
  "Largest length accepted for any single length value, in millimetres. The diagonal of the
square of valid coordinates is 2.83e7 mm, so every distance between two valid points fits;
the bound also keeps every product and sum computed from lengths far from overflow.")

(declaim (inline coordinate-p))
(defun coordinate-p (value)
  "Return true when VALUE is a finite double-float within the allowed coordinate range.

VALUE: any object."
  (and (typep value 'double-float)
       (<= (abs value) +maximum-coordinate-magnitude+)))

(declaim (inline positive-length-p))
(defun positive-length-p (value)
  "Return true when VALUE is a finite double-float length above the length tolerance.

VALUE: any object."
  (and (typep value 'double-float)
       (> value +length-tolerance+)
       (<= value +maximum-length+)))
