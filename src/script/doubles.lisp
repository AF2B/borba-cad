(in-package #:borba-cad/script)

;;;; The functions of Common Lisp that answer in a single-float when they are given a whole number or a ratio,
;;;; which have 7 digits: on a drawing 10 km across, a millimetre. In the user package these are the versions
;;;; that answer in a double-float, whatever the argument, so that (sqrt 2) is as exact as (sqrt 2.0). The
;;;; originals are still there as cl:sqrt and the rest.

(defun as-double (number)
  "Return a real number as a double-float; anything else is left for the function that follows to refuse.

NUMBER: a number."
  (if (realp number)
      (cl:float number 1d0)
      number))

(defun doubles:sqrt (number)
  "Return the square root of NUMBER, in double-float for a real number.

NUMBER: a number."
  (cl:sqrt (as-double number)))

(defun doubles:sin (radians)
  "Return the sine of an angle given in radians, in double-float.

RADIANS: a number."
  (cl:sin (as-double radians)))

(defun doubles:cos (radians)
  "Return the cosine of an angle given in radians, in double-float.

RADIANS: a number."
  (cl:cos (as-double radians)))

(defun doubles:tan (radians)
  "Return the tangent of an angle given in radians, in double-float.

RADIANS: a number."
  (cl:tan (as-double radians)))

(defun doubles:asin (number)
  "Return the arc sine of NUMBER, in radians and double-float.

NUMBER: a number."
  (cl:asin (as-double number)))

(defun doubles:acos (number)
  "Return the arc cosine of NUMBER, in radians and double-float.

NUMBER: a number."
  (cl:acos (as-double number)))

(defun doubles:atan (number
                     &optional (other nil other-p))
  "Return the arc tangent of NUMBER, or of NUMBER over OTHER when it is given, in radians and double-float.

NUMBER: a number, or the y of a direction.
OTHER: the x of a direction."
  (if other-p
      (cl:atan (as-double number) (as-double other))
      (cl:atan (as-double number))))

(defun doubles:exp (number)
  "Return e to the power NUMBER, in double-float.

NUMBER: a number."
  (cl:exp (as-double number)))

(defun doubles:log (number
                    &optional (base nil base-p))
  "Return the logarithm of NUMBER, natural or to BASE when it is given, in double-float.

NUMBER: a number.
BASE: the base of the logarithm."
  (if base-p
      (cl:log (as-double number) (as-double base))
      (cl:log (as-double number))))

(defun doubles:float (number
                      &optional (prototype 1d0))
  "Return NUMBER as a floating point number: a double-float unless PROTOTYPE is a float of another format.

NUMBER: a real number.
PROTOTYPE: a float whose format the result has; double-float when it is left out."
  (cl:float number prototype))
