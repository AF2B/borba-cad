(in-package #:borba-cad/test-support)

;;;; SplitMix64 (Steele, Lea, Flood 2014). Implemented here instead of using CL:RANDOM so that
;;;; tests and benchmarks produce the same numbers on every SBCL version and machine.

(defconstant +splitmix-increment+ #x9E3779B97F4A7C15
  "SplitMix64 state increment (the golden-ratio constant).")

(defconstant +splitmix-multiplier-1+ #xBF58476D1CE4E5B9
  "First SplitMix64 finalizer multiplier.")

(defconstant +splitmix-multiplier-2+ #x94D049BB133111EB
  "Second SplitMix64 finalizer multiplier.")

(defconstant +double-mantissa-bits+ 53
  "Significant bits of a double-float; the PRNG draws exactly this many.")

(defstruct (prng (:constructor %make-prng (state)))
  "A SplitMix64 generator. Its whole state is one 64-bit integer.

STATE: current 64-bit state."
  (state 0 :type (unsigned-byte 64)))

(defun make-prng (&optional (seed 0))
  "Create a generator.

SEED: any non-negative integer below 2^64; equal seeds give equal sequences."
  (check-type seed (unsigned-byte 64))
  (%make-prng seed))

(defun prng-next-integer-64 (prng)
  "Advance PRNG and return an unsigned 64-bit integer.

PRNG: the generator."
  (let ((z (ldb (byte 64 0) (+ (prng-state prng) +splitmix-increment+))))
    (setf (prng-state prng) z)
    (setf z (ldb (byte 64 0) (* (logxor z (ash z -30)) +splitmix-multiplier-1+)))
    (setf z (ldb (byte 64 0) (* (logxor z (ash z -27)) +splitmix-multiplier-2+)))
    (logxor z (ash z -31))))

(defun prng-next-unit-double (prng)
  "Return a double-float in [0, 1) with 53 random bits.

PRNG: the generator."
  (* (float (ash (prng-next-integer-64 prng) (- (- 64 +double-mantissa-bits+))) 1d0)
     (scale-float 1d0 (- +double-mantissa-bits+))))

(defun prng-next-double (prng
                         low
                         high)
  "Return a double-float uniformly distributed in [LOW, HIGH).

PRNG: the generator.
LOW: inclusive lower bound.
HIGH: exclusive upper bound."
  (+ low (* (- high low) (prng-next-unit-double prng))))

(defun prng-next-below (prng
                        limit)
  "Return an integer in [0, LIMIT). The modulo bias is below 2^-40 for LIMIT under 2^24.

PRNG: the generator.
LIMIT: positive integer upper bound (exclusive)."
  (check-type limit (integer 1))
  (mod (prng-next-integer-64 prng) limit))

(defun prng-next-boolean (prng)
  "Return T or NIL with equal probability.

PRNG: the generator."
  (logbitp 63 (prng-next-integer-64 prng)))

(defun prng-pick (prng
                  sequence)
  "Return a uniformly chosen element of SEQUENCE.

PRNG: the generator.
SEQUENCE: a non-empty sequence."
  (elt sequence (prng-next-below prng (length sequence))))
