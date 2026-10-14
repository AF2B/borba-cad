(in-package #:borba-cad/test-support)

;;;; Deterministic generation of floor-plan-like drawings. The same seed and options always
;;;; produce the same drawing, on every machine, so benchmark workloads are reproducible.

(defconstant +workload-site-side-per-root-entity+ 3000d0
  "Side of the square site, in millimetres, per square root of the entity count. Keeps the
entity density constant as the workload grows.")

(defconstant +workload-minimum-site-side+ 20000d0
  "Smallest site side in millimetres.")

(defconstant +workload-coordinate-quantum+ 10d0
  "Generated coordinates are multiples of this many millimetres, so drawings print compactly.")

(defparameter *workload-layer-specs*
  '(("walls" 255 120 80)
    ("doors" 120 200 255)
    ("furniture" 160 220 120)
    ("annotations" 255 220 100)
    ("dimensions" 200 160 255))
  "Name and RGB color of the layers added to every generated drawing.")

(defparameter *workload-kind-weights*
  '((:line . 45) (:rectangle . 15) (:circle . 10) (:polyline . 15) (:text . 10) (:dimension . 5))
  "Share of each entity kind in a generated drawing, in percent (sums to 100).")

(defparameter *workload-words*
  (list "Kitchen" "Bedroom" "Living" "Hall" "Storage" "Bath" "Garage" "Office" "Terrace" "Stairs"
        (coerce (list #\C #\a #\f (code-char 233)) 'string)
        (coerce (list #\A (code-char 231) #\a #\o) 'string))
  "Words used for generated text entities; the last two contain non-ASCII characters.")

(defun quantize (value)
  "Round VALUE to the coordinate quantum.

VALUE: a double-float."
  (* +workload-coordinate-quantum+ (fround (/ value +workload-coordinate-quantum+))))

(defun random-coordinate (prng
                          side)
  "Return a quantized coordinate in [0, SIDE).

PRNG: the generator.
SIDE: extent of the site."
  (quantize (prng-next-double prng
                              0d0
                              side)))

(defun random-length (prng
                      low
                      high)
  "Return a quantized length in [LOW, HIGH), never below one quantum.

PRNG: the generator.
LOW: smallest length.
HIGH: largest length."
  (max +workload-coordinate-quantum+ (quantize (prng-next-double prng
                                                                 low
                                                                 high))))

(defun random-kind (prng)
  "Return an entity kind drawn according to *WORKLOAD-KIND-WEIGHTS*.

PRNG: the generator."
  (let ((roll (prng-next-below prng 100))
        (accumulated 0))
    (dolist (entry *workload-kind-weights* :line)
      (incf accumulated (cdr entry))
      (when (< roll accumulated)
        (return (car entry))))))

(defun random-entity (prng
                      kind
                      id
                      layer-id
                      side)
  "Create one valid entity of KIND.

PRNG: the generator.
KIND: entity kind keyword.
ID: entity id.
LAYER-ID: layer id.
SIDE: extent of the site in millimetres."
  (let ((x (random-coordinate prng side))
        (y (random-coordinate prng side)))
    (ecase kind
      (:line
       (let* ((length (random-length prng
                                     500d0
                                     6000d0))
              (angle (prng-next-double prng
                                       0d0
                                       (* 2d0 (float pi 1d0))))
              (x2 (quantize (+ x (* length (cos angle)))))
              (y2 (quantize (+ y (* length (sin angle))))))
         ;; Never degenerate: nudge when rounding collapsed the segment.
         (when (and (= x2 x) (= y2 y))
           (incf x2 +workload-coordinate-quantum+))
         (borba-cad/domain:make-line :id id
                                     :layer-id layer-id
                                     :x1 x
                                     :y1 y
                                     :x2 x2
                                     :y2 y2)))
      (:rectangle
       (borba-cad/domain:make-rectangle :id id
                                        :layer-id layer-id
                                        :min-x x
                                        :min-y y
                                        :width (random-length prng
                                                              500d0
                                                              8000d0)
                                        :height (random-length prng
                                                               500d0
                                                               8000d0)))
      (:circle
       (borba-cad/domain:make-circle :id id
                                     :layer-id layer-id
                                     :center-x x
                                     :center-y y
                                     :radius (random-length prng
                                                            100d0
                                                            1500d0)))
      (:polyline
       (let* ((count (+ 3 (prng-next-below prng 6)))
              (coordinates (make-array (* 2 count) :element-type 'double-float))
              (cursor-x x)
              (cursor-y y))
         (dotimes (vertex count)
           (setf (aref coordinates (* 2 vertex)) cursor-x
                 (aref coordinates (1+ (* 2 vertex))) cursor-y)
           ;; Steps of at least 300 mm along x guarantee distinct consecutive vertices.
           (incf cursor-x (random-length prng
                                         300d0
                                         3000d0))
           (incf cursor-y (quantize (prng-next-double prng
                                                      -2000d0
                                                      2000d0))))
         (borba-cad/domain:make-polyline :id id
                                         :layer-id layer-id
                                         :coordinates coordinates
                                         :closed nil)))
      (:text
       (borba-cad/domain:make-text :id id
                                   :layer-id layer-id
                                   :x x
                                   :y y
                                   :string (prng-pick prng *workload-words*)
                                   :height (prng-pick prng '(150d0 250d0 350d0))))
      (:dimension
       (let ((length (random-length prng
                                    1000d0
                                    8000d0)))
         (borba-cad/domain:make-dimension :id id
                                          :layer-id layer-id
                                          :x1 x
                                          :y1 y
                                          :x2 (+ x length)
                                          :y2 y
                                          :offset (prng-pick prng '(-900d0 -600d0 300d0 600d0))
                                          :text-height 150d0
                                          :unit (prng-pick prng '(:millimeter :meter))
                                          :precision (prng-pick prng '(0 2))))))))

(defun generate-drawing (&key (seed 1)
                              (entity-count 1000))
  "Generate a deterministic drawing: the default layer plus five named layers and
ENTITY-COUNT entities of mixed kinds scattered over a site whose size grows with the count.

SEED: PRNG seed; equal seeds give identical drawings.
ENTITY-COUNT: number of entities to create."
  (let* ((prng (make-prng seed))
         (drawing (borba-cad/domain:make-drawing))
         (side (max +workload-minimum-site-side+
                    (* +workload-site-side-per-root-entity+ (sqrt (float entity-count 1d0)))))
         (layer-ids (cons borba-cad/domain:+default-layer-id+
                          (loop for (name red green blue) in *workload-layer-specs*
                                collect (borba-cad/domain:layer-id
                                         (borba-cad/domain:add-layer
                                          drawing
                                          name
                                          (borba-cad/domain:make-rgb red
                                                                     green
                                                                     blue))))))
         (entities (loop for id from 1 to entity-count
                         collect (random-entity prng
                                                (random-kind prng)
                                                id
                                                (prng-pick prng layer-ids)
                                                side))))
    (borba-cad/domain:apply-changes drawing (borba-cad/domain:create-entities-changes entities))
    drawing))
