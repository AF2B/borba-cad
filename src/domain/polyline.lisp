(in-package #:borba-cad/domain)

(defstruct (polyline-entity (:include entity)
                            (:conc-name polyline-)
                            (:constructor %make-polyline (id
                                                          layer-id
                                                          bounds
                                                          coordinates
                                                          closed))
                            (:copier nil))
  "A chain of straight segments, optionally closed.

COORDINATES: flat array x0 y0 x1 y1 ...; owned by the entity, never mutate it.
CLOSED: when true a segment joins the last vertex back to the first."
  (coordinates (make-array 0 :element-type 'double-float)
   :type (simple-array double-float (*))
   :read-only t)
  (closed nil :type boolean :read-only t))

(defun copy-validated-coordinates (coordinates)
  "Return a fresh double-float array holding COORDINATES, signaling INVALID-ENTITY when the
sequence is not made of x/y pairs of in-range doubles.

COORDINATES: sequence of numbers x0 y0 x1 y1 ..."
  (unless (typep coordinates 'sequence)
    (invalid-entity-failure :polyline
                            "its coordinates must be a sequence, got ~s"
                            coordinates))
  (when (oddp (length coordinates))
    (invalid-entity-failure :polyline "its coordinates must come in x/y pairs"))
  (let ((copy (make-array (length coordinates) :element-type 'double-float))
        (index 0))
    (map nil
         (lambda (value)
           (require-coordinate :polyline
                               (format nil "coordinate ~d" index)
                               value)
           (setf (aref copy index) value)
           (incf index))
         coordinates)
    copy))

(defun make-polyline (&key id
                           layer-id
                           coordinates
                           closed)
  "Create a validated polyline. Signals INVALID-ENTITY when an invariant is violated: at least
two vertices (three when closed), at most +MAXIMUM-POLYLINE-VERTICES+, every vertex in range,
and no two consecutive vertices coincident (including last and first when closed).

ID: positive entity id.
LAYER-ID: id of the layer.
COORDINATES: sequence of doubles x0 y0 x1 y1 ...; copied.
CLOSED: true for a closed polyline."
  (require-identifiers :polyline
                       id
                       layer-id)
  (let* ((copy (copy-validated-coordinates coordinates))
         (count (geo:polyline-vertex-count copy))
         (minimum (if closed 3 2)))
    (when (< count minimum)
      (invalid-entity-failure :polyline
                              "it has ~d vertices; a ~:[open~;closed~] polyline needs at least ~d"
                              count
                              closed
                              minimum))
    (when (> count +maximum-polyline-vertices+)
      (invalid-entity-failure :polyline
                              "it has ~d vertices; the limit is ~d"
                              count
                              +maximum-polyline-vertices+))
    (flet ((require-distinct (from
                              to)
             (unless (base:positive-length-p
                      (geo:distance (aref copy (* 2 from))
                                    (aref copy (1+ (* 2 from)))
                                    (aref copy (* 2 to))
                                    (aref copy (1+ (* 2 to)))))
               (invalid-entity-failure :polyline
                                       "vertices ~d and ~d coincide; consecutive vertices must be at least ~f mm apart"
                                       from
                                       to
                                       base:+length-tolerance+))))
      (loop for index from 0 below (1- count)
            do (require-distinct index (1+ index)))
      (when closed
        (require-distinct (1- count) 0)))
    (%make-polyline id
                    layer-id
                    (geo:polyline-bounds copy)
                    copy
                    (and closed t))))

(defun update-polyline (polyline
                        &key (layer-id (entity-layer-id polyline))
                             (coordinates (polyline-coordinates polyline))
                             (closed (polyline-closed polyline)))
  "Return a validated copy of POLYLINE with the given fields replaced.

POLYLINE: the polyline to copy.
LAYER-ID: replacement layer id.
COORDINATES: replacement coordinates (copied).
CLOSED: replacement closed flag."
  (make-polyline :id (entity-id polyline)
                 :layer-id layer-id
                 :coordinates coordinates
                 :closed closed))

(defmethod entity-kind ((polyline polyline-entity))
  :polyline)

(defmethod entity-pick-distance ((polyline polyline-entity)
                                 x
                                 y)
  (geo:point-polyline-distance x
                               y
                               (polyline-coordinates polyline)
                               (polyline-closed polyline)))

(defmethod entity-translated ((polyline polyline-entity)
                              dx
                              dy)
  (let ((moved (copy-seq (polyline-coordinates polyline))))
    (loop for index from 0 below (length moved) by 2
          do (incf (aref moved index) dx)
             (incf (aref moved (1+ index)) dy))
    (update-polyline polyline :coordinates moved)))

(defmethod entity-snap-points ((polyline polyline-entity))
  (let ((coordinates (polyline-coordinates polyline)))
    (loop for index from 0 below (length coordinates) by 2
          collect (list (aref coordinates index) (aref coordinates (1+ index)) :endpoint))))

(defmethod entity-properties ((polyline polyline-entity))
  (let ((coordinates (polyline-coordinates polyline))
        (editable-vertices (min +inspector-maximum-vertex-rows+
                                (geo:polyline-vertex-count (polyline-coordinates polyline)))))
    (cons (make-property :closed
                         "Closed"
                         :boolean
                         (polyline-closed polyline)
                         t)
          (loop for vertex from 0 below editable-vertices
                append (list (make-property (cons :vertex-x vertex)
                                            (format nil "Vertex ~d X" (1+ vertex))
                                            :coordinate
                                            (aref coordinates (* 2 vertex))
                                            t)
                             (make-property (cons :vertex-y vertex)
                                            (format nil "Vertex ~d Y" (1+ vertex))
                                            :coordinate
                                            (aref coordinates (1+ (* 2 vertex)))
                                            t))))))

(defmethod entity-derived-values ((polyline polyline-entity))
  (list (make-derived-value "Length"
                            :length
                            (geo:polyline-length (polyline-coordinates polyline)
                                                 (polyline-closed polyline)))
        (make-derived-value "Vertices"
                            :count
                            (geo:polyline-vertex-count (polyline-coordinates polyline)))))

(defmethod entity-with-property ((polyline polyline-entity)
                                 key
                                 value)
  (cond ((eq key :closed)
         (update-polyline polyline :closed value))
        ((and (consp key)
              (member (car key) '(:vertex-x :vertex-y))
              (typep (cdr key) `(integer 0 ,(1- (geo:polyline-vertex-count
                                                 (polyline-coordinates polyline))))))
         (let ((coordinates (copy-seq (polyline-coordinates polyline))))
           (setf (aref coordinates (+ (* 2 (cdr key)) (if (eq (car key) :vertex-y) 1 0)))
                 value)
           (update-polyline polyline :coordinates coordinates)))
        (t (error "Unknown polyline property ~s" key))))

(defmethod entity-with-layer ((polyline polyline-entity)
                              layer-id)
  (update-polyline polyline :layer-id layer-id))
