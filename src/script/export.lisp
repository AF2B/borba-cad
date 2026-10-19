(in-package #:borba-cad/script)

;;;; A drawing as the commands that draw it. The file that results is a program in the language of the
;;;; program: run on an empty drawing it makes the drawing again, with the layers, the entities in the
;;;; same order and the same numbers, each written so that it reads back as the number it was.

(defparameter *script-header*
  (list ";;;; Borba CAD script: this drawing, as the commands that draw it."
        ";;;; Run it on an empty drawing (More > Run script) to make the drawing again."
        ";;;; Lengths are millimetres and angles are degrees, unless written (rad x).")
  "The comment lines a script begins with.")

(defparameter *degree-digits* 12
  "Decimals of a number of degrees that a script writes when that is enough to read back as the same angle.")

(defun number-text (value)
  "Return a number as a script writes it: a whole number without a decimal point, and any other as the
shortest text that reads back as the same double.

VALUE: a double-float."
  (if (and (= value (ftruncate value)) (< (abs value) 1d15))
      (format nil "~d" (round value))
      (persist:format-double value)))

(defun angle-text (radians)
  "Return an angle as a script writes it: in degrees when the degrees read back as the same angle, and as
(rad x) when they would not.

RADIANS: the angle in radians."
  (let* ((degrees (base:radians->degrees radians))
         (rounded (/ (fround (* degrees (expt 10 *degree-digits*))) (expt 10 *degree-digits*)))
         (back (base:degrees->radians rounded)))
    (if (= back radians)
        (number-text rounded)
        (format nil "(rad ~a)" (persist:format-double radians)))))

(defun string-text (string)
  "Return a string as Lisp source.

STRING: a string."
  (let ((*print-readably* nil))
    (prin1-to-string string)))

(defun color-text (color)
  "Return a layer color as a call of RGB.

COLOR: a color of the domain."
  (format nil "(rgb ~d ~d ~d)" (dom:rgb-red color) (dom:rgb-green color) (dom:rgb-blue color)))

(defun boolean-text (value)
  "Return a flag as T or NIL.

VALUE: a generalized boolean."
  (if value "t" "nil"))

(defun ring-points-text (coordinates)
  "Return the points of a polygon as a list of the (x y) texts that a script writes them as.

COORDINATES: a flat array of vertices."
  (loop for index from 0 below (length coordinates) by 2
        collect (format nil "(~a ~a)"
                        (number-text (aref coordinates index))
                        (number-text (aref coordinates (1+ index))))))

(defun entity-command (entity)
  "Return the command that draws ENTITY, as a line of source.

ENTITY: an entity."
  (etypecase entity
    (dom:line-entity
     (format nil "(line ~a ~a ~a ~a)"
             (number-text (dom:line-x1 entity)) (number-text (dom:line-y1 entity))
             (number-text (dom:line-x2 entity)) (number-text (dom:line-y2 entity))))
    (dom:rectangle-entity
     (format nil "(rect-size ~a ~a ~a ~a)"
             (number-text (dom:rectangle-min-x entity)) (number-text (dom:rectangle-min-y entity))
             (number-text (dom:rectangle-width entity)) (number-text (dom:rectangle-height entity))))
    (dom:circle-entity
     (format nil "(circle ~a ~a ~a)"
             (number-text (dom:circle-center-x entity)) (number-text (dom:circle-center-y entity))
             (number-text (dom:circle-radius entity))))
    (dom:polyline-entity
     (let ((coordinates (dom:polyline-coordinates entity)))
       (format nil "(pline '(~{~a~^ ~})~:[~; :closed t~])"
               (loop for index from 0 below (length coordinates) by 2
                     collect (format nil "(~a ~a)"
                                     (number-text (aref coordinates index))
                                     (number-text (aref coordinates (1+ index)))))
               (dom:polyline-closed entity))))
    (dom:text-entity
     (format nil "(text ~a ~a ~a :height ~a~:[~; :angle ~:*~a~])"
             (number-text (dom:text-x entity)) (number-text (dom:text-y entity))
             (string-text (dom:text-string entity)) (number-text (dom:text-height entity))
             (and (/= 0d0 (dom:text-angle entity)) (angle-text (dom:text-angle entity)))))
    (dom:dimension-entity
     (format nil "(dimension ~a ~a ~a ~a ~a :height ~a :unit ~a :decimals ~d)"
             (number-text (dom:dimension-x1 entity)) (number-text (dom:dimension-y1 entity))
             (number-text (dom:dimension-x2 entity)) (number-text (dom:dimension-y2 entity))
             (number-text (dom:dimension-offset entity)) (number-text (dom:dimension-text-height entity))
             (keyword-text (dom:dimension-unit entity)) (dom:dimension-precision entity)))
    (dom:arc-entity
     (format nil "(arc ~a ~a ~a ~a ~a)"
             (number-text (dom:arc-center-x entity)) (number-text (dom:arc-center-y entity))
             (number-text (dom:arc-radius entity))
             (angle-text (dom:arc-start-angle entity)) (angle-text (dom:arc-end-angle entity))))
    (dom:ellipse-entity
     (format nil "(ellipse ~a ~a ~a ~a ~a)"
             (number-text (dom:ellipse-center-x entity)) (number-text (dom:ellipse-center-y entity))
             (number-text (dom:ellipse-radius-x entity)) (number-text (dom:ellipse-radius-y entity))
             (angle-text (dom:ellipse-rotation entity))))
    (dom:polygon-entity
     (format nil "(polygon ~a ~a ~a ~d ~a)"
             (number-text (dom:polygon-center-x entity)) (number-text (dom:polygon-center-y entity))
             (number-text (dom:polygon-radius entity)) (dom:polygon-sides entity)
             (angle-text (dom:polygon-rotation entity))))
    (dom:insert-entity
     (format nil "(insert ~a ~a ~a :scale ~a :rotation ~a~:[~; :flipped t~])"
             (string-text (dom:block-definition-name (dom:insert-block entity)))
             (number-text (dom:insert-x entity))
             (number-text (dom:insert-y entity))
             (number-text (dom:insert-scale entity))
             (angle-text (dom:insert-rotation entity))
             (dom:insert-flipped entity)))
    (dom:hatch-entity
     (format nil "(hatch-polygon '(~{~a~^ ~}) :pattern ~a :angle ~a :spacing ~a~@[ :holes '(~{~a~^ ~})~])"
             (ring-points-text (dom:hatch-coordinates entity))
             (keyword-text (dom:hatch-pattern entity))
             (angle-text (dom:hatch-angle entity))
             (number-text (dom:hatch-spacing entity))
             (and (dom:hatch-holes entity)
                  (mapcar (lambda (ring)
                            (format nil "(~{~a~^ ~})" (ring-points-text ring)))
                          (dom:hatch-holes entity)))))
    (dom:radial-dimension-entity
     (format nil "(~:[radius~;diameter~]-dimension ~a ~a ~a :angle ~a :height ~a :unit ~a :decimals ~d)"
             (dom:radial-dimension-diameter entity)
             (number-text (dom:radial-dimension-center-x entity)) (number-text (dom:radial-dimension-center-y entity))
             (number-text (dom:radial-dimension-radius entity)) (angle-text (dom:radial-dimension-angle entity))
             (number-text (dom:radial-dimension-text-height entity))
             (keyword-text (dom:radial-dimension-unit entity)) (dom:radial-dimension-precision entity)))
    (dom:angular-dimension-entity
     (format nil "(angle-dimension ~a ~a ~a ~a ~a :height ~a :decimals ~d)"
             (number-text (dom:angular-dimension-vertex-x entity))
             (number-text (dom:angular-dimension-vertex-y entity))
             (angle-text (dom:angular-dimension-start-angle entity))
             (angle-text (dom:angular-dimension-end-angle entity))
             (number-text (dom:angular-dimension-radius entity))
             (number-text (dom:angular-dimension-text-height entity))
             (dom:angular-dimension-precision entity)))))

(defun keyword-text (keyword)
  "Return a keyword as lower case source.

KEYWORD: a keyword."
  (format nil ":~(~a~)" (symbol-name keyword)))

(defun layer-command (layer)
  "Return the command that makes a layer with its color; it is shown and unlocked, so that the entities
that belong on it can be drawn.

LAYER: a layer."
  (format nil "(layer ~a :color ~a)"
          (string-text (dom:layer-name layer))
          (color-text (dom:layer-color layer))))

(defun layer-state-command (layer)
  "Return the command that hides or locks a layer as it is, or NIL when it is shown and unlocked.

LAYER: a layer."
  (and (or (not (dom:layer-visible layer)) (dom:layer-locked layer))
       (format nil "(layer ~a :visible ~a :locked ~a)"
               (string-text (dom:layer-name layer))
               (boolean-text (dom:layer-visible layer))
               (boolean-text (dom:layer-locked layer)))))

(defun write-block-definition (definition
                               stream)
  "Write the command that makes a block, with the commands that draw what is in it.

DEFINITION: a block definition.
STREAM: the stream to write to."
  (format stream "(with-block ~a~%" (string-text (dom:block-definition-name definition)))
  (dolist (entity (dom:block-definition-entities definition))
    (format stream "  ~a~%" (entity-command entity)))
  (format stream ")~%"))

(defun drawing-script (drawing)
  "Return the text of a script that makes DRAWING again when it is run on an empty drawing: its settings, its
layers, its entities in the order they are drawn, each on the layer it is on, and last the layers that are
hidden or locked, so that locking a layer does not stop its entities from being drawn.

DRAWING: the drawing."
  (with-output-to-string (stream)
    (dolist (line *script-header*)
      (write-line line stream))
    (terpri stream)
    (format stream "(display-unit ~a)~%" (keyword-text (dom:drawing-display-unit drawing)))
    (format stream "(grid-spacing ~a)~%" (number-text (dom:drawing-grid-spacing drawing)))
    (terpri stream)
    (dolist (layer (dom:layers-in-order drawing))
      (write-line (layer-command layer) stream))
    (terpri stream)
    (dolist (definition (dom:block-definitions drawing))
      (write-block-definition definition stream))
    (let ((current nil))
      (dom:map-entities (lambda (entity)
                          (unless (eql current (dom:entity-layer-id entity))
                            (setf current (dom:entity-layer-id entity))
                            (format stream "(layer ~a)~%"
                                    (string-text (dom:layer-name (dom:find-layer drawing current)))))
                          (write-line (entity-command entity) stream))
                        (dom:drawing-store drawing)))
    (terpri stream)
    (dolist (layer (dom:layers-in-order drawing))
      (let ((command (layer-state-command layer)))
        (when command
          (write-line command stream))))
    (format stream "(layer ~a)~%"
            (string-text (dom:layer-name (dom:find-layer drawing (dom:drawing-current-layer-id drawing)))))))
