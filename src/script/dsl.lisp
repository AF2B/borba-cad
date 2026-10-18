(in-package #:borba-cad/script)

;;;; The commands. Numbers are millimetres unless a command says otherwise, and angles are degrees unless
;;;; they are written (rad x), a number of radians. Every command that makes an entity returns its id, which
;;;; the commands that change it take.

;;; Numbers, units, angles and colors

(defun number-of (value)
  "Return VALUE as a double-float, or signal an error saying what was wanted.

VALUE: a real number."
  (unless (realp value)
    (error "A number was expected, but ~s was given." value))
  (float value 1d0))

(defun mm (value)
  "Return VALUE millimetres in millimetres, the unit of every length: (mm 3) is 3.

VALUE: a number."
  (number-of value))

(defun cm (value)
  "Return VALUE centimetres as millimetres: (cm 20) is 200.

VALUE: a number."
  (* 10d0 (number-of value)))

(defun m (value)
  "Return VALUE metres as millimetres: (m 5) is 5000.

VALUE: a number."
  (* 1000d0 (number-of value)))

(defun inch (value)
  "Return VALUE inches as millimetres: (inch 1) is 25.4.

VALUE: a number."
  (* 25.4d0 (number-of value)))

(defun foot (value)
  "Return VALUE feet as millimetres: (foot 1) is 304.8.

VALUE: a number."
  (* 304.8d0 (number-of value)))

(defstruct (radians (:constructor make-radians (value)))
  "An angle written in radians, which a command takes where it takes degrees.

VALUE: the angle in radians."
  (value 0d0 :type double-float))

(defun rad (value)
  "Return an angle of VALUE radians, to be given where a command takes an angle: (rad pi) is half a turn.

VALUE: a number of radians."
  (make-radians (number-of value)))

(defun angle-radians (value)
  "Return an angle given to a command as radians: a number is degrees, and (rad x) is x radians.

VALUE: a number of degrees or a value made by RAD."
  (if (radians-p value)
      (radians-value value)
      (base:degrees->radians (number-of value))))

(defparameter *color-names*
  '((:white 235 235 235) (:black 20 20 20) (:red 255 80 80) (:green 90 200 90) (:blue 80 140 255)
    (:yellow 240 210 70) (:cyan 70 210 220) (:magenta 220 100 220) (:orange 255 150 60) (:gray 140 140 140))
  "The colors a layer can be given by name, as (name red green blue).")

(defun rgb (red
            green
            blue)
  "Return a color from its red, green and blue, each from 0 to 255.

RED: the red.
GREEN: the green.
BLUE: the blue."
  (dolist (channel (list red green blue))
    (unless (typep channel '(integer 0 255))
      (error "A color channel is a whole number from 0 to 255, but ~s was given." channel)))
  (dom:make-rgb red
                green
                blue))

(defun color-of (specification)
  "Return the color that SPECIFICATION means: a color made by RGB, or the name of one of the colors the
program knows.

SPECIFICATION: a color or a keyword such as :red."
  (cond ((typep specification 'dom:rgb) specification)
        ((and (keywordp specification) (assoc specification *color-names*))
         (destructuring-bind (red green blue) (rest (assoc specification *color-names*))
           (dom:make-rgb red
                         green
                         blue)))
        (t (error "~s is not a color: use (rgb red green blue), or one of ~{~(~s~)~^, ~}."
                  specification
                  (mapcar #'first *color-names*)))))

;;; The drawing and its layers

(defun the-drawing ()
  "Return the drawing the running command acts on."
  (script-context-drawing (current-context)))

(defun creation-layer-id ()
  "Return the id of the layer new entities go on, signaling LAYER-ERROR when it is locked, as the tools do."
  (let* ((drawing (the-drawing))
         (layer (dom:find-layer drawing (dom:drawing-current-layer-id drawing))))
    (when (dom:layer-locked layer)
      (error 'base:layer-error
             :reason (i18n:trf "layer ~s is locked; unlock it or choose another layer" (dom:layer-name layer))))
    (dom:layer-id layer)))

(defun layer (name
              &key color
                   (visible nil visible-p)
                   (locked nil locked-p))
  "Make the layer called NAME the one new entities go on, making the layer first when there is none. Returns
the name.

NAME: the name of the layer.
COLOR: a color (see RGB) or a name such as :red, for a layer that is new or whose color is to change.
VISIBLE: true or false, to show or hide the layer.
LOCKED: true or false, to lock or unlock it."
  (let* ((drawing (the-drawing))
         (existing (dom:find-layer-by-name drawing name))
         (made (or existing
                   (dom:add-layer drawing
                                  name
                                  (if color
                                      (color-of color)
                                      (dom:palette-color-at (dom:drawing-next-layer-id drawing)))))))
    (when (and existing color)
      (dom:recolor-layer drawing
                         (dom:layer-id made)
                         (color-of color)))
    (when visible-p
      (dom:set-layer-visible drawing
                             (dom:layer-id made)
                             (and visible t)))
    (when locked-p
      (dom:set-layer-locked drawing
                            (dom:layer-id made)
                            (and locked t)))
    (dom:set-current-layer drawing (dom:layer-id made))
    (dom:layer-name (dom:find-layer drawing (dom:layer-id made)))))

(defmacro with-layer ((name &rest options) &body body)
  "Run BODY with the layer called NAME, made when there is none, as the one new entities go on, and then go
back to the layer that was in use.

NAME: the name of the layer.
OPTIONS: the keyword arguments of LAYER.
BODY: the commands to run."
  (let ((drawing (gensym "DRAWING"))
        (previous (gensym "PREVIOUS")))
    `(let* ((,drawing (the-drawing))
            (,previous (dom:drawing-current-layer-id ,drawing)))
       (unwind-protect
            (progn (layer ,name ,@options)
                   ,@body)
         (dom:set-current-layer ,drawing ,previous)))))

(defun layers ()
  "Return the names of the layers, in the order they are listed."
  (mapcar #'dom:layer-name (dom:layers-in-order (the-drawing))))

;;; Making entities

(defvar *block-collector* nil
  "While a block is being made, a cons whose car is the list of the entities drawn into it so far, newest first.")

(defun add-entity (builder)
  "Make an entity with BUILDER, add it to the drawing, and return its id. While a block is being made the entity goes
into the block instead, with the next id of the block, on the default layer.

BUILDER: function of the id and the layer id that returns the entity."
  (if *block-collector*
      (let ((entity (funcall builder
                             (1+ (length (car *block-collector*)))
                             dom:+default-layer-id+)))
        (push entity (car *block-collector*))
        (dom:entity-id entity))
      (let* ((drawing (the-drawing))
             (layer-id (creation-layer-id))
             (entity (funcall builder (dom:drawing-next-entity-id drawing) layer-id)))
        (emit-change nil entity))))

(defun line (x1
             y1
             x2
             y2)
  "Draw a line from (X1, Y1) to (X2, Y2). Returns its id.

X1: x of the start.
Y1: y of the start.
X2: x of the end.
Y2: y of the end."
  (add-entity (lambda (id layer-id)
                (dom:make-line :id id
                               :layer-id layer-id
                               :x1 (number-of x1)
                               :y1 (number-of y1)
                               :x2 (number-of x2)
                               :y2 (number-of y2)))))

(defun rect (x1
             y1
             x2
             y2)
  "Draw a rectangle with sides along the axes from one corner, (X1, Y1), to the opposite one, (X2, Y2).
Returns its id.

X1: x of one corner.
Y1: y of one corner.
X2: x of the opposite corner.
Y2: y of the opposite corner."
  (add-entity (lambda (id layer-id)
                (dom:make-rectangle-from-corners :id id
                                                 :layer-id layer-id
                                                 :x1 (number-of x1)
                                                 :y1 (number-of y1)
                                                 :x2 (number-of x2)
                                                 :y2 (number-of y2)))))

(defun rect-size (x
                  y
                  width
                  height)
  "Draw a rectangle with sides along the axes whose lower left corner is (X, Y), WIDTH wide and HEIGHT high.
Returns its id.

X: x of the lower left corner.
Y: y of the lower left corner.
WIDTH: its width.
HEIGHT: its height."
  (add-entity (lambda (id layer-id)
                (dom:make-rectangle :id id
                                    :layer-id layer-id
                                    :min-x (number-of x)
                                    :min-y (number-of y)
                                    :width (number-of width)
                                    :height (number-of height)))))

(defun circle (x
               y
               radius)
  "Draw a circle centered on (X, Y). Returns its id.

X: x of the center.
Y: y of the center.
RADIUS: its radius."
  (add-entity (lambda (id layer-id)
                (dom:make-circle :id id
                                 :layer-id layer-id
                                 :center-x (number-of x)
                                 :center-y (number-of y)
                                 :radius (number-of radius)))))

(defun arc (x
            y
            radius
            start
            end)
  "Draw the arc of the circle centered on (X, Y) that turns counter-clockwise from the angle START to the
angle END. Returns its id.

X: x of the center.
Y: y of the center.
RADIUS: its radius.
START: the angle it starts at, in degrees.
END: the angle it ends at, in degrees."
  (add-entity (lambda (id layer-id)
                (dom:make-arc :id id
                              :layer-id layer-id
                              :center-x (number-of x)
                              :center-y (number-of y)
                              :radius (number-of radius)
                              :start-angle (angle-radians start)
                              :end-angle (angle-radians end)))))

(defun ellipse (x
                y
                radius-x
                radius-y
                &optional (rotation 0))
  "Draw an ellipse centered on (X, Y) with the semi-axes RADIUS-X, along the direction ROTATION, and RADIUS-Y,
across it. Returns its id.

X: x of the center.
Y: y of the center.
RADIUS-X: the semi-axis along ROTATION.
RADIUS-Y: the semi-axis across it.
ROTATION: the direction of the first axis, in degrees."
  (add-entity (lambda (id layer-id)
                (dom:make-ellipse :id id
                                  :layer-id layer-id
                                  :center-x (number-of x)
                                  :center-y (number-of y)
                                  :radius-x (number-of radius-x)
                                  :radius-y (number-of radius-y)
                                  :rotation (angle-radians rotation)))))

(defun polygon (x
                y
                radius
                sides
                &optional (rotation 0))
  "Draw a regular polygon whose vertices are on the circle centered on (X, Y), the first in the direction
ROTATION. Returns its id.

X: x of the center.
Y: y of the center.
RADIUS: the radius of that circle.
SIDES: the number of sides, from 3 to 1000.
ROTATION: the direction of the first vertex, in degrees."
  (add-entity (lambda (id layer-id)
                (dom:make-polygon :id id
                                  :layer-id layer-id
                                  :center-x (number-of x)
                                  :center-y (number-of y)
                                  :radius (number-of radius)
                                  :sides sides
                                  :rotation (angle-radians rotation)))))

(defun pline (points
              &key closed)
  "Draw a polyline through POINTS. Returns its id.

POINTS: a list of (x y) lists, two or more.
CLOSED: true to join the last point to the first."
  (unless (and (listp points) (every (lambda (point) (and (listp point) (= 2 (length point)))) points))
    (error "A polyline takes a list of points, each (x y), but ~s was given." points))
  (let ((coordinates (make-array (* 2 (length points)) :element-type 'double-float)))
    (loop for (x y) in points
          for index from 0 by 2
          do (setf (aref coordinates index) (number-of x)
                   (aref coordinates (1+ index)) (number-of y)))
    (add-entity (lambda (id layer-id)
                  (dom:make-polyline :id id
                                     :layer-id layer-id
                                     :coordinates coordinates
                                     :closed (and closed t))))))

(defun text (x
             y
             string
             &key height
                  (angle 0))
  "Write STRING with its baseline starting at (X, Y). Returns its id.

X: x of the start of the baseline.
Y: y of the start of the baseline.
STRING: the text.
HEIGHT: the text height, or the one the drawing uses for new text.
ANGLE: the direction of the baseline, in degrees."
  (add-entity (lambda (id layer-id)
                (dom:make-text :id id
                               :layer-id layer-id
                               :x (number-of x)
                               :y (number-of y)
                               :string string
                               :height (if height
                                           (number-of height)
                                           (dom:drawing-default-text-height (the-drawing)))
                               :angle (angle-radians angle)))))

(defun dimension (x1
                  y1
                  x2
                  y2
                  offset
                  &key height
                       unit
                       decimals)
  "Dimension the distance between (X1, Y1) and (X2, Y2) with a line OFFSET away from the line through them,
to its left when positive. Returns its id.

X1: x of the first point.
Y1: y of the first point.
X2: x of the second point.
Y2: y of the second point.
OFFSET: how far the dimension line is, to the left of the direction from the first point to the second.
HEIGHT: the text height, or the one the drawing uses.
UNIT: :millimeter, :centimeter or :meter, or the one lengths are shown in.
DECIMALS: how many decimals the text shows, or the usual number for the unit."
  (let* ((drawing (the-drawing))
         (unit (or unit (dom:drawing-display-unit drawing))))
    (add-entity (lambda (id layer-id)
                  (dom:make-dimension :id id
                                      :layer-id layer-id
                                      :x1 (number-of x1)
                                      :y1 (number-of y1)
                                      :x2 (number-of x2)
                                      :y2 (number-of y2)
                                      :offset (number-of offset)
                                      :text-height (if height
                                                       (number-of height)
                                                       (dom:drawing-default-text-height drawing))
                                      :unit unit
                                      :precision (or decimals (base:default-display-precision unit)))))))

(defconstant +default-angle-decimals+ 1
  "How many decimals of a degree the text of an angle dimension shows, unless the command says.")

(defun radial-dimension (x
                         y
                         radius
                         angle
                         diameter
                         height
                         unit
                         decimals)
  "Add the dimension of a circle, as a radius or as a diameter. Returns its id.

X: x of the center.
Y: y of the center.
RADIUS: the radius of the circle.
ANGLE: the direction of the leader from the center, in degrees.
DIAMETER: true for a diameter dimension.
HEIGHT: the text height, or NIL for the one the drawing uses.
UNIT: the unit of the text, or NIL for the one lengths are shown in.
DECIMALS: how many decimals the text shows, or NIL for the usual number for the unit."
  (let* ((drawing (the-drawing))
         (unit (or unit (dom:drawing-display-unit drawing))))
    (add-entity (lambda (id layer-id)
                  (dom:make-radial-dimension :id id
                                             :layer-id layer-id
                                             :center-x (number-of x)
                                             :center-y (number-of y)
                                             :radius (number-of radius)
                                             :angle (angle-radians angle)
                                             :diameter diameter
                                             :text-height (if height
                                                              (number-of height)
                                                              (dom:drawing-default-text-height drawing))
                                             :unit unit
                                             :precision (or decimals (base:default-display-precision unit)))))))

(defun radius-dimension (x
                         y
                         radius
                         &key (angle 45)
                              height
                              unit
                              decimals)
  "Dimension the radius of the circle centered on (X, Y), with a leader that leaves the center at ANGLE. Returns
its id.

X: x of the center.
Y: y of the center.
RADIUS: the radius of the circle.
ANGLE: the direction of the leader, in degrees.
HEIGHT: the text height, or the one the drawing uses.
UNIT: :millimeter, :centimeter or :meter, or the one lengths are shown in.
DECIMALS: how many decimals the text shows, or the usual number for the unit."
  (radial-dimension x
                    y
                    radius
                    angle
                    nil
                    height
                    unit
                    decimals))

(defun diameter-dimension (x
                           y
                           radius
                           &key (angle 45)
                                height
                                unit
                                decimals)
  "Dimension the diameter of the circle centered on (X, Y), with a line through the center at ANGLE. Returns its
id.

X: x of the center.
Y: y of the center.
RADIUS: the radius of the circle (the text says twice that).
ANGLE: the direction of the line, in degrees.
HEIGHT: the text height, or the one the drawing uses.
UNIT: :millimeter, :centimeter or :meter, or the one lengths are shown in.
DECIMALS: how many decimals the text shows, or the usual number for the unit."
  (radial-dimension x
                    y
                    radius
                    angle
                    t
                    height
                    unit
                    decimals))

(defun angle-dimension (x
                        y
                        start
                        end
                        radius
                        &key height
                             decimals)
  "Dimension the angle at the vertex (X, Y) from the arm that points toward START to the arm that points toward
END, counter-clockwise, with an arc of the given radius. Returns its id.

X: x of the vertex.
Y: y of the vertex.
START: the direction of the first arm, in degrees.
END: the direction of the second arm, in degrees.
RADIUS: the radius of the arc that carries the dimension.
HEIGHT: the text height, or the one the drawing uses.
DECIMALS: how many decimals of a degree the text shows, or 1."
  (add-entity (lambda (id layer-id)
                (dom:make-angular-dimension :id id
                                            :layer-id layer-id
                                            :vertex-x (number-of x)
                                            :vertex-y (number-of y)
                                            :start-angle (angle-radians start)
                                            :end-angle (angle-radians end)
                                            :radius (number-of radius)
                                            :text-height (if height
                                                             (number-of height)
                                                             (dom:drawing-default-text-height (the-drawing)))
                                            :precision (or decimals +default-angle-decimals+)))))

(defun hatch-pattern-of (pattern)
  "Return the hatch pattern keyword a command was given, or signal an error saying which are known.

PATTERN: :solid, :lines or :cross."
  (unless (member pattern dom:*hatch-patterns*)
    (error "A hatch pattern is one of ~{~(:~a~)~^, ~}, but ~s was given." dom:*hatch-patterns* pattern))
  pattern)

(defun hatch-polygon (points
                      &key (pattern :lines)
                           (angle 45)
                           spacing)
  "Hatch the polygon through POINTS. Returns its id.

POINTS: a list of (x y) lists, three or more, the last joined to the first.
PATTERN: :solid, :lines or :cross.
ANGLE: the direction of the lines, in degrees.
SPACING: the distance between two lines, or the usual one for the size of the polygon."
  (unless (and (listp points) (every (lambda (point) (and (listp point) (= 2 (length point)))) points))
    (error "A hatch takes a list of points, each (x y), but ~s was given." points))
  (let ((coordinates (make-array (* 2 (length points)) :element-type 'double-float)))
    (loop for (x y) in points
          for index from 0 by 2
          do (setf (aref coordinates index) (number-of x)
                   (aref coordinates (1+ index)) (number-of y)))
    (add-entity (lambda (id layer-id)
                  (dom:make-hatch :id id
                                  :layer-id layer-id
                                  :coordinates coordinates
                                  :pattern (hatch-pattern-of pattern)
                                  :angle (angle-radians angle)
                                  :spacing (if spacing
                                               (number-of spacing)
                                               (dom:default-hatch-spacing coordinates
                                                                          (dom:drawing-default-text-height
                                                                           (the-drawing)))))))))

(defun hatch (x
              y
              &key (pattern :lines)
                   (angle 45)
                   spacing)
  "Hatch the smallest closed shape that holds the point (X, Y): a rectangle, circle, ellipse, polygon or closed
polyline. Returns its id.

X: x of the point.
Y: y of the point.
PATTERN: :solid, :lines or :cross.
ANGLE: the direction of the lines, in degrees.
SPACING: the distance between two lines, or the usual one for the size of the shape."
  (let* ((drawing (the-drawing))
         (changes (handler-case (dom:hatch-changes drawing
                                                   (number-of x)
                                                   (number-of y)
                                                   :pattern (hatch-pattern-of pattern)
                                                   :angle (angle-radians angle)
                                                   :spacing (and spacing (number-of spacing)))
                    (base:edit-refused (condition)
                      (error "~a" condition)))))
    (emit-change nil (dom:change-after (first changes)))))

;;; Blocks

(defun call-with-block (name
                        thunk)
  "Run THUNK, with what it draws going into a new block called NAME instead of the drawing, and add the block to the
drawing. Returns the name of the block.

NAME: what the block is called.
THUNK: function of no arguments that draws."
  (let ((*block-collector* (list nil)))
    (funcall thunk)
    (let ((definition (dom:make-block-definition :name name
                                                 :entities (reverse (car *block-collector*)))))
      (dom:define-block (the-drawing) definition)
      (dom:block-definition-name definition))))

(defmacro with-block (name &body body)
  "Make a block called NAME of the entities that BODY draws; they are not drawn in the drawing. Place it with INSERT.
Returns the name.

NAME: what the block is called.
BODY: drawing commands, such as LINE and CIRCLE."
  `(call-with-block ,name (lambda () ,@body)))

(defun make-block (name
                   ids
                   &key (base-x 0)
                        (base-y 0))
  "Make a block called NAME of the entities with IDS, which are replaced by one placement of it at the base point.
Returns the id of the placement.

NAME: what the block is called.
IDS: an id or a list of ids.
BASE-X: x of the point that is the base point of the block.
BASE-Y: y of that point."
  (multiple-value-bind (definition changes)
      (dom:make-block-changes (the-drawing)
                              (entity-ids-of ids)
                              name
                              (number-of base-x)
                              (number-of base-y))
    (dom:define-block (the-drawing) definition)
    (let ((placed nil))
      (dolist (change changes)
        (let ((after (dom:change-after change)))
          (emit-change (dom:change-before change) after)
          (when after
            (setf placed (dom:entity-id after)))))
      placed)))

(defun insert (name
               x
               y
               &key (scale 1)
                    (rotation 0)
                    flipped)
  "Place the block called NAME with its base point at (X, Y). Returns the id of the placement.

NAME: the name of the block.
X: x of the point.
Y: y of the point.
SCALE: how many times larger than the block.
ROTATION: the turn, in degrees.
FLIPPED: true to reflect the block across its vertical axis before it is turned."
  (when *block-collector*
    (error "A block cannot hold a block: place it after the block is made."))
  (creation-layer-id)
  (let ((changes (dom:insert-changes (the-drawing)
                                     name
                                     (number-of x)
                                     (number-of y)
                                     :scale (float (number-of scale) 1d0)
                                     :rotation (angle-radians rotation)
                                     :flipped flipped)))
    (emit-change nil (dom:change-after (first changes)))))

(defun explode (ids)
  "Replace the placements of blocks among IDS by the entities they stand for. Returns how many were exploded.

IDS: an id or a list of ids."
  (multiple-value-bind (changes exploded) (dom:explode-changes (the-drawing) (entity-ids-of ids))
    (dolist (change changes)
      (emit-change (dom:change-before change) (dom:change-after change)))
    exploded))

(defun blocks ()
  "Return the names of the blocks, in alphabetical order."
  (dom:block-names (the-drawing)))

;;; Finding entities

(defun entity-ids-of (designator)
  "Return the ids an argument stands for: one id, or a list of them.

DESIGNATOR: an id or a list of ids."
  (cond ((integerp designator) (list designator))
        ((and (listp designator) (every #'integerp designator)) designator)
        (t (error "An entity is its id, a whole number, or a list of ids, but ~s was given." designator))))

(defun the-entity (id)
  "Return the entity with an id, or signal an error saying there is none.

ID: an entity id."
  (or (dom:store-find (dom:drawing-store (the-drawing)) id)
      (error "There is no entity ~d." id)))

(defun editable-entity (id)
  "Return the entity with an id, signaling LAYER-ERROR when its layer is locked.

ID: an entity id."
  (let ((entity (the-entity id)))
    (unless (dom:editable-entity-p (the-drawing) entity)
      (error 'base:layer-error
             :reason (i18n:trf "entity ~d is on a locked layer" id)))
    entity))

(defun entities (&key kind
                      layer)
  "Return the ids of the entities, in the order they are drawn, bottom first.

KIND: only the entities of this kind: :line, :rectangle, :circle, :polyline, :text, :dimension,
:radial-dimension, :angular-dimension, :arc, :ellipse, :polygon or :hatch.
LAYER: only the entities on the layer with this name."
  (let ((wanted-layer (and layer (dom:find-layer-by-name (the-drawing) layer)))
        (ids '()))
    (when (and layer (null wanted-layer))
      (error "There is no layer called ~s." layer))
    (dom:map-entities (lambda (entity)
                        (when (and (or (null kind) (eq kind (dom:entity-kind entity)))
                                   (or (null wanted-layer)
                                       (= (dom:layer-id wanted-layer) (dom:entity-layer-id entity))))
                          (push (dom:entity-id entity) ids)))
                      (dom:drawing-store (the-drawing)))
    (nreverse ids)))

(defun entity-count ()
  "Return how many entities the drawing has."
  (dom:drawing-entity-count (the-drawing)))

(defun extents ()
  "Return the smallest rectangle that holds the drawing, as the list (min-x min-y max-x max-y), or NIL when
it is empty."
  (let ((bounds nil))
    (dom:map-entities (lambda (entity)
                        (setf bounds (if bounds
                                         (geo:bounds-union bounds (dom:entity-bounds entity))
                                         (dom:entity-bounds entity))))
                      (dom:drawing-store (the-drawing)))
    (and bounds
         (list (geo:bounds-min-x bounds) (geo:bounds-min-y bounds)
               (geo:bounds-max-x bounds) (geo:bounds-max-y bounds)))))

;;; Changing entities

(defun move (ids
             dx
             dy)
  "Move entities by (DX, DY). Entities on a locked layer are left where they are. Returns how many moved.

IDS: an id or a list of ids.
DX: how far along x.
DY: how far along y."
  (let ((moved 0)
        (dx (number-of dx))
        (dy (number-of dy)))
    (dolist (id (entity-ids-of ids))
      (let ((entity (the-entity id)))
        (when (dom:editable-entity-p (the-drawing) entity)
          (emit-change entity (dom:entity-translated entity
                                                     dx
                                                     dy))
          (incf moved))))
    moved))

(defun copy (ids
             dx
             dy)
  "Make copies of entities, moved by (DX, DY) from the originals. Returns the ids of the copies.

IDS: an id or a list of ids.
DX: how far along x.
DY: how far along y."
  (let ((dx (number-of dx))
        (dy (number-of dy))
        (copies '()))
    (dolist (id (entity-ids-of ids))
      (let* ((entity (the-entity id))
             (duplicate (dom:entity-translated
                         (dom:entity-with-identity entity
                                                   (dom:drawing-next-entity-id (the-drawing))
                                                   (dom:entity-layer-id entity))
                         dx
                         dy)))
        (push (emit-change nil duplicate) copies)))
    (nreverse copies)))

(defun transform-ids (ids
                      transform
                      copy)
  "Change the entities with IDS by what TRANSFORM makes of them, one change each, or add what it makes of them
as new entities when COPY is true. Entities on a locked layer are left. Returns how many were changed, or the ids
of the copies.

IDS: an id or a list of ids.
TRANSFORM: function of an entity that returns the transformed entity.
COPY: true to keep the originals and add the results."
  (let ((changed 0)
        (copies '()))
    (dolist (id (entity-ids-of ids))
      (let ((entity (the-entity id)))
        (when (dom:editable-entity-p (the-drawing) entity)
          (let ((result (funcall transform entity)))
            (cond (copy
                   (push (emit-change nil
                                      (dom:entity-with-identity result
                                                                (dom:drawing-next-entity-id (the-drawing))
                                                                (dom:entity-layer-id entity)))
                         copies))
                  (t
                   (emit-change entity result)
                   (incf changed)))))))
    (if copy
        (nreverse copies)
        changed)))

(defun rotate (ids
               angle
               &key (about-x 0)
                    (about-y 0)
                    copy)
  "Turn entities counter-clockwise by an angle about a point, or, with COPY, add turned copies of them. A
rectangle turned to a slant becomes a closed polyline. Entities on a locked layer are left. Returns how many
were turned, or the ids of the copies.

IDS: an id or a list of ids.
ANGLE: the angle in degrees, or (rad x).
ABOUT-X: x of the center of the turn.
ABOUT-Y: y of the center.
COPY: true to keep the originals and add the turned copies."
  (let ((radians (angle-radians angle))
        (center-x (number-of about-x))
        (center-y (number-of about-y)))
    (transform-ids ids
                   (lambda (entity) (dom:entity-rotated entity
                                                        center-x
                                                        center-y
                                                        radians))
                   copy)))

(defun mirror (ids
               x1
               y1
               x2
               y2
               &key copy)
  "Reflect entities in the line through two points, or, with COPY, add reflected copies of them. A text keeps
facing the way it did; only where it starts is reflected. Entities on a locked layer are left. Returns how many
were reflected, or the ids of the copies.

IDS: an id or a list of ids.
X1: x of the first point of the line.
Y1: y of the first point.
X2: x of the second point.
Y2: y of the second point.
COPY: true to keep the originals and add the reflected copies."
  (let ((line-x1 (number-of x1))
        (line-y1 (number-of y1))
        (line-x2 (number-of x2))
        (line-y2 (number-of y2)))
    (when (= line-x1 line-x2)
      (when (= line-y1 line-y2)
        (error "The two points of the mirror line must be apart.")))
    (transform-ids ids
                   (lambda (entity) (dom:entity-mirrored entity
                                                         line-x1
                                                         line-y1
                                                         line-x2
                                                         line-y2))
                   copy)))

(defun scale (ids
              factor
              &key (about-x 0)
                   (about-y 0)
                   copy)
  "Scale entities about a point by a positive factor, or, with COPY, add scaled copies of them. Entities on a
locked layer are left. Returns how many were scaled, or the ids of the copies.

IDS: an id or a list of ids.
FACTOR: what every length is multiplied by.
ABOUT-X: x of the center of the scaling.
ABOUT-Y: y of the center.
COPY: true to keep the originals and add the scaled copies."
  (let ((multiplier (number-of factor))
        (center-x (number-of about-x))
        (center-y (number-of about-y)))
    (unless (> multiplier 0d0)
      (error "The factor must be above zero, but ~s was given." factor))
    (transform-ids ids
                   (lambda (entity) (dom:entity-scaled entity
                                                       center-x
                                                       center-y
                                                       multiplier))
                   copy)))

(defun erase (ids)
  "Erase entities. Entities on a locked layer are left. Returns how many were erased.

IDS: an id or a list of ids."
  (let ((erased 0))
    (dolist (id (entity-ids-of ids))
      (let ((entity (the-entity id)))
        (when (dom:editable-entity-p (the-drawing) entity)
          (emit-change entity nil)
          (incf erased))))
    erased))

(defun property-value-for (property
                           value)
  "Return the value to give an entity for a property: a length as millimetres, an angle in radians from
degrees, and text and whole numbers as they are.

PROPERTY: an inspector property of the entity.
VALUE: what the command was given."
  (ecase (dom:property-kind property)
    ((:coordinate :length) (number-of value))
    (:angle (angle-radians value))
    (:factor (float (number-of value) 1d0))
    ((:text :integer :boolean :unit :pattern) value)))

(defun edit (id
             &rest changes)
  "Change properties of an entity. Returns its id.

ID: the id of the entity.
CHANGES: alternating property keywords and values, such as :radius 500 or :string \"Kitchen\"; the keywords
are the ones the inspector lists (:center-x, :center-y, :radius, :x1, :min-x, :height, :angle, :sides, ...),
lengths are millimetres and angles are degrees."
  (unless (evenp (length changes))
    (error "EDIT takes pairs of a property and a value, but got ~s." changes))
  (let ((entity (editable-entity id)))
    (loop for (key value) on changes by #'cddr
          do (let ((property (find key (dom:entity-properties entity) :key #'dom:property-key)))
               (unless property
                 (error "A ~(~a~) has no property ~s; it has ~{~(~s~)~^, ~}."
                        (dom:entity-kind entity)
                        key
                        (mapcar #'dom:property-key (dom:entity-properties entity))))
               (setf entity (dom:entity-with-property entity
                                                      key
                                                      (property-value-for property value)))))
    (emit-change (the-entity id) entity)
    id))

(defun move-to-layer (ids
                      name)
  "Move entities to the layer called NAME. Returns how many moved.

IDS: an id or a list of ids.
NAME: the name of the layer."
  (let ((layer (dom:find-layer-by-name (the-drawing) name))
        (moved 0))
    (unless layer
      (error "There is no layer called ~s." name))
    (dolist (id (entity-ids-of ids))
      (let ((entity (editable-entity id)))
        (emit-change entity (dom:entity-with-layer entity (dom:layer-id layer)))
        (incf moved)))
    moved))

;;; Selection and view

(defun select (ids)
  "Select entities in the window. Returns how many are selected.

IDS: an id or a list of ids."
  (let ((ids (remove-if-not (lambda (id) (dom:store-find (dom:drawing-store (the-drawing)) id))
                            (entity-ids-of ids)))
        (writer (script-context-selection-writer (current-context))))
    (when writer
      (funcall writer ids))
    (length ids)))

(defun select-all ()
  "Select every entity that can be selected. Returns how many are selected."
  (select (entities)))

(defun deselect ()
  "Select nothing. Returns 0."
  (select '()))

(defun selected ()
  "Return the ids of the entities selected in the window."
  (let ((reader (script-context-selection-reader (current-context))))
    (and reader (funcall reader))))

(defun fit ()
  "Zoom the window to show the whole drawing."
  (let ((fitter (script-context-view-fitter (current-context))))
    (when fitter
      (funcall fitter))
    nil))

;;; The drawing's settings

(defun display-unit (&optional unit)
  "Return the unit that lengths are shown in, after setting it to UNIT when one is given. The numbers that
commands take stay millimetres whatever it is.

UNIT: :millimeter, :centimeter or :meter."
  (when unit
    (dom:set-display-unit (the-drawing) unit))
  (dom:drawing-display-unit (the-drawing)))

(defun grid-spacing (&optional spacing)
  "Return the spacing of the grid, in millimetres, after setting it to SPACING when one is given.

SPACING: the new spacing."
  (when spacing
    (dom:set-grid-spacing (the-drawing) (number-of spacing)))
  (dom:drawing-grid-spacing (the-drawing)))

;;; Asking and talking

(defun info (id)
  "Say what an entity is: its kind, its layer, and each of its properties and measures. Returns its id.

ID: the id of the entity."
  (let* ((entity (the-entity id))
         (layer (dom:find-layer (the-drawing) (dom:entity-layer-id entity))))
    (format t "~(~a~) ~d on layer ~s~%" (dom:entity-kind entity) id (dom:layer-name layer))
    (dolist (property (dom:entity-properties entity))
      (format t "  ~a: ~a~%" (dom:property-label property) (printed-property-value property)))
    (dolist (derived (dom:entity-derived-values entity))
      (format t "  ~a: ~a~%" (dom:derived-value-label derived) (printed-derived-value derived)))
    id))

(defun printed-property-value (property)
  "Return a property's value as the commands take it: millimetres, and degrees for an angle.

PROPERTY: an inspector property."
  (let ((value (dom:property-value property)))
    (case (dom:property-kind property)
      (:angle (base:format-decimal (base:radians->degrees value) 4))
      ((:coordinate :length :factor) (persist:format-double value))
      (:boolean (if value "yes" "no"))
      ((:unit :pattern) (string-downcase (symbol-name value)))
      (t value))))

(defun printed-derived-value (derived)
  "Return a measure of an entity as text, in millimetres, square millimetres or degrees.

DERIVED: a derived value."
  (let ((value (dom:derived-value-value derived)))
    (ecase (dom:derived-value-kind derived)
      (:length (format nil "~a mm" (persist:format-double value)))
      (:area (format nil "~a mm2" (persist:format-double value)))
      (:angle (format nil "~a degrees" (base:format-decimal (base:radians->degrees value) 4)))
      (:count (format nil "~d" value)))))

(defun say (&rest things)
  "Write THINGS in the console, separated by spaces. Returns NIL.

THINGS: anything."
  (format t "~{~a~^ ~}~%" things)
  nil)

(defparameter *help-topics*
  '(("Making entities" line rect rect-size circle arc ellipse polygon pline text dimension radius-dimension
     diameter-dimension angle-dimension hatch hatch-polygon)
    ("Layers" layer with-layer layers)
    ("Blocks" with-block make-block insert explode blocks)
    ("Changing entities" move copy erase edit move-to-layer rotate mirror scale)
    ("Finding entities" entities entity-count extents info)
    ("Selection and view" select select-all deselect selected fit)
    ("Settings" display-unit grid-spacing)
    ("Units, angles, colors" mm cm m inch foot rad rgb)
    ("Talking" say help))
  "The commands, by topic, as the help lists them.")

(defun first-line (string)
  "Return the first line of STRING.

STRING: a string."
  (subseq string 0 (position #\Newline string)))

(defun help (&optional command)
  "List the commands, or say what one of them does when COMMAND, a symbol, is given.

COMMAND: the name of a command, such as 'line."
  (cond ((null command)
         (format t "Numbers are millimetres, angles are degrees, (rad x) is x radians. Any Lisp works here too.~%")
         (format t "(help 'name) says more about one command.~%")
         (dolist (topic *help-topics*)
           (format t "~a: ~{~(~a~)~^ ~}~%" (first topic) (rest topic))))
        ((and (symbolp command) (or (fboundp command) (macro-function command))
              (eq (symbol-package command) (find-package :borba-cad/script)))
         (format t "~(~a~) ~(~a~)~%~a~%"
                 command
                 (sbcl:function-lambda-list (or (macro-function command) (fdefinition command)))
                 (documentation command 'function)))
        (t
         (format t "There is no command ~s; (help) lists them.~%" command)))
  nil)
