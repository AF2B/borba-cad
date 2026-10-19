(in-package #:borba-cad/shapes)

;;;; The catalog: the shapes that come with the program, in areas and groups, and how a shape becomes a block that a
;;;; drawing places. The acanthus leaves are the first group, in the area of ironwork.

(defparameter *acanthus-recipes*
  '((:leaf :lobes 3 :width 34)
    (:leaf :lobes 4 :width 36 :veins t)
    (:leaf :lobes 5 :width 38 :veins t)
    (:leaf :lobes 6 :width 26)
    (:leaf :lobes 7 :width 24 :veins t)
    (:leaf :lobes 3 :width 50 :veins t)
    (:leaf :lobes 4 :width 46)
    (:leaf :lobes 4 :width 36 :veins t :sweep -22 :left-width 30 :left-lowest 4 :left-highest 44)
    (:leaf :lobes 5 :width 36 :veins t :sweep 26)
    (:leaf :lobes 4 :width 34 :sweep 14)
    (:leaf :lobes 6 :width 28 :sweep -14)
    (:leaf :lobes 4 :width 40 :lowest 22 :highest 60)
    (:leaf :lobes 5 :width 42 :lowest 5 :highest 40)
    (:scroll :lobes 4 :width 34 :veins t :side 1)
    (:scroll :lobes 5 :width 27 :side -1)
    (:scroll :lobes 3 :width 32 :veins t :side 1)
    (:scroll :lobes 4 :width 34 :side -1)
    (:pair :lobes 4 :width 20 :veins t :spread 36)
    (:pair :lobes 3 :width 22 :spread 40)
    (:pair :lobes 5 :width 18 :veins t :spread 30))
  "How each acanthus leaf of the catalog is made, in the order they are shown: a single leaf, a leaf on a stem that
ends in a volute, or two leaves that lean away from each other, with how many lobes, how wide, which way they sweep
and whether the veins are drawn.")

(defun recipe-side (recipe
                    &key left)
  "Return the edge of a leaf that a recipe asks for, the right one or, for the left, the one that the recipe says is
different from the right.

RECIPE: a list that begins with the kind of the shape and goes on with keywords.
LEFT: true for the left edge."
  (flet ((value (key default &optional left-key)
           (or (and left left-key (getf (rest recipe) left-key))
               (getf (rest recipe) key)
               default)))
    (make-side :lobes (getf (rest recipe) :lobes)
               :width (float (value :width 36 :left-width) 1d0)
               :depth 0.34d0
               :lowest (float (value :lowest 12 :left-lowest) 1d0)
               :highest (float (value :highest 56 :left-highest) 1d0)
               :belly 0.14d0
               :scoop 0.10d0)))

(defun recipe-contours (recipe)
  "Return the contours of the shape that a recipe makes, 100 units high with its base at the origin.

RECIPE: a list that begins with the kind of the shape (:leaf, :scroll or :pair) and goes on with keywords."
  (let ((right (recipe-side recipe))
        (left (recipe-side recipe :left t))
        (veins (getf (rest recipe) :veins)))
    (fit-contours
     (ecase (first recipe)
       (:leaf (leaf-contours right
                             left
                             :veins veins
                             :sweep (float (getf (rest recipe) :sweep 0) 1d0)))
       (:scroll (scroll-leaf-contours right
                                      left
                                      :veins veins
                                      :side (getf (rest recipe) :side 1)))
       (:pair (leaf-pair-contours right
                                  left
                                  :veins veins
                                  :spread (float (getf (rest recipe) :spread 30) 1d0)))))))

(defun acanthus-shapes ()
  "Return the acanthus leaves of the catalog, as shapes."
  (loop for recipe in *acanthus-recipes*
        for number from 1
        collect (make-shape (format nil "acanthus-~2,'0d" number)
                            (i18n:mark "Acanthus leaf ~d")
                            (recipe-contours recipe)
                            number)))

(defvar *built-in-areas* nil
  "The areas that come with the program, made the first time they are asked for.")

(defun built-in-areas ()
  "Return the areas that come with the program: ironwork, with its acanthus leaves."
  (or *built-in-areas*
      (setf *built-in-areas*
            (list (make-area "ironwork"
                             (i18n:mark "Ironwork")
                             (list (make-group "acanthus"
                                               (i18n:mark "Acanthus leaves")
                                               (acanthus-shapes))))))))

(defun all-areas (&optional user-areas)
  "Return the areas of the catalog: those that come with the program and then those the user added, with the shapes of
an area of the same name added to it.

USER-AREAS: areas read from the user's folder of shapes, or NIL."
  (let ((areas (mapcar #'copy-area (built-in-areas))))
    (dolist (user user-areas)
      (let ((existing (find (area-id user) areas :key #'area-id :test #'string=)))
        (if existing
            (setf (area-groups existing) (append (area-groups existing) (area-groups user)))
            (setf areas (append areas (list user))))))
    areas))

(defun find-shape (areas
                   area-id
                   group-id
                   shape-id)
  "Return the shape with an id, in the group and area with theirs, or NIL.

AREAS: a list of areas.
AREA-ID: the id of the area.
GROUP-ID: the id of the group.
SHAPE-ID: the id of the shape."
  (let* ((area (find area-id areas :key #'area-id :test #'string=))
         (group (and area (find group-id (area-groups area) :key #'group-id :test #'string=))))
    (and group (find shape-id (group-shapes group) :key #'shape-id :test #'string=))))

;;; Making a block of a shape

(defconstant +duplicate-point-distance+ 1d-4
  "How close two points of a contour may be before they count as one, in the units of the shape.")

(defun distinct-points (points
                        closed)
  "Return the points with each that is as good as on top of the one before it left out, and, for a closed contour, the
last when it is as good as on top of the first.

POINTS: a list of conses of x and y.
CLOSED: true for a closed contour."
  (let ((kept '()))
    (dolist (point points)
      (when (or (null kept)
                (> (point-distance point (first kept)) +duplicate-point-distance+))
        (push point kept)))
    (when (and closed
               (rest kept)
               (<= (point-distance (first kept) (car (last kept))) +duplicate-point-distance+))
      (pop kept))
    (nreverse kept)))

(defun shape-block-name (shape)
  "Return the name of the block that a shape is placed as: the English name of the shape, which does not change with the
language of the interface.

SHAPE: a shape."
  (if (shape-name-argument shape)
      (format nil (shape-name shape) (shape-name-argument shape))
      (shape-name shape)))

(defun shape-block-definition (shape)
  "Return the definition of the block that a shape is placed as: a polyline for each contour of the shape, on the
default layer, with the base point at the origin. Contours with fewer than two points are left out.

SHAPE: a shape."
  (let ((entities '())
        (id 0))
    (dolist (contour (shape-contours shape))
      (let ((points (distinct-points (contour-points contour) (contour-closed contour))))
        (when (>= (length points) (if (contour-closed contour) 3 2))
          (push (dom:make-polyline :id (incf id)
                                   :layer-id dom:+default-layer-id+
                                   :coordinates (coerce (loop for point in points
                                                              append (list (float (car point) 1d0)
                                                                           (float (cdr point) 1d0)))
                                                        '(simple-array double-float (*)))
                                   :closed (contour-closed contour))
                entities))))
    (dom:make-block-definition :name (shape-block-name shape)
                               :entities (nreverse entities))))
