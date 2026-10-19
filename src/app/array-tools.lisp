(in-package #:borba-cad/app)

;;;; Arrays in the application: copies of the selection in a grid, round a point or along a path. The kind is chosen in
;;;; a prompt; a grid needs nothing else, a polar array a click for its center and a path array a click on its path, and
;;;; each ends in a form that asks the numbers, shows how many entities it would add and keeps the values for the next
;;;; array. The result is one step of undo and the originals and their copies are selected.

(defconstant +default-array-items+ 3
  "How many rows, columns or items an array asks for until another number is typed.")

(defconstant +default-array-step-ratio+ 1.25d0
  "How much larger than the selection the first spacing of an array is, so that the copies do not touch.")

(defconstant +fallback-array-step+ 100d0
  "The first spacing of an array of a selection that has no size, in millimetres.")

(defconstant +maximum-array-items+ 100000
  "Most rows, columns or items a form accepts; the limit on entities is the one that matters.")

(defconstant +full-turn-degrees+ 360d0
  "The angle a polar array fills until another is typed.")

(defparameter *path-entity-kinds* '(:line :polyline :arc :circle :ellipse :rectangle :polygon)
  "The kinds of entity that a path array can follow.")

;;; Reading the form

(defun form-whole-number (values
                          key
                          minimum)
  "Return a typed whole number, or signal INVALID-INPUT.

VALUES: the alist of the form.
KEY: the key of the line.
MINIMUM: the least it may be."
  (let* ((text (form-value values key))
         (number (base:parse-decimal text)))
    (unless (and (= number (round number)) (<= minimum number +maximum-array-items+))
      (error 'base:invalid-input
             :text text
             :reason (i18n:trf "a whole number from ~d to ~d"
                               minimum
                               +maximum-array-items+)))
    (round number)))

(defun form-length (app
                    values
                    key)
  "Return a typed length in millimetres, in the unit of the drawing when none is typed, or signal INVALID-INPUT.

APP: the application.
VALUES: the alist of the form.
KEY: the key of the line."
  (base:parse-length (form-value values key)
                     (display-unit-of app)))

(defun form-degrees (values
                     key)
  "Return a typed angle in radians, or signal INVALID-INPUT.

VALUES: the alist of the form.
KEY: the key of the line."
  (base:degrees->radians (base:parse-decimal (form-value values key))))

;;; The question every array ends in

(defun editable-selection-count (app)
  "Return how many of the selected entities an array can repeat.

APP: the application."
  (length (dom:existing-editable-entities (app-drawing app) (selection-ids app))))

(defun array-info (app
                   items)
  "Return the line that tells what an array of ITEMS items would add, and true as a second value when that is too many.

APP: the application.
ITEMS: how many items the array has, the originals included."
  (let ((added (* (editable-selection-count app) (1- items))))
    (if (> added dom:+maximum-array-entities+)
        (values (i18n:trf "That would add ~d entities; an array may add at most ~d."
                          added
                          dom:+maximum-array-entities+)
                t)
        (values (i18n:trf "Adds ~d ~:*~[entities~;entity~:;entities~]." added)
                nil))))

(defun commit-array (app
                     changes-function
                     values)
  "Make an array: commit the changes that CHANGES-FUNCTION works out, select the originals and the copies, and keep the
values of the form. Returns NIL when it was done, or the sentence that says why it could not be, so that the form that
asked stays open.

APP: the application.
CHANGES-FUNCTION: function of no arguments that returns the change set and how many entities were skipped.
VALUES: the alist of the form."
  (handler-case
      (multiple-value-bind (changes skipped) (funcall changes-function)
        (let ((originals (selection-ids app)))
          (cond ((commit-changes app
                                 changes
                                 "entity.array")
                 (remember-form-settings app values)
                 (select-ids app (append originals (result-polyline-ids changes)))
                 (operation-set-tool app :select)
                 (set-message app
                              (i18n:trf "Added ~d ~:*~[entities~;entity~:;entities~].~a"
                                        (length changes)
                                        (skipped-note skipped)))
                 nil)
                (t (i18n:tr "The array could not be made.")))))
    (base:borba-cad-error (condition)
      (remove #\Newline (princ-to-string condition)))))

(defun first-array-step (app)
  "Return the spacing that an array starts with: a little more than the largest side of the selection.

APP: the application."
  (let ((entities (selected-entities app)))
    (if (null entities)
        +fallback-array-step+
        (let* ((bounds (reduce #'geo:bounds-union (mapcar #'dom:entity-bounds entities)))
               (side (max (geo:bounds-width bounds) (geo:bounds-height bounds))))
          (if (base:positive-length-p side)
              (* side +default-array-step-ratio+)
              +fallback-array-step+)))))

;;; Grid

(defun rectangular-array-items (values)
  "Return how many items a grid form asks for.

VALUES: the alist of the form."
  (* (form-whole-number values
                        :rows
                        1)
     (form-whole-number values
                        :columns
                        1)))

(defun make-rectangular-array (app
                               values)
  "Make the grid that a form describes.

APP: the application.
VALUES: the alist of the form."
  (commit-array app
                (lambda ()
                  (dom:rectangular-array-changes (app-drawing app)
                                                 (selection-ids app)
                                                 (form-whole-number values
                                                                    :rows
                                                                    1)
                                                 (form-whole-number values
                                                                    :columns
                                                                    1)
                                                 (form-length app
                                                              values
                                                              :row-spacing)
                                                 (form-length app
                                                              values
                                                              :column-spacing)
                                                 :angle (form-degrees values :grid-angle)))
                values))

(defun ask-rectangular-array (app)
  "Ask the numbers of a grid of copies of the selection.

APP: the application."
  (let ((step (length-text app (first-array-step app))))
    (open-form-prompt app
                      (i18n:tr "Array: grid")
                      (i18n:tr "Rows go up from the original, columns to its right. A negative spacing reverses.")
                      (list (list :rows
                                  (i18n:tr "Rows")
                                  (form-setting app
                                                 :rows
                                                 (princ-to-string +default-array-items+)))
                            (list :columns
                                  (i18n:tr "Columns")
                                  (form-setting app
                                                 :columns
                                                 (princ-to-string +default-array-items+)))
                            (list :row-spacing
                                  (i18n:tr "Spacing of the rows")
                                  (form-setting app
                                                 :row-spacing
                                                 step))
                            (list :column-spacing
                                  (i18n:tr "Spacing of the columns")
                                  (form-setting app
                                                 :column-spacing
                                                 step))
                            (list :grid-angle
                                  (i18n:tr "Angle of the columns (degrees)")
                                  (form-setting app
                                                 :grid-angle
                                                 "0")))
                      (lambda (values) (make-rectangular-array app values))
                      :info (lambda (values)
                              (array-info app (rectangular-array-items values))))))

;;; Round a point

(defclass array-center-tool (tool) ()
  (:documentation "Waits for the click that says the center of a polar array, and then asks the rest in a form."))

(defmethod tool-kind ((tool array-center-tool)) :array-center)

(defmethod tool-prompt ((tool array-center-tool) app)
  (declare (ignore app))
  (i18n:tr "Polar array: click the center the copies go round (Esc cancels)"))

(defun make-polar-array (app
                         center-x
                         center-y
                         values)
  "Make the polar array that a form describes.

APP: the application.
CENTER-X: x of the center.
CENTER-Y: y of the center.
VALUES: the alist of the form."
  (commit-array app
                (lambda ()
                  (dom:polar-array-changes (app-drawing app)
                                           (selection-ids app)
                                           center-x
                                           center-y
                                           (form-whole-number values
                                                              :items
                                                              2)
                                           (form-degrees values :sweep)
                                           :rotate-items (eq (form-value values :turn) :turn)))
                values))

(defun ask-polar-array (app
                        center-x
                        center-y)
  "Ask the numbers of a polar array of the selection.

APP: the application.
CENTER-X: x of the center.
CENTER-Y: y of the center."
  (open-form-prompt app
                    (i18n:tr "Array: round a point")
                    (i18n:trf "Center at ~a; ~a. A full turn, 360, spreads the items round the whole circle."
                              (length-text app center-x)
                              (length-text app center-y))
                    (list (list :items
                                (i18n:tr "Items, the original included")
                                (form-setting app
                                               :items
                                               "6"))
                          (list :sweep
                                (i18n:tr "Angle to fill (degrees)")
                                (form-setting app
                                               :sweep
                                               (base:format-decimal +full-turn-degrees+ 0)))
                          (list :turn
                                (i18n:tr "Copies")
                                (form-setting app
                                               :turn
                                               :turn)
                                (list (cons :turn (i18n:tr "Turn as they go round"))
                                      (cons :keep (i18n:tr "Stay as drawn")))))
                    (lambda (values) (make-polar-array app
                                                       center-x
                                                       center-y
                                                       values))
                    :info (lambda (values)
                            (array-info app (form-whole-number values
                                                               :items
                                                               2)))))

(defmethod tool-pointer-down ((tool array-center-tool) app cursor)
  (ask-polar-array app
                   (cursor-x cursor)
                   (cursor-y cursor)))

;;; Along a path

(defclass array-path-tool (tool) ()
  (:documentation "Waits for the click on the line, polyline, curve or shape that the copies follow, and then asks the
rest in a form."))

(defmethod tool-kind ((tool array-path-tool)) :array-path)

(defmethod tool-prompt ((tool array-path-tool) app)
  (declare (ignore app))
  (i18n:tr "Path array: click the line, curve or shape the copies follow (Esc cancels)"))

(defun path-under (app
                   cursor)
  "Return the entity that a click at the pointer would take as the path, or NIL.

APP: the application.
CURSOR: the pointer position."
  (dom:pick-entity (app-drawing app)
                   (cursor-raw-x cursor)
                   (cursor-raw-y cursor)
                   (pick-radius-in-world app)
                   *path-entity-kinds*))

(defun path-entity (app
                    path-id)
  "Return the entity with an id, which a path array follows.

APP: the application.
PATH-ID: id of the path."
  (dom:store-find (dom:drawing-store (app-drawing app)) path-id))

(defun path-array-items (app
                         path-id
                         values)
  "Return how many items a path form asks for, the originals included.

APP: the application.
PATH-ID: id of the path.
VALUES: the alist of the form."
  (if (eq (form-value values :measure) :divide)
      (form-whole-number values
                         :path-items
                         2)
      (multiple-value-bind (coordinates closed) (dom:entity-path (path-entity app path-id))
        (length (geo:path-spaced-distances (geo:path-length coordinates closed)
                                           closed
                                           (form-length app
                                                        values
                                                        :path-spacing))))))

(defun make-path-array (app
                        path-id
                        values)
  "Make the path array that a form describes.

APP: the application.
PATH-ID: id of the path.
VALUES: the alist of the form."
  (let ((divide (eq (form-value values :measure) :divide)))
    (commit-array app
                  (lambda ()
                    (dom:path-array-changes (app-drawing app)
                                            (selection-ids app)
                                            path-id
                                            :count (and divide
                                                        (form-whole-number values
                                                                           :path-items
                                                                           2))
                                            :spacing (and (not divide)
                                                          (form-length app
                                                                       values
                                                                       :path-spacing))
                                            :align (eq (form-value values :align) :follow)))
                  values)))

(defun ask-path-array (app
                       path-id)
  "Ask the numbers of an array of the selection along a path.

APP: the application.
PATH-ID: id of the path."
  (multiple-value-bind (coordinates closed) (dom:entity-path (path-entity app path-id))
    (open-form-prompt app
                      (i18n:tr "Array: along a path")
                      (i18n:trf "The path is ~a long. The originals are the first item, where the path starts."
                                (length-text app (geo:path-length coordinates closed)))
                      (list (list :measure
                                  (i18n:tr "Items")
                                  (form-setting app
                                                 :measure
                                                 :divide)
                                  (list (cons :divide (i18n:tr "A number, at equal steps"))
                                        (cons :measure (i18n:tr "A spacing, as many as fit"))))
                            (list :path-items
                                  (i18n:tr "Number of items")
                                  (form-setting app
                                                 :path-items
                                                 "6"))
                            (list :path-spacing
                                  (i18n:tr "Spacing")
                                  (form-setting app
                                                 :path-spacing
                                                 (length-text app (first-array-step app))))
                            (list :align
                                  (i18n:tr "Copies")
                                  (form-setting app
                                                 :align
                                                 :follow)
                                  (list (cons :follow (i18n:tr "Turn to follow the path"))
                                        (cons :keep (i18n:tr "Stay as drawn")))))
                      (lambda (values) (make-path-array app
                                                        path-id
                                                        values))
                      :info (lambda (values)
                              (array-info app (path-array-items app
                                                                path-id
                                                                values))))))

(defmethod tool-pointer-down ((tool array-path-tool) app cursor)
  (let ((path (path-under app cursor)))
    (if path
        (ask-path-array app (dom:entity-id path))
        (set-message app
                     (i18n:tr "Click a line, polyline, arc, circle, ellipse, rectangle or polygon.")
                     :error))))

(defmethod tool-preview-entities ((tool array-path-tool) app)
  (let* ((cursor (app-cursor app))
         (path (and cursor (path-under app cursor))))
    (and path (list path))))

;;; Choosing the kind

(defun operation-array (app)
  "Repeat the selection: ask whether in a grid, round a point or along a path, and go on from there.

APP: the application."
  (if (zerop (editable-selection-count app))
      (set-message app
                   (i18n:tr "Select what to repeat first, on a layer that is not locked.")
                   :error)
      (open-choice-prompt app
                          (i18n:tr "Array")
                          (i18n:tr "Repeat the selection in a grid, round a point or along a path.")
                          (list (cons (i18n:tr "Grid")
                                      (lambda () (ask-rectangular-array app)))
                                (cons (i18n:tr "Round a point")
                                      (lambda () (activate-tool app (make-instance 'array-center-tool))))
                                (cons (i18n:tr "Along a path")
                                      (lambda () (activate-tool app (make-instance 'array-path-tool))))
                                (cons (i18n:tr "Cancel") nil)))))
