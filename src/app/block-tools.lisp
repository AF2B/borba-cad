(in-package #:borba-cad/app)

;;;; Blocks in the application: making one of the selection, placing one, and exploding a placement. The names are
;;;; asked in a prompt; making and placing then wait for a click, in tools that are made with the name they were given,
;;;; so they are not among the tools that make-tool makes from a keyword.

(defconstant +block-names-listed+ 8
  "Most block names the question about which block to place lists.")

(defun activate-tool (app
                      tool)
  "Make a tool that has already been made the active one, cancelling whatever the previous one was doing.

APP: the application.
TOOL: the tool."
  (when (app-tool app)
    (tool-cancel (app-tool app) app))
  (setf (app-tool app) tool
        (app-interaction app) nil
        (app-translation app) nil
        (app-window app) nil
        (app-window-crossing app) nil)
  (app-request-redraw app))

(defun block-names-text (app)
  "Return the names of the blocks of the drawing as a sentence fragment, the first few, with an ellipsis when there are
more.

APP: the application."
  (let ((names (append (dom:block-names (app-drawing app))
                       (mapcar #'script:block-generator-name (block-generators app)))))
    (format nil "~{~a~^, ~}~:[~;, ...~]"
            (subseq names 0 (min +block-names-listed+ (length names)))
            (> (length names) +block-names-listed+))))

;;; Making a block

(defclass make-block-tool (tool)
  ((block-name :initarg :block-name :reader make-block-name)
   (ids :initarg :ids :reader make-block-ids))
  (:documentation "Waits for the click that says where the base point of the block is, and then makes the block of the
entities that were selected, which are replaced by one placement of it."))

(defmethod tool-kind ((tool make-block-tool)) :make-block)

(defmethod tool-prompt ((tool make-block-tool) app)
  (declare (ignore app))
  (i18n:trf "Block ~a: click its base point, the point it is placed by (Esc cancels)"
            (make-block-name tool)))

(defmethod tool-pointer-down ((tool make-block-tool) app cursor)
  (let ((drawing (app-drawing app)))
    (handler-case
        (multiple-value-bind (definition changes skipped)
            (dom:make-block-changes drawing
                                    (make-block-ids tool)
                                    (make-block-name tool)
                                    (cursor-x cursor)
                                    (cursor-y cursor))
          (declare (ignore skipped))
          (dom:define-block drawing definition)
          (when (commit-changes app
                                changes
                                "block.make")
            (select-ids app (list (dom:entity-id (dom:change-after (car (last changes))))))
            (set-message app
                         (i18n:trf "Made the block ~a of ~d ~:*~[entities~;entity~:;entities~]."
                                   (dom:block-definition-name definition)
                                   (length (dom:block-definition-entities definition))))
            (setf (app-last-block-name app) (dom:block-definition-name definition))
            (operation-set-tool app :select)))
      (base:borba-cad-error (condition)
        (report-error app
                      condition
                      "block")))))

(defun operation-make-block (app)
  "Make a block of the selection: ask its name, then wait for the click that gives its base point.

APP: the application."
  (let ((ids (selection-ids app)))
    (if (null ids)
        (set-message app
                     (i18n:tr "Select what the block is made of first.")
                     :error)
        (open-text-prompt app
                          (i18n:tr "Make block")
                          (i18n:tr "Type the name of the block. Then click its base point.")
                          ""
                          (lambda (text)
                            (handler-case
                                (activate-tool app
                                               (make-instance 'make-block-tool
                                                              :block-name (dom:clean-block-name text)
                                                              :ids ids))
                              (base:edit-refused (condition)
                                (report-error app
                                              condition
                                              "block"))))))))

;;; Placing a block

(defclass insert-tool (tool)
  ((block-name :initarg :block-name :reader insert-block-name)
   (point :initform nil :accessor insert-point)
   (scale :initform 1d0 :accessor insert-scale-factor)
   (flipped :initform nil :accessor insert-flipped))
  (:documentation "Places a block: a click for the point its base point goes to, then a click that turns it toward the
pointer, or Enter to leave it as it is. A number typed before the first click is the scale, and one typed after it is
the turn in degrees. F flips it across its vertical axis."))

(defmethod tool-kind ((tool insert-tool)) :insert)

(defmethod tool-prompt ((tool insert-tool) app)
  (declare (ignore app))
  (if (insert-point tool)
      (i18n:trf "Block ~a: click to turn it, or Enter to leave it straight; type the angle in degrees"
                (insert-block-name tool))
      (i18n:trf "Block ~a: click where to place it, or type the scale (now ~a)"
                (insert-block-name tool)
                (base:format-decimal (insert-scale-factor tool) 4))))

(defun placement-at (tool
                     app
                     point
                     rotation)
  "Return the placement of the block of an insert tool at a point, turned by ROTATION.

TOOL: the insert tool.
APP: the application.
POINT: an (x . y) cons, where the base point of the block goes.
ROTATION: the turn in radians."
  (dom:make-insert :id (creation-id app)
                   :layer-id (creation-layer-id app)
                   :block (dom:find-block (app-drawing app) (insert-block-name tool))
                   :x (car point)
                   :y (cdr point)
                   :scale (insert-scale-factor tool)
                   :rotation rotation
                   :flipped (insert-flipped tool)))

(defun place-block (tool
                    app
                    rotation)
  "Place the block of an insert tool at its point, turned by ROTATION, and ready the tool for the next placement.

TOOL: the insert tool with its point set.
APP: the application.
ROTATION: the turn in radians."
  (when (create-entity app
                       (lambda ()
                         (placement-at tool
                                       app
                                       (insert-point tool)
                                       rotation)))
    (setf (app-last-block-name app) (insert-block-name tool)))
  (setf (insert-point tool) nil))

(defmethod tool-pointer-down ((tool insert-tool) app cursor)
  (cond ((null (insert-point tool))
         (setf (insert-point tool) (point-key (cursor-x cursor) (cursor-y cursor))))
        (t
         (let ((rotation (angle-from-pivot (insert-point tool)
                                           (cursor-x cursor)
                                           (cursor-y cursor))))
           (place-block tool
                        app
                        (or rotation 0d0)))))
  (app-request-redraw app))

(defmethod tool-pointer-move ((tool insert-tool) app cursor)
  (declare (ignore cursor))
  (app-request-redraw app))

(defmethod tool-key ((tool insert-tool) app key modifiers)
  (declare (ignore modifiers))
  (cond ((and (eq key :enter) (insert-point tool))
         (place-block tool
                      app
                      0d0)
         (app-request-redraw app)
         t)
        ((eq key :f)
         (setf (insert-flipped tool) (not (insert-flipped tool)))
         (app-request-redraw app)
         t)))

(defmethod tool-typed-number ((tool insert-tool) app number)
  (cond ((insert-point tool)
         (place-block tool
                      app
                      (base:degrees->radians (float number 1d0))))
        ((and (plusp number) (< (/ dom:+maximum-insert-scale+) number dom:+maximum-insert-scale+))
         (setf (insert-scale-factor tool) (float number 1d0))
         (app-request-redraw app))
        (t (set-message app
                        (i18n:tr "The scale must be above zero.")
                        :error)))
  t)

(defmethod tool-preview-entities ((tool insert-tool) app)
  (let* ((cursor (app-cursor app))
         (point (insert-point tool))
         (here (and cursor (point-key (cursor-x cursor) (cursor-y cursor))))
         (placed (and here
                      (try-entity (lambda ()
                                    (placement-at tool
                                                  app
                                                  (or point here)
                                                  (or (and point
                                                           (angle-from-pivot point
                                                                             (car here)
                                                                             (cdr here)))
                                                      0d0)))))))
    (and placed (list placed))))

(defmethod tool-cancel ((tool insert-tool) app)
  (setf (insert-point tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool insert-tool))
  (and (insert-point tool) t))

(defmethod tool-anchor ((tool insert-tool))
  (insert-point tool))

(defun start-insert-tool (app
                          name)
  "Make the block called NAME the one the insert tool places, and activate the tool.

APP: the application.
NAME: the name of a block of the drawing."
  (activate-tool app
                 (make-instance 'insert-tool :block-name (dom:block-definition-name
                                                          (dom:find-block (app-drawing app) name)))))

(defun ask-for-block-to-insert (app
                                &optional problem)
  "Ask which block to place; a name that is not a block is said and the question asked again.

APP: the application.
PROBLEM: what was wrong with the last answer, or NIL."
  (let ((instructions (i18n:trf "Blocks: ~a. Type the name of the one to place." (block-names-text app))))
    (open-text-prompt app
                      (i18n:tr "Insert block")
                      (if problem
                          (format nil "~a ~a" problem instructions)
                          instructions)
                      (or (and (app-last-block-name app)
                               (dom:find-block (app-drawing app) (app-last-block-name app))
                               (app-last-block-name app))
                          (first (dom:block-names (app-drawing app)))
                          (script:block-generator-name (first (block-generators app)))
                          "")
                      (lambda (text)
                        (let ((name (string-trim " " text)))
                          (cond ((dom:find-block (app-drawing app) name)
                                 (start-insert-tool app name))
                                ((find-block-generator-in app name)
                                 (operation-insert-parametric-block app name))
                                (t
                                 (ask-for-block-to-insert app
                                                          (i18n:trf "There is no block called ~a." name)))))))))

(defun operation-insert-block (app)
  "Place a block: ask which, then wait for the clicks that say where and how it is turned.

APP: the application."
  (if (and (null (dom:block-names (app-drawing app))) (null (block-generators app)))
      (set-message app
                   (i18n:tr "There are no blocks yet: select entities and choose Make block.")
                   :error)
      (ask-for-block-to-insert app)))

;;; Exploding

(defun operation-explode-selection (app)
  "Replace the selected blocks, rectangles, polygons and polylines by the entities they come apart into: the entities a
block stands for, and the sides of a shape.

APP: the application."
  (cond ((zerop (hash-table-count (app-selection app)))
         (set-message app
                      (i18n:tr "Select what you want to explode first.")
                      :error))
        (t
         (multiple-value-bind (changes exploded) (dom:explode-changes (app-drawing app) (selection-ids app))
           (cond ((zerop exploded)
                  (set-message app
                               (i18n:tr "That cannot be exploded: use a block, rectangle, polygon or polyline.")
                               :error))
                 ((commit-changes app
                                  changes
                                  "entity.explode")
                  (select-ids app
                              (loop for change in changes
                                    when (null (dom:change-before change))
                                      collect (dom:entity-id (dom:change-after change))))
                  (set-message app
                               (i18n:trf "Exploded ~d ~:*~[entities~;entity~:;entities~]." exploded))))))))
