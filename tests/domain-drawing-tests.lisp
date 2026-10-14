(in-package #:borba-cad/tests)

(def-suite domain-drawing
  :description "Entity store, drawing, layers, change sets, history, picking and snapping."
  :in all)

(in-suite domain-drawing)

;;; Helpers

(defun commit-entities (drawing &rest entities)
  "Add ENTITIES to DRAWING through a change set."
  (dom:apply-changes drawing (dom:create-entities-changes entities)))

(defun entity-ids (drawing)
  "Return the ids of every entity of DRAWING in stacking order."
  (let ((ids '()))
    (dom:map-entities (lambda (entity) (push (dom:entity-id entity) ids)) (dom:drawing-store drawing))
    (nreverse ids)))

(defun drawing-entities (drawing)
  "Return the entities of DRAWING as a list in stacking order."
  (let ((entities '()))
    (dom:map-entities (lambda (entity) (push entity entities)) (dom:drawing-store drawing))
    (nreverse entities)))

(defun same-entities-p (first second)
  "True when two entity lists hold the very same entity objects in the same order."
  (and (= (length first) (length second))
       (every #'eq first second)))

(defun id-line (id)
  "Return a valid line with the given id whose geometry depends on the id."
  (a-line :id id :x1 (* 10d0 id) :x2 (+ 1000d0 id)))

(defmacro signals-layer-error (&body body)
  "Check that BODY signals LAYER-ERROR with a readable message."
  `(handler-case (progn ,@body (fail "expected LAYER-ERROR, but no error was signaled"))
     (base:layer-error (condition)
       (is (plusp (length (princ-to-string condition)))))))

;;; Entity store

(test store-keeps-entities-ordered-by-id-regardless-of-insertion-order
  (let ((store (dom:make-entity-store)))
    (dolist (id '(5 1 9 3 7))
      (dom:store-add store (id-line id)))
    (let ((ids '()))
      (dom:map-entities (lambda (entity) (push (dom:entity-id entity) ids)) store)
      (is (equal '(1 3 5 7 9) (nreverse ids))))
    (is (= 5 (dom:store-count store)))
    (is (= 9 (dom:entity-id (dom:store-find store 9))))
    (is (null (dom:store-find store 4)))))

(test store-rejects-duplicate-ids-and-missing-removals
  (let ((store (dom:make-entity-store)))
    (dom:store-add store (id-line 1))
    (signals error (dom:store-add store (id-line 1)))
    (signals error (dom:store-remove store 2))
    (signals error (dom:store-replace store (id-line 2)))
    (is (= 1 (dom:store-count store)))))

(test store-grows-past-its-initial-capacity
  (let ((store (dom:make-entity-store)))
    (loop for id from 1 to 1000 do (dom:store-add store (id-line id)))
    (is (= 1000 (dom:store-count store)))
    (is (= 500 (dom:entity-id (dom:store-entity-at store 499))))))

(test store-bulk-operations-are-atomic-on-invalid-input
  (let ((store (dom:make-entity-store)))
    (dom:store-add-many store (list (id-line 1) (id-line 2) (id-line 3)))
    (signals error (dom:store-remove-many store '(1 99)))
    (is (= 3 (dom:store-count store)))                       ; nothing was removed
    (signals error (dom:store-add-many store (list (id-line 4) (id-line 2))))
    (signals error (dom:store-add-many store (list (id-line 5) (id-line 5))))
    (is (= 3 (dom:store-count store)))))

(test store-matches-a-reference-model-under-random-operations
  (let ((prng (rng:make-prng 5))
        (store (dom:make-entity-store))
        (model (make-hash-table)))
    (labels ((unused-id ()
               (loop for id = (1+ (rng:prng-next-below prng 300))
                     unless (gethash id model) return id))
             (existing-id ()
               (let ((ids (sort (loop for id being the hash-keys of model collect id) #'<)))
                 (and ids (rng:prng-pick prng ids))))
             (check-agreement ()
               (let ((expected (sort (loop for id being the hash-keys of model collect id) #'<))
                     (actual '()))
                 (dom:map-entities (lambda (entity) (push (dom:entity-id entity) actual)) store)
                 (is (equal expected (nreverse actual)))
                 (is (= (length expected) (dom:store-count store)))
                 (dolist (id expected)
                   (is (eq (gethash id model) (dom:store-find store id)))))))
      (dotimes (step 600)
        (case (rng:prng-next-below prng 5)
          (0 (when (< (hash-table-count model) 290)
               (let ((entity (id-line (unused-id))))
                 (dom:store-add store entity)
                 (setf (gethash (dom:entity-id entity) model) entity))))
          (1 (let ((id (existing-id)))
               (when id
                 (dom:store-remove store id)
                 (remhash id model))))
          (2 (let ((id (existing-id)))
               (when id
                 (let ((entity (a-line :id id :x2 (+ 2000d0 step))))
                   (dom:store-replace store entity)
                   (setf (gethash id model) entity)))))
          (3 (when (< (hash-table-count model) 280)
               (let ((entities '()))
                 (loop repeat 3
                       do (let ((id (loop for candidate = (unused-id)
                                          unless (find candidate entities :key #'dom:entity-id)
                                            return candidate)))
                            (push (id-line id) entities)))
                 (dom:store-add-many store entities)
                 (dolist (entity entities)
                   (setf (gethash (dom:entity-id entity) model) entity)))))
          (4 (let ((ids (remove-duplicates (loop repeat 3 for id = (existing-id) when id collect id))))
               (when ids
                 (dom:store-remove-many store ids)
                 (dolist (id ids) (remhash id model))))))
        (check-agreement)))))

;;; Drawing and layers

(test new-drawing-defaults
  (let ((drawing (dom:make-drawing)))
    (is (= 0 (dom:drawing-entity-count drawing)))
    (is (= 1 (length (dom:layers-in-order drawing))))
    (let ((layer (dom:find-layer drawing dom:+default-layer-id+)))
      (is (string= "0" (dom:layer-name layer)))
      (is (dom:layer-visible layer))
      (is (not (dom:layer-locked layer))))
    (is (= dom:+default-layer-id+ (dom:drawing-current-layer-id drawing)))
    (is (eq :millimeter (dom:drawing-unit drawing)))
    (is (= dom:+default-grid-spacing+ (dom:drawing-grid-spacing drawing)))
    (is (dom:drawing-snap-to-grid drawing))
    (is (dom:drawing-object-snap drawing))
    (is (= 1 (dom:drawing-next-entity-id drawing)))
    (is (= 0 (dom:drawing-revision drawing)))))

(test layers-are-added-with-fresh-ids-and-unique-names
  (let* ((drawing (dom:make-drawing))
         (walls (dom:add-layer drawing "walls" (dom:make-rgb 255 0 0)))
         (doors (dom:add-layer drawing "  doors " (dom:make-rgb 0 255 0))))
    (is (= 1 (dom:layer-id walls)))
    (is (= 2 (dom:layer-id doors)))
    (is (string= "doors" (dom:layer-name doors)))                  ; trimmed
    (is (equal '("0" "walls" "doors") (mapcar #'dom:layer-name (dom:layers-in-order drawing))))
    (is (eq walls (dom:find-layer-by-name drawing "WALLS")))        ; case-insensitive lookup
    (signals-layer-error (dom:add-layer drawing "Walls" (dom:make-rgb 1 1 1)))
    (signals-layer-error (dom:add-layer drawing "   " (dom:make-rgb 1 1 1)))
    (signals-layer-error (dom:add-layer drawing (make-string 65 :initial-element #\a) (dom:make-rgb 1 1 1)))
    (is (= 3 (length (dom:layers-in-order drawing))))))

(test layer-edits-replace-the-layer-value-and-bump-the-revision
  (let* ((drawing (dom:make-drawing))
         (layer (dom:add-layer drawing "walls" (dom:make-rgb 255 0 0)))
         (id (dom:layer-id layer))
         (revision (dom:drawing-revision drawing)))
    (dom:rename-layer drawing id "Walls")                           ; same name, other case: allowed
    (is (string= "Walls" (dom:layer-name (dom:find-layer drawing id))))
    (signals-layer-error (dom:rename-layer drawing id "0"))         ; clashes with the default layer
    (dom:recolor-layer drawing id (dom:make-rgb 1 2 3))
    (is (= 2 (dom:rgb-green (dom:layer-color (dom:find-layer drawing id)))))
    (dom:set-layer-visible drawing id nil)
    (is (not (dom:layer-visible (dom:find-layer drawing id))))
    (dom:set-layer-locked drawing id t)
    (is (dom:layer-locked (dom:find-layer drawing id)))
    (is (> (dom:drawing-revision drawing) revision))
    (is (eq layer layer))                                           ; the old value is unchanged
    (is (dom:layer-visible layer))
    (signals-layer-error (dom:rename-layer drawing 99 "x"))))

(test current-layer-and-layer-removal-rules
  (let* ((drawing (dom:make-drawing))
         (walls (dom:layer-id (dom:add-layer drawing "walls" (dom:make-rgb 255 0 0)))))
    (dom:set-current-layer drawing walls)
    (is (= walls (dom:drawing-current-layer-id drawing)))
    (signals-layer-error (dom:set-current-layer drawing 42))
    (signals-layer-error (dom:remove-layer drawing dom:+default-layer-id+))
    (commit-entities drawing (a-line :id 1 :layer-id walls))
    (is (= 1 (dom:layer-entity-count drawing walls)))
    (signals-layer-error (dom:remove-layer drawing walls))          ; still holds an entity
    (dom:apply-changes drawing (dom:delete-entities-changes drawing '(1)))
    (dom:remove-layer drawing walls)
    (is (null (dom:find-layer drawing walls)))
    (is (= dom:+default-layer-id+ (dom:drawing-current-layer-id drawing)))   ; falls back
    (is (= 1 (length (dom:layers-in-order drawing))))
    (let ((again (dom:add-layer drawing "walls" (dom:make-rgb 0 0 0))))
      (is (/= walls (dom:layer-id again))))))                       ; ids are never reused

(test layer-removal-error-message-counts-entities
  (let* ((drawing (dom:make-drawing))
         (walls (dom:layer-id (dom:add-layer drawing "walls" (dom:make-rgb 255 0 0)))))
    (commit-entities drawing (a-line :id 1 :layer-id walls))
    (handler-case (dom:remove-layer drawing walls)
      (base:layer-error (condition)
        (is (search "1 entity" (princ-to-string condition)))))))

(test drawing-settings-validate-their-values
  (let ((drawing (dom:make-drawing)))
    (dom:set-grid-spacing drawing 250d0)
    (is (= 250d0 (dom:drawing-grid-spacing drawing)))
    (signals base:invalid-input (dom:set-grid-spacing drawing 0.5d0))
    (signals base:invalid-input (dom:set-grid-spacing drawing 5))
    (signals base:invalid-input (dom:set-grid-spacing drawing (* 2 base:+maximum-coordinate-magnitude+)))
    (dom:set-display-unit drawing :meter)
    (is (eq :meter (dom:drawing-display-unit drawing)))
    (signals type-error (dom:set-display-unit drawing :furlong))
    (dom:set-default-text-height drawing 120d0)
    (is (= 120d0 (dom:drawing-default-text-height drawing)))
    (signals base:invalid-input (dom:set-default-text-height drawing 0d0))
    (dom:set-grid-visible drawing nil)
    (dom:set-snap-to-grid drawing nil)
    (dom:set-object-snap drawing nil)
    (is (not (dom:drawing-grid-visible drawing)))
    (is (not (dom:drawing-snap-to-grid drawing)))
    (is (not (dom:drawing-object-snap drawing)))))

(test restore-operations-rebuild-a-drawing-from-explicit-values
  (let ((drawing (dom:make-drawing)))
    (dom:restore-layer drawing 0 "ground" (dom:make-rgb 9 9 9) t nil)   ; replaces the default layer
    (dom:restore-layer drawing 5 "roof" (dom:make-rgb 1 2 3) nil t)
    (signals-layer-error (dom:restore-layer drawing 5 "again" (dom:make-rgb 1 2 3) t nil))
    (signals-layer-error (dom:restore-layer drawing 6 "ROOF" (dom:make-rgb 1 2 3) t nil))
    (dom:restore-drawing-state drawing
                               :current-layer-id 5
                               :next-entity-id 40
                               :next-layer-id 9
                               :display-unit :centimeter
                               :grid-spacing 50d0
                               :grid-visible nil
                               :snap-to-grid nil
                               :object-snap t
                               :default-text-height 300d0)
    (is (equal '("ground" "roof") (mapcar #'dom:layer-name (dom:layers-in-order drawing))))
    (is (= 5 (dom:drawing-current-layer-id drawing)))
    (is (= 40 (dom:drawing-next-entity-id drawing)))
    (is (= 9 (dom:drawing-next-layer-id drawing)))
    (is (eq :centimeter (dom:drawing-display-unit drawing)))
    (is (= 50d0 (dom:drawing-grid-spacing drawing)))
    (is (dom:layer-locked (dom:find-layer drawing 5)))))

;;; Change sets

(test applying-and-inverting-a-change-set-round-trips
  (let* ((drawing (dom:make-drawing))
         (changes (dom:create-entities-changes (list (id-line 1) (id-line 2)))))
    (dom:apply-changes drawing changes)
    (is (equal '(1 2) (entity-ids drawing)))
    (is (= 3 (dom:drawing-next-entity-id drawing)))                 ; advanced past the new ids
    (dom:apply-changes drawing (dom:invert-changes changes))
    (is (null (entity-ids drawing)))
    (is (= 3 (dom:drawing-next-entity-id drawing)))))               ; ids are never reused

(test change-sets-are-validated-before-anything-changes
  (let ((drawing (dom:make-drawing)))
    (commit-entities drawing (id-line 1))
    (let ((revision (dom:drawing-revision drawing)))
      ;; The second change is stale: it claims entity 2 exists.
      (signals error
        (dom:apply-changes drawing
                           (list (dom:make-change 3 nil (id-line 3))
                                 (dom:make-change 2 (id-line 2) nil))))
      (is (equal '(1) (entity-ids drawing)))                        ; the valid first change was not applied
      (is (= revision (dom:drawing-revision drawing)))
      ;; Replacing with a stale "before" object fails too.
      (signals error
        (dom:apply-changes drawing (list (dom:make-change 1 (id-line 1) (a-line :id 1 :x2 5d0)))))
      ;; The same id twice, an entity on a missing layer, and an empty change are rejected.
      (signals error
        (dom:apply-changes drawing (list (dom:make-change 5 nil (id-line 5)) (dom:make-change 5 nil (id-line 5)))))
      (signals base:layer-error
        (dom:apply-changes drawing (list (dom:make-change 6 nil (a-line :id 6 :layer-id 9)))))
      (signals error (dom:apply-changes drawing (list (dom:make-change 7 nil nil))))
      (is (equal '(1) (entity-ids drawing))))))

;;; History

(test history-undo-and-redo-restore-exact-states-and-stacking-order
  (let ((drawing (dom:make-drawing))
        (history (dom:make-history)))
    (dom:history-commit history drawing (dom:create-entities-changes (list (id-line 1) (id-line 2) (id-line 3))))
    (let ((full (drawing-entities drawing)))
      (dom:history-commit history drawing (dom:delete-entities-changes drawing '(2)))
      (is (equal '(1 3) (entity-ids drawing)))
      (is (dom:history-undo history drawing))
      (is (equal '(1 2 3) (entity-ids drawing)))                    ; entity 2 returns to the middle
      (is (same-entities-p full (drawing-entities drawing)))
      (is (dom:history-redo history drawing))
      (is (equal '(1 3) (entity-ids drawing))))))

(test history-clears-redo-on-a-new-commit-and-reports-availability
  (let ((drawing (dom:make-drawing))
        (history (dom:make-history)))
    (is (not (dom:history-can-undo-p history)))
    (is (not (dom:history-undo history drawing)))
    (is (not (dom:history-redo history drawing)))
    (is (not (dom:history-commit history drawing '())))              ; empty change sets are ignored
    (dom:history-commit history drawing (dom:create-entity-changes (id-line 1)))
    (dom:history-commit history drawing (dom:create-entity-changes (id-line 2)))
    (dom:history-undo history drawing)
    (is (dom:history-can-redo-p history))
    (is (= 1 (dom:history-undo-depth history)))
    (is (= 1 (dom:history-redo-depth history)))
    (dom:history-commit history drawing (dom:create-entity-changes (id-line 3)))
    (is (not (dom:history-can-redo-p history)))
    (is (equal '(1 3) (entity-ids drawing)))
    (dom:history-clear history)
    (is (not (dom:history-can-undo-p history)))))

(test history-depth-is-limited
  (let ((drawing (dom:make-drawing))
        (history (dom:make-history :limit 3)))
    (loop for id from 1 to 6
          do (dom:history-commit history drawing (dom:create-entity-changes (id-line id))))
    (is (= 3 (dom:history-undo-depth history)))
    (loop while (dom:history-undo history drawing))
    (is (equal '(1 2 3) (entity-ids drawing)))))                     ; the oldest three can no longer be undone

(defun random-edit-changes (prng drawing)
  "Return a change set for a random user operation on DRAWING (possibly empty)."
  (let* ((entities (drawing-entities drawing))
         (ids (mapcar #'dom:entity-id entities))
         (some-ids (and ids (remove-duplicates (loop repeat (1+ (rng:prng-next-below prng 3))
                                                    collect (rng:prng-pick prng ids))))))
    (case (rng:prng-next-below prng 6)
      ((0 1) (dom:create-entity-changes
              (rng:random-entity prng
                                 (rng:random-kind prng)
                                 (dom:drawing-next-entity-id drawing)
                                 (rng:prng-pick prng '(0 1 2))
                                 50000d0)))
      (2 (and some-ids (values (dom:delete-entities-changes drawing some-ids))))
      (3 (and some-ids (values (dom:translate-entities-changes drawing some-ids
                                                               (rng:prng-next-double prng -500d0 500d0)
                                                               (rng:prng-next-double prng -500d0 500d0)))))
      (4 (and some-ids (values (dom:assign-layer-changes drawing some-ids (rng:prng-pick prng '(0 1 2))))))
      (5 (let ((entity (and entities (rng:prng-pick prng entities))))
           (when (and entity (typep entity 'dom:rectangle-entity))
             (dom:edit-property-changes drawing (dom:entity-id entity) :width
                                        (rng:prng-next-double prng 100d0 3000d0))))))))

(test history-property-undoing-everything-restores-every-intermediate-state
  (let* ((prng (rng:make-prng 99))
         (drawing (dom:make-drawing))
         (history (dom:make-history :limit 1000))
         (states (list (drawing-entities drawing))))
    (dom:add-layer drawing "a" (dom:make-rgb 1 1 1))
    (dom:add-layer drawing "b" (dom:make-rgb 2 2 2))
    (dotimes (step 250)
      (let ((changes (random-edit-changes prng drawing)))
        (when (dom:history-commit history drawing changes)
          (push (drawing-entities drawing) states))))
    (is (> (length states) 100))                                     ; the run really exercised the history
    ;; states is newest first. Undo one step at a time and compare with the recorded state.
    (let ((expected (rest states)))
      (loop while (dom:history-undo history drawing)
            do (is (same-entities-p (pop expected) (drawing-entities drawing))))
      (is (null expected)))
    ;; Redo everything and compare with the recorded states, oldest first.
    (let ((recorded (rest (reverse states))))
      (loop while (dom:history-redo history drawing)
            do (is (same-entities-p (pop recorded) (drawing-entities drawing))))
      (is (null recorded)))))

;;; Commands

(defun two-layer-drawing ()
  "Return a drawing with an unlocked layer 1 and a locked layer 2 and the matching ids."
  (let ((drawing (dom:make-drawing)))
    (dom:add-layer drawing "open" (dom:make-rgb 1 1 1))
    (dom:add-layer drawing "locked" (dom:make-rgb 2 2 2))
    (commit-entities drawing
                     (a-line :id 1 :layer-id 1)
                     (a-line :id 2 :layer-id 2)
                     (a-rectangle :id 3 :layer-id 1 :min-x 200d0 :min-y 200d0 :width 400d0 :height 300d0))
    (dom:set-layer-locked drawing 2 t)
    drawing))

(test commands-skip-entities-on-locked-layers-and-count-them
  (let ((drawing (two-layer-drawing)))
    (multiple-value-bind (changes skipped) (dom:delete-entities-changes drawing '(1 2 3 99))
      (is (equal '(1 3) (mapcar #'dom:change-entity-id changes)))
      (is (= 2 skipped)))                                            ; entity 2 (locked) and the missing 99
    (multiple-value-bind (changes skipped) (dom:translate-entities-changes drawing '(1 2) 10d0 0d0)
      (is (= 1 (length changes)))
      (is (= 1 skipped))
      (is (= 10d0 (dom:line-x1 (dom:change-after (first changes))))))
    (multiple-value-bind (changes skipped) (dom:assign-layer-changes drawing '(1 2 3) 0)
      (is (= 2 (length changes)))
      (is (= 1 skipped))
      (is (every (lambda (change) (= 0 (dom:entity-layer-id (dom:change-after change)))) changes)))
    (is (null (dom:assign-layer-changes drawing '(1) 1)))             ; already on that layer
    (signals-layer-error (dom:assign-layer-changes drawing '(1) 42))))

(test edit-property-changes-handle-noops-and-errors
  (let ((drawing (two-layer-drawing)))
    (is (null (dom:edit-property-changes drawing 1 :x2 1000d0)))       ; same value: no change
    (let ((changes (dom:edit-property-changes drawing 1 :x2 1234d0)))
      (is (= 1 (length changes)))
      (is (= 1234d0 (dom:line-x2 (dom:change-after (first changes))))))
    (signals-layer-error (dom:edit-property-changes drawing 2 :x2 5d0))  ; locked layer
    (signals-layer-error (dom:edit-property-changes drawing 99 :x2 5d0)) ; missing entity
    (signals base:invalid-entity (dom:edit-property-changes drawing 1 :x2 0d0))))

;;; Picking

(defun pick-id (drawing x y radius)
  "Return the id of the entity picked at (X, Y), or NIL."
  (let ((entity (dom:pick-entity drawing x y radius)))
    (and entity (dom:entity-id entity))))

(test picking-chooses-the-nearest-entity-within-the-radius
  (let ((drawing (dom:make-drawing)))
    (commit-entities drawing
                     (a-line :id 1 :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0)
                     (a-line :id 2 :x1 0d0 :y1 30d0 :x2 1000d0 :y2 30d0))
    (is (= 1 (pick-id drawing 500d0 8d0 20d0)))          ; 8 from line 1, 22 from line 2
    (is (= 2 (pick-id drawing 500d0 25d0 20d0)))
    (is (null (pick-id drawing 500d0 500d0 20d0)))
    (is (null (pick-id drawing 500d0 -20.0001d0 20d0)))
    (is (= 1 (pick-id drawing 500d0 -20d0 20d0)))        ; exactly on the radius counts
    (is (= 1 (pick-id drawing 500d0 -20d0 20d0)))))

(test picking-ties-go-to-the-topmost-entity
  (let ((drawing (dom:make-drawing)))
    (commit-entities drawing
                     (a-line :id 1 :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0)
                     (a-line :id 2 :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0))
    (is (= 2 (pick-id drawing 500d0 0d0 10d0)))))

(test picking-ignores-hidden-and-locked-layers
  (let ((drawing (two-layer-drawing)))
    (is (= 1 (pick-id drawing 500d0 0d0 10d0)))
    (dom:set-layer-visible drawing 1 nil)
    (is (null (pick-id drawing 500d0 0d0 10d0)))
    (is (null (pick-id drawing 500d0 0d0 10d0)))
    (dom:set-layer-visible drawing 1 t)
    (dom:set-layer-locked drawing 1 t)
    (is (null (pick-id drawing 500d0 0d0 10d0)))))

(test picking-a-rectangle-requires-its-outline-not-its-interior
  (let ((drawing (dom:make-drawing)))
    (commit-entities drawing (a-rectangle :id 1 :min-x 0d0 :min-y 0d0 :width 1000d0 :height 1000d0))
    (is (null (pick-id drawing 500d0 500d0 10d0)))
    (is (= 1 (pick-id drawing 500d0 5d0 10d0)))
    (is (= 1 (pick-id drawing 1003d0 500d0 10d0)))))

(test pick-agrees-with-brute-force-on-a-random-drawing
  (let* ((prng (rng:make-prng 123))
         (drawing (rng:generate-drawing :seed 17 :entity-count 400))
         (entities (drawing-entities drawing))
         (bounds-all (reduce #'geo:bounds-union (mapcar #'dom:entity-bounds entities))))
    (dom:set-layer-visible drawing 2 nil)
    (dom:set-layer-locked drawing 3 t)
    (dotimes (i 400)
      (let* ((x (rng:prng-next-double prng (geo:bounds-min-x bounds-all) (geo:bounds-max-x bounds-all)))
             (y (rng:prng-next-double prng (geo:bounds-min-y bounds-all) (geo:bounds-max-y bounds-all)))
             (radius (rng:prng-next-double prng 5d0 400d0))
             (expected nil)
             (expected-distance radius))
        ;; Reference: scan everything, no bounds pre-filter; the highest id wins ties.
        (dolist (entity entities)
          (when (dom:pickable-p drawing entity)
            (let ((distance (dom:entity-pick-distance entity x y)))
              (when (and (<= distance radius)
                         (or (null expected) (<= distance expected-distance)))
                (setf expected entity
                      expected-distance distance)))))
        (is (eq expected (dom:pick-entity drawing x y radius)))))))

(test window-selection-takes-only-fully-enclosed-pickable-entities
  (let ((drawing (two-layer-drawing)))
    (commit-entities drawing (a-circle :id 4 :layer-id 1 :center-x 5000d0 :center-y 5000d0 :radius 100d0))
    ;; Window 0..2000 x -100..1100 encloses lines 1, 2 and rectangle 3, but not the far circle.
    (let ((selected (dom:entities-in-window drawing (geo:make-bounds -100d0 -100d0 2000d0 1100d0))))
      (is (equal '(1 3) (mapcar #'dom:entity-id selected))))           ; 2 is on the locked layer
    ;; A window that cuts the rectangle selects nothing of it.
    (is (null (dom:entities-in-window drawing (geo:make-bounds -100d0 -100d0 900d0 100d0))))
    ;; Touching edges count as enclosed.
    (is (equal '(1) (mapcar #'dom:entity-id
                            (dom:entities-in-window drawing (geo:make-bounds 0d0 0d0 1000d0 0d0)))))
    (is (equal '(1 3 4) (mapcar #'dom:entity-id (dom:pickable-entities drawing))))))

(test window-selection-agrees-with-brute-force
  (let* ((prng (rng:make-prng 321))
         (drawing (rng:generate-drawing :seed 5 :entity-count 300))
         (entities (drawing-entities drawing)))
    (dotimes (i 100)
      (let* ((x (rng:prng-next-double prng 0d0 40000d0))
             (y (rng:prng-next-double prng 0d0 40000d0))
             (window (geo:bounds-from-corners x y
                                              (+ x (rng:prng-next-double prng 500d0 20000d0))
                                              (+ y (rng:prng-next-double prng 500d0 20000d0))))
             (expected (remove-if-not (lambda (entity)
                                        (geo:bounds-contains-bounds-p window (dom:entity-bounds entity)))
                                      entities)))
        (is (same-entities-p expected (dom:entities-in-window drawing window)))))))

;;; Snapping

(test grid-snap-produces-exact-multiples-of-the-spacing
  (let ((drawing (dom:make-drawing)))
    (dom:set-grid-spacing drawing 250d0)
    (multiple-value-bind (x y) (dom:snap-to-grid drawing 1130d0 -380d0)
      (is (= 1250d0 x))
      (is (= -500d0 y)))
    (multiple-value-bind (x y kind) (dom:snap-point drawing 1130d0 -380d0 0d0)
      (is (= 1250d0 x))
      (is (= -500d0 y))
      (is (eq :grid kind)))))

(test snapping-can-be-disabled
  (let ((drawing (dom:make-drawing)))
    (dom:set-snap-to-grid drawing nil)
    (dom:set-object-snap drawing nil)
    (multiple-value-bind (x y kind) (dom:snap-point drawing 1130.5d0 -380.25d0 100d0)
      (is (= 1130.5d0 x))
      (is (= -380.25d0 y))
      (is (null kind)))))

(test object-snap-wins-over-the-grid-and-respects-its-radius
  (let ((drawing (dom:make-drawing)))
    (commit-entities drawing
                     (a-line :id 1 :x1 1013d0 :y1 507d0 :x2 3013d0 :y2 507d0)
                     (a-circle :id 2 :center-x 6000d0 :center-y 6000d0 :radius 500d0))
    (multiple-value-bind (x y kind) (dom:snap-point drawing 1020d0 500d0 20d0)
      (is (= 1013d0 x))
      (is (= 507d0 y))
      (is (eq :endpoint kind)))
    (multiple-value-bind (x y kind) (dom:snap-point drawing 6004d0 5995d0 20d0)
      (is (= 6000d0 x))
      (is (= 6000d0 y))
      (is (eq :center kind)))
    ;; Outside the radius the grid applies again (spacing 100).
    (multiple-value-bind (x y kind) (dom:snap-point drawing 1040d0 500d0 20d0)
      (is (= 1000d0 x))
      (is (= 500d0 y))
      (is (eq :grid kind)))
    ;; Exactly on the radius still snaps.
    (is (eq :endpoint (nth-value 2 (dom:snap-point drawing 1033d0 507d0 20d0))))))

(test object-snap-ignores-entities-on-hidden-and-locked-layers
  (let ((drawing (two-layer-drawing)))
    ;; Entity 2 is the locked line starting at the origin; entity 1 is on the open layer.
    (dom:set-snap-to-grid drawing nil)
    (commit-entities drawing (a-line :id 4 :layer-id 2 :x1 777d0 :y1 777d0 :x2 900d0 :y2 900d0))
    (is (null (nth-value 2 (dom:snap-point drawing 779d0 779d0 20d0))))
    (dom:set-layer-visible drawing 1 nil)
    (is (null (nth-value 2 (dom:snap-point drawing 0d0 0d0 20d0))))))

(test object-snap-picks-the-nearest-candidate
  (let ((drawing (dom:make-drawing)))
    (commit-entities drawing
                     (a-line :id 1 :x1 100d0 :y1 100d0 :x2 1000d0 :y2 100d0)
                     (a-line :id 2 :x1 110d0 :y1 100d0 :x2 1000d0 :y2 900d0))
    (multiple-value-bind (x y) (dom:find-object-snap drawing 108d0 100d0 50d0)
      (is (= 110d0 x))
      (is (= 100d0 y)))
    (is (null (dom:find-object-snap drawing 5000d0 5000d0 50d0)))))

;;; Workload generator

(test generated-drawings-are-deterministic-and-valid
  (let ((first (rng:generate-drawing :seed 8 :entity-count 300))
        (second (rng:generate-drawing :seed 8 :entity-count 300))
        (other (rng:generate-drawing :seed 9 :entity-count 300)))
    (is (= 300 (dom:drawing-entity-count first)))
    (is (equalp (drawing-entities first) (drawing-entities second)))
    (is (not (equalp (drawing-entities first) (drawing-entities other))))
    (is (equal (loop for id from 1 to 300 collect id) (entity-ids first)))
    (is (= 6 (length (dom:layers-in-order first))))
    (is (= 6 (length (remove-duplicates (mapcar #'dom:entity-kind (drawing-entities first)))))))
  (is (= 0 (dom:drawing-entity-count (rng:generate-drawing :seed 1 :entity-count 0)))))
