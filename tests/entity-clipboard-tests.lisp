(in-package #:borba-cad/tests)

(def-suite entity-clipboard
  :description "Copying, cutting and pasting entities."
  :in all)

(in-suite entity-clipboard)

(defun share-clipboard (&rest applications)
  "Make APPLICATIONS use one clipboard, as two windows of the program do through the system's."
  (let ((cell nil))
    (dolist (application applications)
      (setf (app:app-clipboard-reader application) (lambda () cell)
            (app:app-clipboard-writer application) (lambda (text) (setf cell text))))
    (lambda () cell)))

(defun clipboard-contents-of (application)
  "Return the text on the clipboard of APPLICATION."
  (app:clipboard-text application))

(defun entity-signature (entity)
  "Return what describes ENTITY apart from its id and layer: its kind and its property values."
  (list (dom:entity-kind entity)
        (mapcar #'dom:property-value (dom:entity-properties entity))))

(defun entity-signatures (application)
  "Return the signatures of the entities of APPLICATION in stacking order."
  (mapcar #'entity-signature (entities-of application)))

(defun select-everything (application)
  "Select every entity of APPLICATION."
  (app:operation-select-all application))

;;; Copying

(test copying-puts-the-selected-entities-on-the-clipboard-as-drawing-text
  (let* ((application (two-lines-app))
         (revision (dom:drawing-revision (app:app-drawing application))))
    (click application 500 5)
    (press application :c :control)
    (is (= revision (dom:drawing-revision (app:app-drawing application))) "copying is not a change")
    (let ((snippet (persist:parse-drawing-text (clipboard-contents-of application))))
      (is (= 1 (dom:drawing-entity-count snippet)) "only the selected line")
      (is (eq :line (dom:entity-kind (first (dom:drawing-entities snippet))))))
    (is (search "Copied 1 entity" (message-text application)))
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))) "copying changes nothing")))

(test copying-with-nothing-selected-leaves-the-clipboard-alone
  (let ((application (two-lines-app)))
    (app:set-clipboard-text application "earlier text")
    (press application :escape)
    (press application :c :control)
    (is (string= "earlier text" (clipboard-contents-of application)))
    (is (search "Nothing is selected" (message-text application)))))

(test the-copied-text-is-a-drawing-with-the-layers-the-entities-are-on
  (let ((application (make-test-app)))
    (app:operation-add-layer application "walls")
    (press application :l)
    (click application 0 0)
    (click application 1000 0)
    (press application :escape)
    (click application 500 5)
    (press application :c :control)
    (let ((snippet (persist:parse-drawing-text (clipboard-contents-of application))))
      (is (equal '("0" "walls") (mapcar #'dom:layer-name (dom:layers-in-order snippet)))))))

;;; Pasting

(test pasting-centers-the-copy-on-the-pointer-and-selects-it
  (let ((application (two-lines-app)))
    (click application 500 5)
    (press application :c :control)
    (send application (pointer application :move nil 2000 1000))
    (press application :v :control)
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))))
    (let* ((pasted (last-entity application))
           (bounds (dom:entity-bounds pasted)))
      (is (< (abs (- 2000 (geo:bounds-center-x bounds))) 1d0))
      (is (< (abs (- 1000 (geo:bounds-center-y bounds))) 1d0))
      (is (equal (list (dom:entity-id pasted)) (app:selection-ids application)) "the copy is selected")
      (is (= 3 (dom:entity-id pasted)) "with a new id"))
    (is (search "Pasted 1 entity" (message-text application)))))

(test pasting-in-place-keeps-the-coordinates
  (let ((application (two-lines-app)))
    (click application 500 5)
    (press application :c :control)
    (send application (pointer application :move nil 2000 1000))
    (press application :v :control :shift)
    (let ((pasted (last-entity application)))
      (is (= 0d0 (dom:line-y1 pasted)))
      (is (= 1000d0 (dom:line-x2 pasted))))))

(test pasting-with-the-pointer-off-the-canvas-centers-on-the-view
  (let ((application (two-lines-app)))
    (click application 500 5)
    (press application :c :control)
    (send application (app:make-pointer-event :move nil 5d0 5d0))
    (press application :v :control)
    (let* ((pasted (last-entity application))
           (viewport (app:app-viewport application)))
      (is (< (abs (- (view:viewport-center-x viewport) (geo:bounds-center-x (dom:entity-bounds pasted)))) 1d0))
      (is (< (abs (- (view:viewport-center-y viewport) (geo:bounds-center-y (dom:entity-bounds pasted)))) 1d0)))))

(test a-paste-is-one-step-of-undo-and-redo
  (let ((application (two-lines-app)))
    (select-everything application)
    (press application :c :control)
    (press application :v :control :shift)
    (is (= 4 (dom:drawing-entity-count (app:app-drawing application))))
    (is (equal '(3 4) (app:selection-ids application)))
    (press application :z :control)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))) "one undo removes both")
    (press application :y :control)
    (is (= 4 (dom:drawing-entity-count (app:app-drawing application))))))

(test every-kind-of-entity-survives-a-copy-and-a-paste-into-another-window
  (let* ((source (make-test-app :drawing (rng:generate-drawing :seed 21 :entity-count 80)))
         (target (make-test-app)))
    (share-clipboard source target)
    (select-everything source)
    (press source :c :control)
    (press target :v :control :shift)
    (is (= (dom:drawing-entity-count (app:app-drawing source))
           (dom:drawing-entity-count (app:app-drawing target))))
    (is (equal (entity-signatures source) (entity-signatures target)) "the same shapes, in the same order")))

(test a-paste-goes-onto-the-layer-of-the-same-name-when-it-can-and-onto-the-current-one-otherwise
  (let ((source (make-test-app))
        (target (make-test-app)))
    (share-clipboard source target)
    (app:operation-add-layer source "walls")
    (press source :l)
    (click source 0 0)
    (click source 1000 0)
    (press source :escape)
    (click source 500 5)
    (press source :c :control)
    ;; the target has no layer of that name: the current layer receives it
    (press target :v :control :shift)
    (is (= dom:+default-layer-id+ (dom:entity-layer-id (last-entity target))))
    ;; the target has the layer: the copy goes there
    (app:operation-add-layer target "walls")
    (app:operation-set-current-layer target dom:+default-layer-id+)
    (press target :v :control :shift)
    (is (string= "walls" (dom:layer-name (dom:find-layer (app:app-drawing target)
                                                         (dom:entity-layer-id (last-entity target))))))
    ;; the layer is locked: the current layer receives it again
    (app:operation-toggle-layer-locked target (dom:layer-id (dom:find-layer-by-name (app:app-drawing target) "walls")))
    (press target :v :control :shift)
    (is (= dom:+default-layer-id+ (dom:entity-layer-id (last-entity target))))))

(test pasting-onto-a-locked-current-layer-is-refused-and-says-why
  (let ((application (two-lines-app)))
    (click application 500 5)
    (press application :c :control)
    (app:operation-toggle-layer-locked application dom:+default-layer-id+)
    (press application :v :control)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))
    (is (search "locked" (message-text application)))
    (is (eq :error (cdr (app:app-message application))))))

;;; Cutting

(test cutting-copies-and-deletes-and-undo-brings-the-entities-back
  (let ((application (two-lines-app)))
    (click application 500 5)
    (press application :x :control)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (is (search "Cut 1 entity" (message-text application)))
    (is (= 1 (dom:drawing-entity-count (persist:parse-drawing-text (clipboard-contents-of application)))))
    (press application :z :control)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))
    (press application :escape)
    (press application :x :control)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))) "nothing selected, nothing cut")))

(test cutting-then-pasting-moves-entities-between-windows
  (let ((source (two-lines-app))
        (target (make-test-app)))
    (share-clipboard source target)
    (select-everything source)
    (press source :x :control)
    (press target :v :control :shift)
    (is (= 0 (dom:drawing-entity-count (app:app-drawing source))))
    (is (= 2 (dom:drawing-entity-count (app:app-drawing target))))))

;;; What is not a copy

(test text-that-is-not-a-copied-drawing-is-refused-and-changes-nothing
  (dolist (text (list "" "hello" "(drawing" "(drawing (format-version 1))" "(+ 1 2)"
                      "#.(error \"read-time evaluation\")" (format nil "~c" (code-char 0))))
    (let ((application (two-lines-app)))
      (app:set-clipboard-text application text)
      (press application :v :control)
      (is (= 2 (dom:drawing-entity-count (app:app-drawing application))) "~s" text)
      (is (search "Nothing to paste" (message-text application)) "~s" text)
      (is (eq :error (cdr (app:app-message application)))))))

(test an-empty-clipboard-is-not-an-error-of-the-program
  (let ((application (two-lines-app)))
    (setf (app:app-clipboard-reader application) (lambda () nil))
    (press application :v :control)
    (is (search "Nothing to paste" (message-text application)))
    (is (null (app:clipboard-drawing application)))))

(test a-whole-drawing-file-on-the-clipboard-pastes-its-entities
  (let ((source (two-lines-app))
        (target (make-test-app)))
    (app:set-clipboard-text target (persist:drawing-to-string (app:app-drawing source)))
    (press target :v :control :shift)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing target))))))

;;; Where the keys go

(test the-copy-keys-act-on-the-text-of-a-field-when-one-has-the-keyboard
  (let ((application (two-lines-app)))
    (click application 500 5)
    (app:open-text-prompt application "Name" "Type:" "abc" #'identity)
    (press application :c :control)
    (is (string= "abc" (clipboard-contents-of application)) "the text of the field, not the entity")
    (app:close-prompt application)))

;;; The domain operations under it

(test an-entity-copied-with-another-identity-keeps-its-shape
  (dolist (entity (dom:drawing-entities (rng:generate-drawing :seed 3 :entity-count 60)))
    (let ((copy (dom:entity-with-identity entity 900 0)))
      (is (= 900 (dom:entity-id copy)))
      (is (= 0 (dom:entity-layer-id copy)))
      (is (equal (entity-signature entity) (entity-signature copy))))))

(test an-extracted-drawing-keeps-ids-layers-and-settings
  (let* ((drawing (rng:generate-drawing :seed 4 :entity-count 50))
         (entities (subseq (dom:drawing-entities drawing) 5 12))
         (snippet (dom:extract-entities drawing entities)))
    (is (equal (mapcar #'dom:entity-id entities) (mapcar #'dom:entity-id (dom:drawing-entities snippet))))
    (is (equal (mapcar #'dom:entity-layer-id entities)
               (mapcar #'dom:entity-layer-id (dom:drawing-entities snippet))))
    (is (eq (dom:drawing-display-unit drawing) (dom:drawing-display-unit snippet)))
    (is (> (dom:drawing-next-entity-id snippet) (reduce #'max entities :key #'dom:entity-id)))
    (is (every (lambda (entity) (dom:find-layer snippet (dom:entity-layer-id entity))) entities))))

(test pasted-entities-take-fresh-ids-from-the-drawing-they-go-into
  (let* ((source (rng:generate-drawing :seed 5 :entity-count 20))
         (snippet (dom:extract-entities source (dom:drawing-entities source)))
         (target (dom:make-drawing))
         (pasted (dom:pasted-entities target snippet 100d0 -50d0)))
    (is (equal (loop for id from 1 repeat 20 collect id) (mapcar #'dom:entity-id pasted)))
    (is (every (lambda (entity) (= dom:+default-layer-id+ (dom:entity-layer-id entity))) pasted)
        "the layers of the copy do not exist in the target, so the current layer receives them")))
