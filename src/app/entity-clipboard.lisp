(in-package #:borba-cad/app)

;;;; Copying, cutting and pasting entities. What is copied is the text of a small drawing that holds
;;;; the entities and the layers they use, in the format of a .bcad file: it goes on the system
;;;; clipboard, so it can be pasted into another window of the program, and into a text editor. Pasting
;;;; reads that text with the strict reader that opens files, which evaluates nothing, so whatever else
;;;; is on the clipboard is refused and does no harm.

(defun clipboard-drawing (app)
  "Return the drawing whose text is on the clipboard, or NIL when the clipboard holds anything else
or is empty.

APP: the application."
  (let ((text (clipboard-text app)))
    (when (and text
               (plusp (length text))
               (<= (length text) persist:+maximum-file-bytes+))
      (handler-case (persist:parse-drawing-text text)
        (base:drawing-file-error ()
          nil)))))

(defun copy-selection-to-clipboard (app
                                    entities)
  "Put the text of a drawing that holds ENTITIES on the clipboard.

APP: the application.
ENTITIES: the entities to copy, selected ones."
  (obs:with-operation ((app-telemetry app) "entity.copy"
                       :fields (list :entity-count (length entities))
                       :level :debug)
    (set-clipboard-text app (persist:drawing-to-string (dom:extract-entities (app-drawing app) entities)))))

(defun operation-copy (app)
  "Copy the selected entities to the clipboard.

APP: the application."
  (let ((entities (selected-entities app)))
    (cond ((null entities)
           (set-message app (i18n:tr "Nothing is selected.")))
          (t
           (copy-selection-to-clipboard app entities)
           (set-message app (i18n:trf "Copied ~d ~:*~[entities~;entity~:;entities~]." (length entities)))))))

(defun operation-cut (app)
  "Copy the selected entities to the clipboard and delete them.

APP: the application."
  (let ((entities (selected-entities app)))
    (cond ((null entities)
           (set-message app (i18n:tr "Nothing is selected.")))
          (t
           (copy-selection-to-clipboard app entities)
           (operation-delete-selection app)
           (when (zerop (hash-table-count (app-selection app)))
             (set-message app (i18n:trf "Cut ~d ~:*~[entities~;entity~:;entities~]." (length entities))))))))

(defun paste-target (app)
  "Return, as two values, the point the pasted entities are centered on: the pointer when it is
over the canvas, and the center of the view when it is not.

APP: the application."
  (let ((cursor (app-cursor app))
        (viewport (app-viewport app)))
    (if cursor
        (values (cursor-x cursor) (cursor-y cursor))
        (values (view:viewport-center-x viewport) (view:viewport-center-y viewport)))))

(defun paste-displacement (app
                           entities
                           in-place)
  "Return, as two values, how far the pasted ENTITIES move from where they were copied: nothing in
place, and otherwise as far as takes the center of their bounds to the paste target.

APP: the application.
ENTITIES: the entities of the copied drawing.
IN-PLACE: true to keep their coordinates."
  (if in-place
      (values 0d0 0d0)
      (let ((bounds (dom:entities-bounds entities)))
        (multiple-value-bind (x y)
            (paste-target app)
          (values (- x (geo:bounds-center-x bounds))
                  (- y (geo:bounds-center-y bounds)))))))

(defun operation-paste (app
                        &key in-place)
  "Paste the entities on the clipboard as new entities, selected, so that they can be moved at
once: centered on the pointer, or where they were copied from when IN-PLACE. They go onto the layer
of the same name when there is one that is visible and unlocked, and onto the current layer
otherwise.

APP: the application.
IN-PLACE: true to keep the coordinates they were copied with."
  (let ((snippet (clipboard-drawing app)))
    (if (or (null snippet) (zerop (dom:drawing-entity-count snippet)))
        (set-message app
                     (i18n:tr "Nothing to paste: the clipboard has no drawing entities.")
                     :error)
        (handler-case
            (multiple-value-bind (dx dy)
                (paste-displacement app
                                    (dom:drawing-entities snippet)
                                    in-place)
              (check-creation-layer app)
              (let ((pasted (dom:pasted-entities (app-drawing app)
                                                 snippet
                                                 dx
                                                 dy)))
                (when (commit-changes app
                                      (dom:create-entities-changes pasted)
                                      "entity.paste"
                                      :context "paste")
                  (select-ids app (mapcar #'dom:entity-id pasted))
                  (set-message app (i18n:trf "Pasted ~d ~:*~[entities~;entity~:;entities~]." (length pasted))))))
          (base:borba-cad-error (condition)
            (report-error app
                          condition
                          "paste"))))))
