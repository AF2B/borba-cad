(in-package #:borba-cad/app)

;;;; User-level operations. Each one validates, changes the drawing through the history, times
;;;; itself, and reports failures to the user instead of raising them.

(declaim (ftype function make-tool tool-cancel tool-kind selection-tool-kind-p measure-tool-kind-p activate-tool
                operation-insert-block))

(defparameter *untitled-file-name* "untitled.bcad"
  "File name offered in the Save As prompt for a drawing that has none.")

(defun commit-changes (app
                       changes
                       event
                       &key (context "edit"))
  "Commit a change set through the history, timed and logged as EVENT. Failures are reported to
the user and nothing changes. Returns true when changes were committed.

APP: the application.
CHANGES: list of change records; an empty list commits nothing and returns NIL.
EVENT: dotted event name such as entity.create.
CONTEXT: short name used in the error message log."
  (when changes
    (handler-case
        (progn
          (obs:with-operation ((app-telemetry app) event
                               :fields (list :changed (length changes)
                                             :entity-count (dom:drawing-entity-count (app-drawing app))))
            (dom:history-commit (app-history app)
                                (app-drawing app)
                                changes))
          (prune-selection app)
          (app-request-redraw app)
          t)
      (base:borba-cad-error (condition)
        (report-error app
                      condition
                      context)
        nil))))

(defun skipped-note (skipped)
  "Return a sentence about entities that were not changed, or an empty string.

SKIPPED: how many entities were skipped."
  (if (plusp skipped)
      (i18n:trf " (~d on locked layers were left unchanged)" skipped)
      ""))

;;; Editing operations

(defun operation-delete-selection (app)
  "Delete the selected entities.

APP: the application."
  (let ((ids (selection-ids app)))
    (if (null ids)
        (set-message app (i18n:tr "Nothing is selected."))
        (multiple-value-bind (changes skipped)
            (dom:delete-entities-changes (app-drawing app) ids)
          (cond ((null changes)
                 (set-message app
                              (i18n:tr "The selected entities are on locked layers.")
                              :error))
                ((commit-changes app
                                 changes
                                 "entity.delete"
                                 :context "delete")
                 (clear-selection app)
                 (set-message app (i18n:trf "Deleted ~d ~:*~[entities~;entity~:;entities~].~a"
                                            (length changes)
                                            (skipped-note skipped)))))))))

(defun operation-undo (app)
  "Undo the last change.

APP: the application."
  (handler-case
      (if (obs:with-operation ((app-telemetry app) "history.undo"
                               :fields (list :depth (dom:history-undo-depth (app-history app))))
            (dom:history-undo (app-history app) (app-drawing app)))
          (progn (prune-selection app)
                 (set-message app (i18n:tr "Undone."))
                 (app-request-redraw app))
          (set-message app (i18n:tr "Nothing to undo.")))
    (base:borba-cad-error (condition) (report-error app
                                                    condition
                                                    "undo"))))

(defun operation-redo (app)
  "Redo the last undone change.

APP: the application."
  (handler-case
      (if (obs:with-operation ((app-telemetry app) "history.redo"
                               :fields (list :depth (dom:history-redo-depth (app-history app))))
            (dom:history-redo (app-history app) (app-drawing app)))
          (progn (prune-selection app)
                 (set-message app (i18n:tr "Redone."))
                 (app-request-redraw app))
          (set-message app (i18n:tr "Nothing to redo.")))
    (base:borba-cad-error (condition) (report-error app
                                                    condition
                                                    "redo"))))

(defun operation-select-all (app)
  "Select every pickable entity.

APP: the application."
  (select-ids app (mapcar #'dom:entity-id (dom:pickable-entities (app-drawing app))))
  (set-message app (i18n:trf "~d selected." (hash-table-count (app-selection app)))))

(defun operation-assign-selection-layer (app
                                         layer-id)
  "Move the selected entities to another layer.

APP: the application.
LAYER-ID: id of the destination layer."
  (handler-case
      (multiple-value-bind (changes skipped)
          (dom:assign-layer-changes (app-drawing app)
                                    (selection-ids app)
                                    layer-id)
        (when (commit-changes app
                              changes
                              "entity.edit"
                              :context "layer-assignment")
          (set-message app (i18n:trf "Moved ~d to layer ~s.~a"
                                     (length changes)
                                     (dom:layer-name (dom:find-layer (app-drawing app) layer-id))
                                     (skipped-note skipped)))))
    (base:borba-cad-error (condition) (report-error app
                                                    condition
                                                    "layer-assignment"))))

(defun parse-property-input (kind
                             text
                             display-unit)
  "Interpret user text for a property of KIND. Signals INVALID-INPUT when it makes no sense.

KIND: the inspector property kind.
TEXT: what the user typed.
DISPLAY-UNIT: the drawing's display unit, assumed for numbers without a unit suffix."
  (ecase kind
    ((:coordinate :length) (base:parse-length text display-unit))
    (:angle (base:degrees->radians (base:parse-decimal text)))
    (:factor (float (base:parse-decimal text) 1d0))
    (:integer (handler-case (parse-integer (string-trim " " text))
                (parse-error ()
                  (error 'base:invalid-input :text text :reason (i18n:tr "expected a whole number")))))
    (:text text)
    (:unit (or (base:unit-from-abbreviation (string-trim " " text))
               (error 'base:invalid-input :text text :reason (i18n:tr "unknown unit; use mm, cm or m"))))))

(defun operation-edit-property (app
                                entity-id
                                key
                                value)
  "Set one property of one entity to an already interpreted value.

APP: the application.
ENTITY-ID: id of the entity.
KEY: property key from the entity's property list.
VALUE: the new value (double-float, string, integer, boolean or unit keyword)."
  (handler-case
      (let ((changes (dom:edit-property-changes (app-drawing app)
                                                entity-id
                                                key
                                                value)))
        (when (commit-changes app
                              changes
                              "entity.edit"
                              :context "property-edit")
          (set-message app (i18n:tr "Property changed."))))
    (base:borba-cad-error (condition) (report-error app
                                                    condition
                                                    "property-edit"))))

;;; View and settings

(defun operation-zoom-extents (app)
  "Fit the view to every visible entity.

APP: the application."
  (when (layout-mode-p app)
    (fit-layout-view app)
    (return-from operation-zoom-extents))
  (let ((bounds nil)
        (drawing (app-drawing app)))
    (dom:map-entities (lambda (entity)
                        (let ((layer (dom:find-layer drawing (dom:entity-layer-id entity))))
                          (when (and layer (dom:layer-visible layer))
                            (setf bounds (if bounds
                                             (geo:bounds-union bounds (dom:entity-bounds entity))
                                             (dom:entity-bounds entity))))))
                      (dom:drawing-store drawing))
    (setf (app-viewport app)
          (if bounds
              (view:fit-viewport-to-bounds (app-viewport app) bounds)
              (let ((viewport (app-viewport app)))
                (view:default-viewport (view:viewport-width viewport)
                                       (view:viewport-height viewport)
                                       :origin-x (view:viewport-origin-x viewport)
                                       :origin-y (view:viewport-origin-y viewport)))))
    (app-request-redraw app)))

(defun operation-zoom-by (app
                          factor)
  "Zoom the view by FACTOR about the center of the canvas, as a turn of the wheel does about the pointer.

APP: the application.
FACTOR: what the scale is multiplied by; above 1 zooms in."
  (let ((canvas (canvas-rect app)))
    (setf (app-viewport app)
          (view:zoom-viewport-at (app-viewport app)
                                 factor
                                 (float (+ (ui:rect-x canvas) (/ (ui:rect-width canvas) 2)) 1d0)
                                 (float (+ (ui:rect-y canvas) (/ (ui:rect-height canvas) 2)) 1d0)))
    (app-request-redraw app)))

(defun operation-zoom-in (app)
  "Zoom in by one notch of the wheel, about the center of the canvas.

APP: the application."
  (operation-zoom-by app +zoom-step-per-notch+))

(defun operation-zoom-out (app)
  "Zoom out by one notch of the wheel, about the center of the canvas.

APP: the application."
  (operation-zoom-by app (/ 1d0 +zoom-step-per-notch+)))

(defun operation-toggle-grid (app)
  "Show or hide the grid.

APP: the application."
  (dom:set-grid-visible (app-drawing app) (not (dom:drawing-grid-visible (app-drawing app))))
  (app-request-redraw app))

(defun operation-toggle-snap (app)
  "Turn snapping to the grid on or off.

APP: the application."
  (dom:set-snap-to-grid (app-drawing app) (not (dom:drawing-snap-to-grid (app-drawing app))))
  (app-request-redraw app))

(defun operation-toggle-object-snap (app)
  "Turn snapping to the points of the entities (endpoints, midpoints, centers, intersections, quadrants,
perpendiculars and tangents) on or off.

APP: the application."
  (dom:set-object-snap (app-drawing app) (not (dom:drawing-object-snap (app-drawing app))))
  (app-request-redraw app))

(defun operation-cycle-display-unit (app)
  "Switch the display unit to the next one (mm, cm, m).

APP: the application."
  (let* ((units (base:supported-length-units))
         (next (or (second (member (dom:drawing-display-unit (app-drawing app)) units)) (first units))))
    (dom:set-display-unit (app-drawing app) next)
    (set-message app (i18n:trf "Lengths are now shown in ~a." (base:unit-abbreviation next)))
    (app-request-redraw app)))

(defun operation-toggle-overlay (app)
  "Show or hide the performance overlay.

APP: the application."
  (setf (app-overlay-visible app) (not (app-overlay-visible app)))
  (app-request-redraw app))

(defun operation-set-tool (app
                           kind)
  "Activate a tool, cancelling whatever the previous one was doing.

APP: the application.
KIND: :select, :line, :rectangle, :circle, :polyline, :text, :dimension, :arc, :ellipse, :polygon, :rotate,
:mirror, :scale, :measure-distance, :measure-angle, :measure-area, :radius-dimension,
:diameter-dimension, :angular-dimension, :hatch, :solid-fill, :trim, :extend, :offset, :fillet, :spread or
:subtract. A tool that
changes the selection (rotate, mirror, scale) is not activated when nothing is selected."
  (when (and (layout-mode-p app) (not (eq kind :layout)))
    (operation-close-layout app))
  (cond ((and (selection-tool-kind-p kind) (zerop (hash-table-count (app-selection app))))
         (set-message app
                      (i18n:tr "Select what you want to change first.")
                      :error))
        ((eq kind :insert)
         (operation-insert-block app))
        (t
         (activate-tool app (make-tool kind))))
  (app-request-redraw app))

;;; Layers

(defun layer-operation (app
                        context
                        thunk)
  "Run THUNK, which changes layers, reporting layer errors to the user.

APP: the application.
CONTEXT: short name for the error log.
THUNK: function of no arguments."
  (handler-case (progn (funcall thunk)
                       (app-request-redraw app)
                       t)
    (base:borba-cad-error (condition)
      (report-error app
                    condition
                    context)
      nil)))

(defun operation-add-layer (app
                            name)
  "Create a layer, make it current and tell the user.

APP: the application.
NAME: the layer name."
  (layer-operation app
                   "layer"
                   (lambda ()
                     (let ((layer (dom:add-layer (app-drawing app)
                                                 name
                                                 (dom:palette-color-at (dom:drawing-next-layer-id (app-drawing app))))))
                       (dom:set-current-layer (app-drawing app) (dom:layer-id layer))
                       (set-message app (i18n:trf "Layer ~s created and selected." (dom:layer-name layer)))))))

(defun operation-rename-layer (app
                               layer-id
                               name)
  "Rename a layer.

APP: the application.
LAYER-ID: layer id.
NAME: the new name."
  (layer-operation app
                   "layer"
                   (lambda ()
                     (dom:rename-layer (app-drawing app)
                                       layer-id
                                       name)
                     (set-message app (i18n:tr "Layer renamed.")))))

(defun operation-delete-layer (app
                               layer-id)
  "Delete an empty layer. Deleting a layer clears the undo history, which may refer to it.

APP: the application.
LAYER-ID: layer id."
  (layer-operation app
                   "layer"
                   (lambda ()
                     (let ((name (dom:layer-name (dom:find-layer (app-drawing app) layer-id))))
                       (dom:remove-layer (app-drawing app) layer-id)
                       (dom:history-clear (app-history app))
                       (set-message app (i18n:trf "Layer ~s deleted; undo history cleared." name))))))

(defun operation-toggle-layer-visible (app
                                       layer-id)
  "Show or hide a layer; its entities leave the selection when hidden.

APP: the application.
LAYER-ID: layer id."
  (layer-operation app
                   "layer"
                   (lambda ()
                     (let ((layer (dom:find-layer (app-drawing app) layer-id)))
                       (dom:set-layer-visible (app-drawing app)
                                              layer-id
                                              (not (dom:layer-visible layer)))
                       (prune-selection app)))))

(defun operation-toggle-layer-locked (app
                                      layer-id)
  "Lock or unlock a layer; its entities leave the selection when locked.

APP: the application.
LAYER-ID: layer id."
  (layer-operation app
                   "layer"
                   (lambda ()
                     (let ((layer (dom:find-layer (app-drawing app) layer-id)))
                       (dom:set-layer-locked (app-drawing app)
                                             layer-id
                                             (not (dom:layer-locked layer)))
                       (prune-selection app)))))

(defun operation-cycle-layer-color (app
                                    layer-id)
  "Give a layer the next palette color.

APP: the application.
LAYER-ID: layer id."
  (layer-operation app
                   "layer"
                   (lambda ()
                     (dom:recolor-layer (app-drawing app)
                                        layer-id
                                        (dom:palette-color-after
                                         (dom:layer-color (dom:find-layer (app-drawing app) layer-id)))))))

(defun operation-set-current-layer (app
                                    layer-id)
  "Make a layer the one that receives new entities.

APP: the application.
LAYER-ID: layer id."
  (layer-operation app
                   "layer"
                   (lambda () (dom:set-current-layer (app-drawing app) layer-id))))

;;; Files

(defun drawings-directory (app)
  "Return the folder that the file dialogs start in: the folder of the file being edited, or the
configured drawings folder, or the user's documents folder.

APP: the application."
  (cond ((app-file-path app)
         (persist:directory-of (app-file-path app)))
        ((base:config-drawings-directory (app-config app)))
        (t (persist:default-drawings-directory))))

(defun reset-document (app
                       drawing
                       path)
  "Replace the drawing being edited, with a fresh history, selection and view.

APP: the application.
DRAWING: the new drawing.
PATH: its file, or NIL for a new drawing."
  (setf (app-drawing app) drawing
        (app-history app) (dom:make-history)
        (app-file-path app) path
        (app-saved-revision app) (dom:drawing-revision drawing)
        (app-translation app) nil
        (app-window app) nil
        (app-interaction app) nil
        (app-welcome app) nil)
  (clrhash (app-selection app))
  (operation-set-tool app :select)
  (operation-zoom-extents app)
  (app-request-redraw app)
  (run-hooks app :drawing-opened))

(defun log-failed-operation (app
                             event
                             started
                             condition
                             &rest fields)
  "Log a failed file operation with its duration.

APP: the application.
EVENT: dotted event name.
STARTED: monotonic time when it began.
CONDITION: the condition signaled.
FIELDS: extra alternating keys and values."
  (when (app-telemetry app)
    (obs:record-operation (obs:telemetry-metrics (app-telemetry app))
                          event
                          (- (sbcl:monotonic-nanoseconds) started))
    (apply #'obs:log-condition (obs:telemetry-logger (app-telemetry app)) :warn event condition
           :duration-ms (obs:nanoseconds->milliseconds (- (sbcl:monotonic-nanoseconds) started))
           fields)))

(defun fail-file-operation (app
                            event
                            started
                            condition)
  "Record a file operation that failed: log it, keep the condition and tell the user. Returns NIL,
so that the caller can return it as the outcome.

APP: the application.
EVENT: dotted event name, such as file.save.
STARTED: monotonic time when the operation began.
CONDITION: the condition signaled."
  (log-failed-operation app
                        event
                        started
                        condition)
  (setf (app-last-error app) condition)
  (set-message app
               (remove #\Newline (princ-to-string condition))
               :error)
  nil)

(defun fail-export-operation (app
                              event
                              started
                              condition)
  "Record an export that failed, as FAIL-FILE-OPERATION does, and also tell the user in a modal, which is
where the answer to an export is looked for. Returns NIL.

APP: the application.
EVENT: dotted event name, such as file.export-dxf.
STARTED: monotonic time when the operation began.
CONDITION: the condition signaled."
  (fail-file-operation app
                       event
                       started
                       condition)
  (show-notice app
               (i18n:tr "Could not export")
               (remove #\Newline (princ-to-string condition))
               +report-message-lines+)
  nil)

(defun note-recent-file (app
                         path)
  "Put the drawing PATH at the head of the list of recent files. A list that cannot be written is
logged, not shown: it is a convenience.

APP: the application.
PATH: the drawing's pathname."
  (let ((state (base:config-state-directory (app-config app))))
    (when (and state
               (not (persist:remember-recent-file state path))
               (app-telemetry app))
      (obs:log-event (obs:telemetry-logger (app-telemetry app))
                     :warn
                     "recent.write"
                     :result :failed
                     :path (uiop:native-namestring path)))))

(defun resolve-open-path (text
                          &optional (extension-path #'persist:drawing-file-path))
  "Return the pathname that an Open request names. A name without an extension that matches no
file but matches a file with the extension of the files being opened names that file.

TEXT: file name as typed by the user.
EXTENSION-PATH: function from the text to the pathname with the extension added."
  (let ((path (persist:native-path text)))
    (if (and (null (sbcl:file-kind path))
             (member (pathname-type path) '(nil "") :test #'equal))
        (let ((with-extension (funcall extension-path text)))
          (if (sbcl:file-kind with-extension)
              with-extension
              path))
        path)))

(defun load-drawing-into-app (app
                              path-text)
  "Load the drawing at PATH-TEXT and make it the current one. On failure the current drawing is
untouched and the reason is shown. Returns true on success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (let ((path (resolve-open-path path-text)))
          (multiple-value-bind (drawing stats)
              (persist:load-drawing path)
            (obs:record-operation-result (app-telemetry app)
                                         "file.load"
                                         (- (sbcl:monotonic-nanoseconds) started)
                                         :info
                                         :bytes (getf stats :bytes)
                                         :entity-count (dom:drawing-entity-count drawing)
                                         :read-ms (obs:nanoseconds->milliseconds
                                                   (getf stats :read-nanoseconds))
                                         :parse-ms (obs:nanoseconds->milliseconds
                                                    (getf stats :parse-nanoseconds))
                                         :result :success)
            (reset-document app
                            drawing
                            path)
            (note-recent-file app path)
            (set-message app
                         (i18n:trf "Opened ~a: ~d ~:*~[entities~;entity~:;entities~] (~a)."
                                 (file-namestring path)
                                 (dom:drawing-entity-count drawing)
                                 (base:format-byte-size (getf stats :bytes))))
            t))
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-file-operation app
                             "file.load"
                             started
                             condition)))))

(defun save-app-drawing (app
                         path-text)
  "Save the drawing to PATH-TEXT; a name without an extension gets .bcad. On failure the
previous file is untouched and the reason is shown. Returns true on success.

APP: the application.
PATH-TEXT: file name as typed by the user."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (run-hooks app :before-save)
    (handler-case
        (let ((path (persist:drawing-file-path path-text)))
          (multiple-value-bind (saved stats)
              (persist:save-drawing (app-drawing app) path)
            (declare (ignore saved))
            (obs:record-operation-result (app-telemetry app)
                                         "file.save"
                                         (- (sbcl:monotonic-nanoseconds) started)
                                         :info
                                         :bytes (getf stats :bytes)
                                         :entity-count (dom:drawing-entity-count (app-drawing app))
                                         :serialize-ms (obs:nanoseconds->milliseconds
                                                        (getf stats :serialize-nanoseconds))
                                         :rename-ms (obs:nanoseconds->milliseconds
                                                     (getf stats :rename-nanoseconds))
                                         :result :success)
            (setf (app-file-path app) path
                  (app-saved-revision app) (dom:drawing-revision (app-drawing app)))
            (note-recent-file app path)
            (set-message app
                         (i18n:trf "Saved ~a (~a)."
                                 (file-namestring path)
                                 (base:format-byte-size (getf stats :bytes))))
            (run-hooks app :after-save)
            t))
      ((or base:drawing-file-error base:invalid-input) (condition)
        (fail-file-operation app
                             "file.save"
                             started
                             condition)))))

(defun same-file-p (first
                    second)
  "Return true when two pathnames name the same existing file.

FIRST: a pathname, or NIL.
SECOND: a pathname, or NIL."
  (and first
       second
       (let ((first-true (probe-file first))
             (second-true (probe-file second)))
         (and first-true
              (equal first-true second-true)))))

(defun operation-save-as (app
                          &optional continuation)
  "Open the dialog where the user picks a folder and a name, and save the drawing there, then run
CONTINUATION.

APP: the application.
CONTINUATION: optional function of no arguments run after a successful save."
  (open-file-dialog app
                    :save
                    continuation))

(defun operation-save (app
                       &optional continuation)
  "Save the drawing to its file, asking for a name first when it has none.

APP: the application.
CONTINUATION: optional function of no arguments run after a successful save."
  (if (app-file-path app)
      (when (and (save-app-drawing app (uiop:native-namestring (app-file-path app))) continuation)
        (funcall continuation))
      (operation-save-as app continuation)))

(defun confirm-discard (app
                        continuation)
  "Run CONTINUATION now when there are no unsaved changes; otherwise ask whether to save,
discard or cancel first.

APP: the application.
CONTINUATION: function of no arguments."
  (if (not (app-dirty-p app))
      (funcall continuation)
      (open-choice-prompt app
                          (i18n:tr "Unsaved changes")
                          (i18n:tr "The drawing has changes that are not saved.")
                          (list (cons (i18n:tr "Save") (lambda () (operation-save app continuation)))
                                (cons (i18n:tr "Discard") continuation)
                                (cons (i18n:tr "Cancel") nil)))))

(defun operation-new (app)
  "Start a new empty drawing, after offering to save the current one.

APP: the application."
  (confirm-discard app (lambda () (reset-document app
                                                  (dom:make-drawing)
                                                  nil)
                         (set-message app (i18n:tr "New drawing.")))))

(defun operation-open (app)
  "Open the dialog where the user finds a drawing to open, after offering to save the current one.

APP: the application."
  (confirm-discard app (lambda () (open-file-dialog app :open))))

(defun operation-quit (app)
  "Close the application, after offering to save unsaved changes.

APP: the application."
  (confirm-discard app (lambda () (setf (app-quit-requested app) t))))

;;; Theme

(defparameter *theme-setting-name* "theme"
  "Name under which the chosen theme is kept between runs.")

(defun saved-theme (config)
  "Return the theme kept from the last run, or NIL when there is none.

CONFIG: the application configuration."
  (let ((name (persist:load-setting (base:config-state-directory config) *theme-setting-name*)))
    (and name (scene:theme-named name))))

(defun operation-set-theme (app
                            theme)
  "Draw the window with THEME now and keep the choice for the next run.

APP: the application.
THEME: a theme."
  (setf (app-theme app) theme)
  (persist:save-setting (base:config-state-directory (app-config app))
                        *theme-setting-name*
                        (scene:theme-name theme))
  (set-message app (if (eq (scene:theme-kind theme) :light)
                       (i18n:tr "Light theme.")
                       (i18n:tr "Dark theme.")))
  (app-request-redraw app))

(defun operation-toggle-theme (app)
  "Change from the dark theme to the light one, or back.

APP: the application."
  (operation-set-theme app
                       (if (eq (scene:theme-kind (app-theme app)) :light)
                           (scene:default-theme)
                           (scene:light-theme))))

;;; Interface zoom

(defparameter *interface-zoom-setting-name* "interface-zoom"
  "Name under which the zoom of the interface is kept between runs, in percent.")

(defun saved-interface-zoom (config)
  "Return the zoom of the interface kept from the last run, as a number where 1 is the interface as designed, or NIL
when there is none or it is not one of the zooms that the keys give.

CONFIG: the application configuration."
  (let* ((text (persist:load-setting (base:config-state-directory config) *interface-zoom-setting-name*))
         (percent (and text (parse-integer text :junk-allowed t))))
    (and percent
         (member percent *interface-zoom-percentages*)
         (/ percent 100d0))))

(defun operation-set-interface-zoom (app
                                     percent)
  "Draw the interface PERCENT per cent as large as designed, and keep the choice for the next run. The window lays
itself out again when the loop next looks.

APP: the application.
PERCENT: one of *INTERFACE-ZOOM-PERCENTAGES*."
  (setf (app-interface-zoom app) (/ percent 100d0))
  (persist:save-setting (base:config-state-directory (app-config app))
                        *interface-zoom-setting-name*
                        (format nil "~d" percent))
  (set-message app (i18n:trf "Interface zoom: ~d%" percent))
  (app-request-redraw app))

(defun operation-interface-zoom-step (app
                                      direction)
  "Make the interface one step larger or smaller.

APP: the application.
DIRECTION: 1 for larger, -1 for smaller."
  (let* ((current (round (* 100 (app-interface-zoom app))))
         (candidates (if (plusp direction)
                         (remove-if-not (lambda (percent) (> percent current)) *interface-zoom-percentages*)
                         (reverse (remove-if-not (lambda (percent) (< percent current))
                                                 *interface-zoom-percentages*)))))
    (if candidates
        (operation-set-interface-zoom app (first candidates))
        (set-message app (i18n:trf "Interface zoom: ~d%" current)))))

(defun operation-interface-zoom-in (app)
  "Make the interface one step larger.

APP: the application."
  (operation-interface-zoom-step app 1))

(defun operation-interface-zoom-out (app)
  "Make the interface one step smaller.

APP: the application."
  (operation-interface-zoom-step app -1))

(defun operation-interface-zoom-reset (app)
  "Draw the interface as designed.

APP: the application."
  (operation-set-interface-zoom app 100))

;;; Language

(defparameter *language-setting-name* "language"
  "Name under which the chosen language is kept between runs.")

(defun saved-language (config)
  "Return the language kept from the last run, or NIL when there is none or the program no longer
speaks it.

CONFIG: the application configuration."
  (let ((tag (persist:load-setting (base:config-state-directory config) *language-setting-name*)))
    (and tag (i18n:parse-language tag))))

(defun operation-set-language (app
                               language)
  "Make the interface speak LANGUAGE now and keep the choice for the next run.

APP: the application.
LANGUAGE: a language keyword."
  (setf i18n:*language* language)
  (persist:save-setting (base:config-state-directory (app-config app))
                        *language-setting-name*
                        (i18n:language-tag language))
  (set-message app (i18n:trf "Language: ~a" (i18n:language-name language)))
  (app-request-redraw app))
