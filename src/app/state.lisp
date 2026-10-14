(in-package #:borba-cad/app)

(defconstant +pick-radius-pixels+ 6d0
  "How close, in pixels, the pointer must be to an entity to pick it: about 1.6 mm on screen,
enough for a steady hand and small enough not to grab distant entities.")

(defconstant +snap-radius-pixels+ 12d0
  "How close, in pixels, the pointer must be to an endpoint or center for object snap to grab it.")

(defconstant +zoom-step-per-notch+ 1.2d0
  "Zoom factor of one mouse-wheel notch.")

(defconstant +drag-threshold-pixels+ 4d0
  "Pointer travel, in pixels, before a press becomes a drag; below it a release is a click.")

(defconstant +maximum-message-length+ 400
  "Longest status message kept, in characters.")

(defstruct (cursor (:constructor make-cursor (raw-x
                                              raw-y
                                              x
                                              y
                                              snap-kind
                                              modifiers)))
  "The pointer position in world coordinates.

RAW-X: world x under the pointer.
RAW-Y: world y under the pointer.
X: world x after snapping.
Y: world y after snapping.
SNAP-KIND: :endpoint, :center, :grid or NIL.
MODIFIERS: modifier keys held."
  (raw-x 0d0 :type double-float :read-only t)
  (raw-y 0d0 :type double-float :read-only t)
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  snap-kind
  (modifiers nil :type list :read-only t))

(defstruct (prompt (:constructor make-prompt (kind
                                              title
                                              message
                                              edit
                                              choices)))
  "A modal question.

KIND: :text (a text field with OK and Cancel) or :choice (a row of buttons).
TITLE: heading.
MESSAGE: explanation shown under the heading.
EDIT: the EDIT-STATE of the text field, for :text prompts.
CHOICES: for :text, a list of (label . function) with the accept function first; for :choice, the buttons.
The accept function of a :text prompt receives the typed string."
  kind title message edit choices)

(defstruct (app (:constructor %make-app))
  "The whole application state.

CONFIG: configuration.
TELEMETRY: observability hub, or NIL.
THEME: colors.
DRAWING: the drawing being edited.
HISTORY: undo and redo stacks.
VIEWPORT: current view.
SELECTION: hash table of selected entity ids.
TOOL: the active tool.
DRAW-LIST: reused every frame.
CURSOR: last pointer position in world coordinates, or NIL.
POINTER-X: last pointer window x.
POINTER-Y: last pointer window y.
INTERACTION: current pointer drag (:pan or a tool-owned drag) or NIL.
TRANSLATION: (dx . dy) move preview in millimetres, or NIL.
WINDOW: world bounds of the window-selection rectangle, or NIL.
SPACE-DOWN: true while the space bar is held (pan modifier).
FILE-PATH: path of the drawing file, or NIL.
SAVED-REVISION: drawing revision at the last save or load.
FOCUS: id of the field that has the keyboard focus, or NIL.
EDIT: edit state of that field.
PROMPT: the open modal prompt, or NIL.
MESSAGE: (text . severity) shown in the status bar, or NIL.
OVERLAY-VISIBLE: whether the performance overlay is shown.
LAYER-SCROLL: first visible layer row.
INSPECTOR-SCROLL: first visible inspector row.
FIELD-COMMIT: function of the typed string run when the focused field is committed.
FIELD-ORIGINAL: the text the focused field started with; an unchanged field commits nothing.
WINDOW-WIDTH: window width in logical pixels.
WINDOW-HEIGHT: window height in logical pixels.
DISPLAY-SCALE: framebuffer pixels per logical pixel.
NEEDS-REDRAW: true when the next frame differs from the last.
QUIT-REQUESTED: true when the application should close.
LAST-SCENE-RESULT: statistics of the last scene build.
LAST-ERROR: the last error condition reported, for tests and diagnostics."
  config telemetry theme drawing history viewport
  (selection (make-hash-table) :type hash-table)
  tool
  (draw-list (scene:make-draw-list))
  cursor
  (pointer-x 0d0 :type double-float)
  (pointer-y 0d0 :type double-float)
  interaction
  translation
  window
  space-down
  file-path
  (saved-revision 0 :type fixnum)
  focus
  edit
  prompt
  message
  overlay-visible
  (layer-scroll 0 :type fixnum)
  (inspector-scroll 0 :type fixnum)
  field-commit
  (field-original "" :type string)
  (window-width 1440d0 :type double-float)
  (window-height 900d0 :type double-float)
  (display-scale 1d0 :type double-float)
  (needs-redraw t)
  quit-requested
  last-scene-result
  last-error)

(defun canvas-rect (app)
  "Return the canvas rectangle of the current window size.

APP: the application."
  (ui:layout-canvas (ui:compute-layout (app-window-width app) (app-window-height app))))

(defun fit-viewport-to-window (app
                               viewport)
  "Return VIEWPORT with its canvas rectangle set from the window layout.

APP: the application.
VIEWPORT: a viewport."
  (let ((canvas (canvas-rect app)))
    (view:resize-viewport viewport
                          (max 1d0 (float (ui:rect-width canvas) 1d0))
                          (max 1d0 (float (ui:rect-height canvas) 1d0))
                          :origin-x (float (ui:rect-x canvas) 1d0)
                          :origin-y (float (ui:rect-y canvas) 1d0))))

(defun app-request-redraw (app)
  "Mark the next frame as different from the last.

APP: the application."
  (setf (app-needs-redraw app) t))

(defun app-dirty-p (app)
  "Return true when the drawing has unsaved changes.

APP: the application."
  (/= (dom:drawing-revision (app-drawing app)) (app-saved-revision app)))

(defun app-title (app)
  "Return the window title: file name, unsaved marker and program name.

APP: the application."
  (format nil "~:[Untitled~;~:*~a~]~:[~; *~] - Borba CAD"
          (and (app-file-path app) (file-namestring (app-file-path app)))
          (app-dirty-p app)))

(defun app-wants-text-input-p (app)
  "Return true when typed characters should be delivered as text events.

APP: the application."
  (and (or (app-focus app) (app-prompt app)) t))

(defun set-message (app
                    text
                    &optional (severity :info))
  "Show TEXT in the status bar.

APP: the application.
TEXT: the message.
SEVERITY: :info or :error."
  (setf (app-message app)
        (cons (if (> (length text) +maximum-message-length+)
                  (subseq text 0 +maximum-message-length+)
                  text)
              severity))
  (app-request-redraw app))

(defun report-error (app
                     condition
                     &optional (context "operation"))
  "Tell the user about a failure and log it; nothing is swallowed.

APP: the application.
CONDITION: the condition that was signaled.
CONTEXT: short name of what failed, used in the log event."
  (setf (app-last-error app) condition)
  (set-message app
               (remove #\Newline (princ-to-string condition))
               :error)
  (when (app-telemetry app)
    (obs:log-condition (obs:telemetry-logger (app-telemetry app))
                       :warn
                       (format nil "app.~a-failed" context)
                       condition)))

(defun selection-ids (app)
  "Return the ids of the selected entities in ascending order.

APP: the application."
  (sort (loop for id being the hash-keys of (app-selection app) collect id) #'<))

(defun selected-entities (app)
  "Return the selected entities in stacking order.

APP: the application."
  (loop for id in (selection-ids app)
        for entity = (dom:store-find (dom:drawing-store (app-drawing app)) id)
        when entity collect entity))

(defun clear-selection (app)
  "Deselect everything.

APP: the application."
  (clrhash (app-selection app))
  (app-request-redraw app))

(defun select-ids (app
                   ids
                   &key add)
  "Select the entities with IDS, replacing the selection unless ADD is true.

APP: the application.
IDS: list of entity ids.
ADD: when true, keep the current selection."
  (unless add
    (clrhash (app-selection app)))
  (dolist (id ids)
    (setf (gethash id (app-selection app)) t))
  (app-request-redraw app))

(defun toggle-selected (app
                        id)
  "Select ID if it is not selected, deselect it otherwise.

APP: the application.
ID: entity id."
  (if (gethash id (app-selection app))
      (remhash id (app-selection app))
      (setf (gethash id (app-selection app)) t))
  (app-request-redraw app))

(defun prune-selection (app)
  "Drop from the selection every entity that is gone or no longer pickable.

APP: the application."
  (let ((drawing (app-drawing app)))
    (loop for id in (selection-ids app)
          for entity = (dom:store-find (dom:drawing-store drawing) id)
          unless (and entity (dom:pickable-p drawing entity))
            do (remhash id (app-selection app)))))

(defun format-model-number (app
                            millimeters)
  "Format a length for display in the drawing's display unit with up to six decimals and no
trailing zeros, so that editing a shown value does not change it.

APP: the application.
MILLIMETERS: the length in millimetres."
  (let* ((text (base:format-length millimeters
                                   (dom:drawing-display-unit (app-drawing app))
                                   :precision 6))
         (trimmed (if (find #\. text) (string-right-trim "0" text) text)))
    (string-right-trim "." trimmed)))
