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

(defconstant +info-message-nanoseconds+ (* 6 sbcl:+nanoseconds-per-second+)
  "How long a message of the status bar stays before the hint or the tool's prompt shows again.")

(defconstant +error-message-nanoseconds+ (* 12 sbcl:+nanoseconds-per-second+)
  "How long an error stays in the status bar: longer, so that it can be read.")

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
SNAP-KIND: :endpoint, :midpoint, :center, :intersection, :quadrant, :perpendicular, :tangent, :grid, :ortho,
:polar or NIL.
MODIFIERS: modifier keys held."
  (raw-x 0d0 :type double-float :read-only t)
  (raw-y 0d0 :type double-float :read-only t)
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  snap-kind
  (modifiers nil :type list :read-only t))

(defconstant +report-message-lines+ 8
  "Lines of the message of a prompt that carries a report to read, which is longer than a question.")

(defstruct (prompt (:constructor make-prompt (kind
                                              title
                                              message
                                              edit
                                              choices
                                              &optional message-lines)))
  "A modal question.

KIND: :text (a text field with OK and Cancel) or :choice (a row of buttons).
TITLE: heading.
MESSAGE: explanation shown under the heading.
EDIT: the EDIT-STATE of the text field, for :text prompts.
CHOICES: for :text, a list of (label . function) with the accept function first; for :choice, the buttons.
The accept function of a :text prompt receives the typed string.
MESSAGE-LINES: how many lines the message may take before it is cut, or NIL for the usual three."
  kind title message edit choices message-lines)

(defstruct (menu (:constructor make-menu (kind
                                          anchor
                                          highlight)))
  "An open drop-down menu.

KIND: :draw, :modify, :language or :more.
ANCHOR: id of the button the menu hangs from.
HIGHLIGHT: position, among the choices, of the one the keyboard is on."
  kind
  anchor
  highlight)

(defconstant +console-line-limit+ 500
  "Most lines the console keeps of what was typed and printed; the oldest are dropped.")

(defconstant +console-history-limit+ 200
  "Most commands the console remembers for the up and down arrows.")

(defconstant +console-input-limit+ 20000
  "Most characters one command typed or pasted in the console may have.")

(defstruct (console (:constructor make-console ()))
  "The console, where commands are typed to draw and edit with the language of the program.

OPEN: true while the console is shown under the canvas.
LINES: what was typed and printed, newest first, each as (kind . text) with the kind :input, :output,
:value, :error or :note.
HISTORY: the commands that were run, newest first.
HISTORY-POSITION: where the up and down arrows are in the history, or NIL when a new command is being typed.
DRAFT: what was typed before the arrows started to walk through the history.
PENDING: the lines of a command whose parentheses are not closed yet, joined by newlines, or the empty string.
SCROLL: how many lines the output is scrolled up from its end."
  (open nil)
  (lines '() :type list)
  (history '() :type list)
  (history-position nil)
  (draft "" :type string)
  (pending "" :type string)
  (scroll 0 :type fixnum))

(defstruct (welcome (:constructor make-welcome (recents
                                                highlight)))
  "The start screen.

RECENTS: the pathnames of the recent drawings it lists, the latest first.
HIGHLIGHT: position, among its choices, of the one the keyboard is on."
  recents
  highlight)

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
WINDOW-CROSSING: true when that rectangle selects what it touches (dragged from right to left), and
NIL when it selects only what it holds (dragged from left to right).
SPACE-DOWN: true while the space bar is held (pan modifier).
FILE-PATH: path of the drawing file, or NIL.
SAVED-REVISION: drawing revision at the last save or load.
FOCUS: id of the field that has the keyboard focus, or NIL.
EDIT: edit state of that field.
PROMPT: the open modal prompt, or NIL.
DIALOG: the open file dialog, or NIL.
MENU: the open drop-down menu, or NIL.
WELCOME: the start screen, or NIL.
POLYGON-SIDES: the number of sides the polygon tool gives the next polygon.
GRIP-DRAG: the grip being dragged, or NIL.
CONSOLE: the console.
CONSTRAINT: :ortho, :polar or NIL, how the next point is held to the last one.
COORDINATE-EDIT: the edit state of a point being typed, or NIL.
AUTOSAVE-TIME: monotonic time of the last look of the autosave, or NIL before the first.
AUTOSAVE-REVISION: the revision of the drawing in the copy written last, or -1.
AUTOSAVE-WRITTEN: true when this session has written a copy that is still in the state directory.
IMAGE-CAPTURER: function of no arguments that returns what the canvas shows as a picture, as three values: the
pixels (bytes, row by row from the top, red, green and blue of each), the width and the height; or NIL when this
window cannot make one.
CLIPBOARD-READER: function of no arguments that returns the text on the system clipboard, or NIL.
CLIPBOARD-WRITER: function of one string that puts it on the system clipboard, or NIL.
CLIPBOARD-FALLBACK: the clipboard text kept here while there is no system clipboard.
MESSAGE: (text . severity) shown in the status bar, or NIL.
MESSAGE-TIME: monotonic time when the message was set, in nanoseconds.
OVERLAY-VISIBLE: whether the performance overlay is shown.
LAYER-SCROLL: first visible layer row.
SEEN-CURRENT-LAYER: the current layer when the layers card was last built, to scroll the card to a
new one.
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
  window-crossing
  space-down
  file-path
  (saved-revision 0 :type fixnum)
  focus
  edit
  prompt
  dialog
  menu
  welcome
  (polygon-sides 6 :type (integer 3))
  grip-drag
  (console (make-console))
  constraint
  coordinate-edit
  autosave-time
  (autosave-revision -1 :type fixnum)
  autosave-written
  image-capturer
  clipboard-reader
  clipboard-writer
  clipboard-fallback
  message
  message-time
  overlay-visible
  (layer-scroll 0 :type fixnum)
  (seen-current-layer nil)
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

(defun app-layout (app)
  "Return the regions of the window as they are now: the console takes its strip from the canvas when open.

APP: the application."
  (ui:compute-layout (app-window-width app)
                     (app-window-height app)
                     :console (console-open (app-console app))))

(defun canvas-rect (app)
  "Return the canvas rectangle of the current window size.

APP: the application."
  (ui:layout-canvas (app-layout app)))

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
  (format nil "~a~:[~; *~] - Borba CAD"
          (if (app-file-path app)
              (file-namestring (app-file-path app))
              (i18n:tr "Untitled"))
          (app-dirty-p app)))

(defun app-wants-text-input-p (app)
  "Return true when typed characters should be delivered as text events.

APP: the application."
  (and (or (app-focus app)
           (app-prompt app)
           (app-dialog app)
           (app-coordinate-edit app)
           (and (app-tool app)
                (not (eq :select (tool-kind (app-tool app))))
                (not (or (app-menu app) (app-welcome app)))))
       t))

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
              severity)
        (app-message-time app) (sbcl:monotonic-nanoseconds))
  (app-request-redraw app))

(defun clear-message (app)
  "Take the message out of the status bar. The next thing the user does is a new beginning, and
what the last one said no longer applies.

APP: the application."
  (when (app-message app)
    (setf (app-message app) nil
          (app-message-time app) nil)
    (app-request-redraw app)))

(defun expire-message (app
                       &optional (now (sbcl:monotonic-nanoseconds)))
  "Take the message out of the status bar once it has been there long enough, so that the hint
under the pointer and the prompt of the tool show again. Returns true when it was removed.

APP: the application.
NOW: the monotonic time to judge by, in nanoseconds."
  (let ((message (app-message app)))
    (when (and message
               (app-message-time app)
               (> (- now (app-message-time app))
                  (if (eq (cdr message) :error)
                      +error-message-nanoseconds+
                      +info-message-nanoseconds+)))
      (setf (app-message app) nil
            (app-message-time app) nil)
      (app-request-redraw app)
      t)))

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

(defparameter *internal-error-advice*
  (i18n:mark "Save your work and restart if things look wrong; the log has the details.")
  "What the user is told to do after a failure of the program itself.")

(defun report-internal-error (app
                              condition
                              context
                              backtrace)
  "Tell the user that the program itself failed, a bug rather than something they did, and log it
with the call stack. The program goes on: a failed event leaves the drawing as the last finished
operation left it.

APP: the application.
CONDITION: the condition that was signaled.
CONTEXT: what the program was doing, as a phrase that follows \"while\", for the message.
BACKTRACE: the call stack at the moment of the failure, as text, or NIL."
  (setf (app-last-error app) condition)
  (set-message app
               (i18n:trf "Internal error while ~a: ~a. ~a"
                         (i18n:tr context)
                         (remove #\Newline (princ-to-string condition))
                         (i18n:tr *internal-error-advice*))
               :error)
  (when (app-telemetry app)
    (obs:log-condition (obs:telemetry-logger (app-telemetry app))
                       :error
                       "app.internal-error"
                       condition
                       :context context
                       :backtrace (substitute #\| #\Newline (or backtrace "")))))

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

(defun trim-decimal-zeros (text)
  "Return TEXT, a decimal number, without the zeros that end its fraction and without a point left
at the end: 12.500 is 12.5 and 70.0000 is 70.

TEXT: a number written with a decimal point or without."
  (let ((trimmed (if (find #\. text)
                     (string-right-trim "0" text)
                     text)))
    (string-right-trim "." trimmed)))

(defun format-model-number (app
                            millimeters)
  "Format a length for display in the drawing's display unit with up to six decimals and no
trailing zeros, so that editing a shown value does not change it.

APP: the application.
MILLIMETERS: the length in millimetres."
  (trim-decimal-zeros (base:format-length millimeters
                                          (dom:drawing-display-unit (app-drawing app))
                                          :precision 6)))
