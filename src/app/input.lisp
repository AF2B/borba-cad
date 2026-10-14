(in-package #:borba-cad/app)

;;;; Input routing. Order of precedence for every event: the modal prompt, then the focused text
;;;; field, then the widgets under the pointer, then the canvas (pan, zoom and the active tool).

(defun canvas-contains-p (app
                          x
                          y)
  "Return true when the window position lies over the canvas.

APP: the application.
X: window x.
Y: window y."
  (ui:rect-contains-p (canvas-rect app)
                      x
                      y))

(defun update-cursor (app
                      x
                      y
                      modifiers)
  "Recompute the world position and snapped position of the pointer.

APP: the application.
X: pointer window x.
Y: pointer window y.
MODIFIERS: modifier keys held."
  (let ((viewport (app-viewport app))
        (drawing (app-drawing app)))
    (multiple-value-bind (world-x world-y)
        (view:screen->world viewport
                            x
                            y)
      (multiple-value-bind (snapped-x snapped-y kind)
          (dom:snap-point drawing
                          world-x
                          world-y
                          (view:pixels->world-length viewport +snap-radius-pixels+))
        (setf (app-cursor app) (make-cursor world-x
                                            world-y
                                            snapped-x
                                            snapped-y
                                            kind
                                            modifiers))))))

(defun refresh-cursor (app
                       &optional modifiers)
  "Recompute the cursor from the last pointer position, or clear it when the pointer is off the
canvas.

APP: the application.
MODIFIERS: modifier keys held."
  (if (canvas-contains-p app
                         (app-pointer-x app)
                         (app-pointer-y app))
      (update-cursor app
                     (app-pointer-x app)
                     (app-pointer-y app)
                     (or modifiers (and (app-cursor app) (cursor-modifiers (app-cursor app)))))
      (setf (app-cursor app) nil)))

;;; Pointer

(defun handle-widget-click (app
                            widget)
  "Run the action of a clicked widget.

APP: the application.
WIDGET: the widget under the pointer."
  (when (ui:widget-action widget)
    (funcall (ui:widget-action widget))
    (app-request-redraw app)))

(defun follow-pointer-in-menu (app
                               x
                               y)
  "Move the keyboard highlight of the open menu to the choice under the pointer, if there is one.

APP: the application.
X: pointer window x.
Y: pointer window y."
  (let* ((menu (app-menu app))
         (hit (ui:widget-at (build-widgets app)
                            x
                            y))
         (id (and hit (consp (ui:widget-id hit)) (eq :menu-item (first (ui:widget-id hit)))
                  (second (ui:widget-id hit))))
         (position (and id (position id
                                     (selectable-items (menu-items app (menu-kind menu)))
                                     :key #'menu-item-id))))
    (when (and position (not (eql position (menu-highlight menu))))
      (setf (menu-highlight menu) position)
      (app-request-redraw app))))

(defun handle-pointer-move (app
                            event)
  "Handle pointer movement: panning, cursor update and the tool's reaction.

APP: the application.
EVENT: a pointer event."
  (let ((x (pointer-event-x event))
        (y (pointer-event-y event))
        (interaction (app-interaction app)))
    (when (and (consp interaction) (eq (first interaction) :pan))
      (let ((started (sbcl:monotonic-nanoseconds)))
        (setf (app-viewport app) (view:pan-viewport (app-viewport app)
                                                    (- x (second interaction))
                                                    (- y (third interaction)))
              (app-interaction app) (list :pan x y))
        (when (app-telemetry app)
          (obs:telemetry-record-viewport-update (app-telemetry app) (- (sbcl:monotonic-nanoseconds) started)))))
    (cond ((or (app-prompt app) (app-dialog app)) nil)
          ((app-menu app)
           (follow-pointer-in-menu app
                                   x
                                   y))
          ((or (canvas-contains-p app
                                  x
                                  y) (eq interaction :tool))
           (update-cursor app
                          x
                          y
                          (pointer-event-modifiers event))
           (unless (and (consp interaction) (eq (first interaction) :pan))
             (tool-pointer-move (app-tool app)
                                app
                                (app-cursor app))))
          (t (setf (app-cursor app) nil)))
    (app-request-redraw app)))

(defun handle-pointer-down (app
                            event)
  "Handle a button press.

APP: the application.
EVENT: a pointer event."
  (let* ((x (pointer-event-x event))
         (y (pointer-event-y event))
         (button (pointer-event-button event))
         (widgets (build-widgets app))
         (hit (ui:widget-at widgets
                            x
                            y)))
    (clear-message app)
    (when (and (app-focus app) (not (member (app-focus app) '(:prompt :dialog)))
               (not (and hit (equal (ui:widget-id hit) (app-focus app)))))
      (commit-field-edit app)
      (setf widgets (build-widgets app)
            hit (ui:widget-at widgets
                              x
                              y)))
    (cond ((and hit (eq button :left))
           (handle-widget-click app hit))
          ((app-menu app)
           (close-menu app))
          ((or (app-prompt app) (app-dialog app)) nil)
          ((and (canvas-contains-p app
                                   x
                                   y)
                (or (eq button :middle) (and (eq button :left) (app-space-down app))))
           (setf (app-interaction app) (list :pan x y)))
          ((and (canvas-contains-p app
                                   x
                                   y) (eq button :left))
           (update-cursor app
                          x
                          y
                          (pointer-event-modifiers event))
           (setf (app-interaction app) :tool)
           (tool-pointer-down (app-tool app)
                              app
                              (app-cursor app)))
          ((and (canvas-contains-p app
                                   x
                                   y) (eq button :right))
           (cancel-or-select app)))
    (app-request-redraw app)))

(defun handle-pointer-up (app
                          event)
  "Handle a button release.

APP: the application.
EVENT: a pointer event."
  (let ((interaction (app-interaction app))
        (button (pointer-event-button event)))
    (cond ((and (consp interaction) (eq (first interaction) :pan)
                (member button '(:middle :left)))
           (setf (app-interaction app) nil))
          ((and (eq interaction :tool) (eq button :left))
           (setf (app-interaction app) nil)
           (update-cursor app
                          (pointer-event-x event)
                          (pointer-event-y event)
                          (pointer-event-modifiers event))
           (tool-pointer-up (app-tool app)
                            app
                            (app-cursor app))))
    (app-request-redraw app)))

(defun handle-pointer (app
                       event)
  "Route a pointer event.

APP: the application.
EVENT: a pointer event."
  (setf (app-pointer-x app) (pointer-event-x event)
        (app-pointer-y app) (pointer-event-y event))
  (ecase (pointer-event-action event)
    (:move (handle-pointer-move app event))
    (:down (handle-pointer-down app event))
    (:up (handle-pointer-up app event))))

;;; Wheel

(defun scroll-panel (app
                     rect
                     delta
                     x
                     y
                     setter)
  "Scroll a panel by wheel notches when the pointer is over it.

APP: the application.
RECT: the panel rectangle.
DELTA: wheel notches (positive scrolls up).
X: pointer window x.
Y: pointer window y.
SETTER: function of the row offset change."
  (declare (ignore app))
  (when (ui:rect-contains-p rect
                            x
                            y)
    (funcall setter (- (round delta)))
    t))

(defun handle-wheel (app
                     event)
  "Zoom at the pointer over the canvas, or scroll the panel under the pointer.

APP: the application.
EVENT: a wheel event."
  (let* ((x (wheel-event-x event))
         (y (wheel-event-y event))
         (delta (wheel-event-delta event))
         (layout (ui:compute-layout (app-window-width app) (app-window-height app))))
    (setf (app-pointer-x app) x
          (app-pointer-y app) y)
    (cond ((app-prompt app) nil)
          ((app-menu app) nil)
          ((app-dialog app)
           (handle-dialog-wheel app delta))
          ((scroll-panel app
                         (ui:layout-layers-panel layout)
                         delta
                         x
                         y
                         (lambda (change)
                           (setf (app-layer-scroll app)
                                 (max 0 (+ (app-layer-scroll app) change))))))
          ((scroll-panel app
                         (ui:layout-inspector-panel layout)
                         delta
                         x
                         y
                         (lambda (change)
                           (setf (app-inspector-scroll app)
                                 (max 0 (+ (app-inspector-scroll app) change))))))
          ((canvas-contains-p app
                              x
                              y)
           (let ((started (sbcl:monotonic-nanoseconds)))
             (setf (app-viewport app)
                   (view:zoom-viewport-at (app-viewport app)
                                          (expt +zoom-step-per-notch+ delta)
                                          x
                                          y))
             (refresh-cursor app (wheel-event-modifiers event))
             (when (app-telemetry app)
               (obs:telemetry-record-viewport-update (app-telemetry app)
                                                     (- (sbcl:monotonic-nanoseconds) started))))))
    (app-request-redraw app)))

;;; Keyboard

(defun cancel-or-select (app)
  "Cancel what the active tool is doing; when idle, return to the select tool, and when that is
already active, clear the selection.

APP: the application."
  (let ((tool (app-tool app)))
    (cond ((tool-in-progress-p tool) (tool-cancel tool app))
          ((not (eq (tool-kind tool) :select)) (operation-set-tool app :select))
          (t (clear-selection app)))
    (app-request-redraw app)))

(defun handle-prompt-key (app
                          key
                          modifiers)
  "Handle a key press while a modal prompt is open.

APP: the application.
KEY: key keyword.
MODIFIERS: modifier keys held."
  (let ((prompt (app-prompt app)))
    (case key
      (:enter (accept-prompt app))
      (:escape (cancel-prompt app))
      (t (when (eq (prompt-kind prompt) :text)
           (let ((edited (edit-with-clipboard app
                                              (prompt-edit prompt)
                                              key
                                              modifiers)))
             (when edited
               (setf (prompt-edit prompt) edited))))))
    (app-request-redraw app)))

(defun handle-field-key (app
                         key
                         modifiers)
  "Handle a key press while a text field has the focus.

APP: the application.
KEY: key keyword.
MODIFIERS: modifier keys held."
  (case key
    (:enter (commit-field-edit app))
    (:escape (stop-field-edit app))
    (t (let ((edited (and (app-edit app)
                          (edit-with-clipboard app
                                               (app-edit app)
                                               key
                                               modifiers))))
         (when edited
           (setf (app-edit app) edited)))))
  (app-request-redraw app))

(defun handle-control-shortcut (app
                                key
                                modifiers)
  "Handle a Ctrl+key shortcut; returns true when it was one.

APP: the application.
KEY: key keyword.
MODIFIERS: modifier keys held."
  (let ((shift (modifier-p modifiers :shift)))
    (case key
      (:z (if shift (operation-redo app) (operation-undo app)) t)
      (:y (operation-redo app) t)
      (:a (operation-select-all app) t)
      (:c (operation-copy app) t)
      (:x (operation-cut app) t)
      (:v (operation-paste app :in-place shift) t)
      (:s (if shift (operation-save-as app) (operation-save app)) t)
      (:o (operation-open app) t)
      (:i (when shift (operation-import-dxf app)) t)
      (:e (when shift (operation-export-dxf app)) t)
      (:n (operation-new app) t)
      (:q (operation-quit app) t)
      (t nil))))

(defun handle-plain-shortcut (app
                              key)
  "Handle a key without modifiers on the canvas; returns true when it was a shortcut.

APP: the application.
KEY: key keyword."
  (let ((tool (cdr (assoc key *tool-shortcuts*))))
    (cond (tool (operation-set-tool app tool) t)
          (t (case key
               (:escape (cancel-or-select app) t)
               ((:delete :backspace) (operation-delete-selection app) t)
               (:g (operation-toggle-grid app) t)
               (:s (operation-toggle-snap app) t)
               (:o (operation-toggle-object-snap app) t)
               (:u (operation-cycle-display-unit app) t)
               (:home (operation-zoom-extents app) t)
               (:f3 (operation-toggle-overlay app) t)
               (t nil))))))

(defun handle-key (app
                   event)
  "Route a key event.

APP: the application.
EVENT: a key event."
  (let ((key (key-event-key event))
        (modifiers (key-event-modifiers event)))
    (when (eq (key-event-action event) :down)
      (clear-message app))
    (when (and (app-menu app) (modifier-p modifiers :control))
      (close-menu app))
    (cond ((eq (key-event-action event) :up)
           (when (eq key :space)
             (setf (app-space-down app) nil)))
          ((app-prompt app) (handle-prompt-key app
                                               key
                                               modifiers))
          ((app-dialog app) (handle-dialog-key app
                                               key
                                               modifiers))
          ((app-menu app) (handle-menu-key app key))
          ((app-focus app) (handle-field-key app
                                             key
                                             modifiers))
          ((eq key :space) (setf (app-space-down app) t))
          ((modifier-p modifiers :control) (handle-control-shortcut app
                                                                    key
                                                                    modifiers))
          ((tool-key (app-tool app)
                     app
                     key
                     modifiers))
          (t (handle-plain-shortcut app key)))
    (app-request-redraw app)))

(defun handle-text (app
                    event)
  "Insert typed text into the open prompt or the focused field.

APP: the application.
EVENT: a text event."
  (let ((prompt (app-prompt app)))
    (cond ((and prompt (eq (prompt-kind prompt) :text))
           (setf (prompt-edit prompt) (ui:edit-insert (prompt-edit prompt) (text-event-text event))))
          (prompt nil)
          ((app-dialog app)
           (handle-dialog-text app (text-event-text event)))
          ((and (app-focus app) (app-edit app))
           (setf (app-edit app) (ui:edit-insert (app-edit app) (text-event-text event)))))
    (app-request-redraw app)))

(defun app-resize (app
                   width
                   height
                   &optional (scale 1d0))
  "Handle a window size change: the canvas follows the layout and keeps its center and zoom.

APP: the application.
WIDTH: new logical window width.
HEIGHT: new logical window height.
SCALE: display scale."
  (setf (app-window-width app) (float width 1d0)
        (app-window-height app) (float height 1d0)
        (app-display-scale app) (float scale 1d0)
        (app-viewport app) (fit-viewport-to-window app (app-viewport app)))
  (refresh-cursor app)
  (app-request-redraw app))

(defun handle-window-state (app
                            event)
  "Settle what the window's own changes leave hanging. When the keyboard goes elsewhere the
release of a held Space or of a mouse button never arrives, so a pan or a drag is ended here and
the pan modifier let go (a line or a polyline half drawn is kept: the user may come back to it).
When the pointer leaves the window its hover and its position in the status bar go, unless a drag
holds it.

APP: the application.
EVENT: a window state event."
  (ecase (window-state-event-kind event)
    (:focus-lost
     (setf (app-space-down app) nil)
     (when (app-interaction app)
       (when (eq (app-interaction app) :tool)
         (when (eq (tool-kind (app-tool app)) :select)
           (tool-cancel (app-tool app) app)))
       (setf (app-interaction app) nil)))
    (:pointer-left
     (unless (app-interaction app)
       (setf (app-cursor app) nil
             (app-pointer-x app) -1d0
             (app-pointer-y app) -1d0))))
  (app-request-redraw app))

(defun handle-file-drop (app
                         event)
  "Open a drawing dropped on the window, after offering to save the current one. A dropped file
is ignored while a prompt is asking something; it closes the file dialog, which it replaces.

APP: the application.
EVENT: a file drop event."
  (unless (app-prompt app)
    (when (app-dialog app)
      (close-file-dialog app))
    (let ((path (file-drop-event-path event)))
      (confirm-discard app (lambda () (load-drawing-into-app app path)))))
  (app-request-redraw app))

(defun event-type-name (event)
  "Return the name under which an event's handling time is aggregated.

EVENT: an input event."
  (etypecase event
    (pointer-event (format nil "pointer-~(~a~)" (pointer-event-action event)))
    (wheel-event "wheel")
    (key-event "key")
    (text-event "text")
    (file-drop-event "file-drop")
    (window-state-event "window-state")
    (resize-event "resize")
    (quit-event "quit")))

(defun app-handle-event (app
                         event)
  "Handle one input event and record its handling time for the aggregated input statistics.

APP: the application.
EVENT: a pointer, wheel, key, text, resize or quit event."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (handler-case
        (etypecase event
          (pointer-event (handle-pointer app event))
          (wheel-event (handle-wheel app event))
          (key-event (handle-key app event))
          (text-event (handle-text app event))
          (file-drop-event (handle-file-drop app event))
          (window-state-event (handle-window-state app event))
          (resize-event (app-resize app
                                    (resize-event-width event)
                                    (resize-event-height event)
                                    (resize-event-scale event)))
          (quit-event (operation-quit app)))
      (base:borba-cad-error (condition)
        (report-error app
                      condition
                      "input")))
    (when (app-telemetry app)
      (obs:telemetry-record-input (app-telemetry app)
                                  (event-type-name event)
                                  (- (sbcl:monotonic-nanoseconds) started)))))
