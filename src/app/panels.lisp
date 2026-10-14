(in-package #:borba-cad/app)

;;;; Panels: the widget lists for the toolbar, the layers panel, the inspector, the status bar and
;;;; the modal prompt, derived from the application state. Because the list is rebuilt from
;;;; state, drawing and hit testing always agree.

(defconstant +button-gap+ 4f0
  "Gap between toolbar buttons, in pixels.")

(defconstant +group-gap+ 14f0
  "Extra gap between toolbar groups, in pixels.")

(defconstant +prompt-width+ 600f0
  "Width of the modal prompt, in pixels.")

(defparameter *degree-sign* (string (code-char 176))
  "The degree sign, built from its code point to keep sources ASCII.")

(defun focused-p (app
                  id)
  "Return true when the field ID has the keyboard focus.

APP: the application.
ID: field id."
  (and (app-focus app) (equal (app-focus app) id)))

(defun start-field-edit (app
                         id
                         text
                         commit)
  "Give the keyboard focus to a field, with its text selected.

APP: the application.
ID: field id.
TEXT: the text the field shows.
COMMIT: function of the typed string, called when the edit is committed."
  (setf (app-focus app) id
        (app-edit app) (ui:edit-select-all-replace (ui:make-edit-state :text text))
        (app-field-commit app) commit
        (app-field-original app) text)
  (app-request-redraw app))

(defun stop-field-edit (app)
  "Remove the keyboard focus from the focused field without committing.

APP: the application."
  (setf (app-focus app) nil
        (app-edit app) nil
        (app-field-commit app) nil)
  (app-request-redraw app))

(defun commit-field-edit (app)
  "Commit the focused field: the commit function runs unless the text is unchanged.

APP: the application."
  (let ((commit (app-field-commit app))
        (text (and (app-edit app) (ui:edit-state-text (app-edit app))))
        (original (app-field-original app)))
    (stop-field-edit app)
    (when (and commit text (string/= text original))
      (funcall commit text))))

(defun make-button (id
                    label
                    x
                    y
                    &key action
                         active
                         (enabled t)
                         blocked
                         hint
                         width)
  "Create a toolbar-style button sized to its label.

ID: widget id.
LABEL: text.
X: left edge.
Y: top edge.
ACTION: function called on click.
ACTIVE: toggled-on state.
ENABLED: false to dim and disable.
BLOCKED: true while a modal covers it.
HINT: status-bar hint while hovered.
WIDTH: width override."
  (ui:make-widget :button
                  id
                  x
                  y
                  (or width (+ (ui:text-width-pixels label) (* 2 ui:+padding+)))
                  (- ui:+toolbar-height+ (* 2 ui:+padding+) -4f0)
                  :label label
                  :action action
                  :active active
                  :enabled enabled
                  :blocked blocked
                  :hint hint))

(defun toolbar-specs (app)
  "Return the toolbar buttons as a list of (id label action hint active enabled), with NIL
entries marking group separators.

APP: the application."
  (let* ((drawing (app-drawing app))
         (kind (tool-kind (app-tool app))))
    (flet ((tool-spec (tool-kind label hint)
             (list tool-kind label (lambda () (operation-set-tool app tool-kind)) hint (eq kind tool-kind) t)))
      (list (list :new "New" (lambda () (operation-new app)) "New drawing (Ctrl+N)" nil t)
            (list :open "Open" (lambda () (operation-open app)) "Open a drawing (Ctrl+O)" nil t)
            (list :save "Save" (lambda () (operation-save app)) "Save (Ctrl+S)" nil t)
            (list :save-as "Save As" (lambda () (operation-save-as app)) "Save as... (Ctrl+Shift+S)" nil t)
            nil
            (list :undo "Undo" (lambda () (operation-undo app)) "Undo (Ctrl+Z)" nil
                  (dom:history-can-undo-p (app-history app)))
            (list :redo "Redo" (lambda () (operation-redo app)) "Redo (Ctrl+Y)" nil
                  (dom:history-can-redo-p (app-history app)))
            nil
            (tool-spec :select "Select" "Select and move (Esc)")
            (tool-spec :line "Line" "Line (L)")
            (tool-spec :rectangle "Rect" "Rectangle (R)")
            (tool-spec :circle "Circle" "Circle (C)")
            (tool-spec :polyline "Poly" "Polyline (P)")
            (tool-spec :text "Text" "Text (T)")
            (tool-spec :dimension "Dim" "Dimension (D)")
            nil
            (list :grid "Grid" (lambda () (operation-toggle-grid app)) "Show the grid (G)"
                  (dom:drawing-grid-visible drawing) t)
            (list :snap "Snap" (lambda () (operation-toggle-snap app)) "Snap to the grid (S)"
                  (dom:drawing-snap-to-grid drawing) t)
            (list :object-snap "OSnap" (lambda () (operation-toggle-object-snap app))
                  "Snap to endpoints and centers (O)" (dom:drawing-object-snap drawing) t)
            (list :unit (format nil "Unit: ~a" (base:unit-abbreviation (dom:drawing-display-unit drawing)))
                  (lambda () (operation-cycle-display-unit app)) "Change the display unit (U)" nil t)
            (list :extents "Extents" (lambda () (operation-zoom-extents app)) "Zoom to fit everything (Home)" nil t)
            nil
            (list :perf "Perf" (lambda () (operation-toggle-overlay app)) "Performance overlay (F3)"
                  (app-overlay-visible app) t)))))

(defun toolbar-widgets (app
                        blocked)
  "Build the toolbar background and buttons.

APP: the application.
BLOCKED: true while a modal covers the toolbar."
  (let ((widgets (list (ui:make-widget :panel
                                       :toolbar
                                       0f0
                                       0f0
                                       (float (app-window-width app) 1f0)
                                       ui:+toolbar-height+)))
        (x +button-gap+)
        (y (+ 4f0 0f0)))
    (dolist (spec (toolbar-specs app))
      (if (null spec)
          (incf x +group-gap+)
          (destructuring-bind (id label action hint active enabled) spec
            (let ((button (make-button id
                                       label
                                       x
                                       y
                                       :action action
                                       :active active
                                       :enabled enabled
                                       :blocked blocked
                                       :hint hint)))
              (push button widgets)
              (incf x (+ (ui:widget-width button) +button-gap+))))))
    (nreverse widgets)))

;;; Layers

(defun layers-panel-widgets (app
                             rect
                             blocked)
  "Build the layers panel: heading, one row per layer with visibility and lock toggles, and the
Add, Rename, Delete and Color buttons.

APP: the application.
RECT: the panel rectangle.
BLOCKED: true while a modal covers the panel."
  (let* ((drawing (app-drawing app))
         (x (ui:rect-x rect))
         (y (ui:rect-y rect))
         (width (ui:rect-width rect))
         (height (ui:rect-height rect))
         (layers (dom:layers-in-order drawing))
         (current (dom:drawing-current-layer-id drawing))
         (button-height ui:+row-height+)
         (rows-top (+ y ui:+row-height+ 2f0))
         (rows-bottom (- (+ y height) button-height ui:+padding+ 2f0))
         (capacity (max 0 (floor (- rows-bottom rows-top) ui:+row-height+)))
         (widgets (list (ui:make-widget :panel
                                        :layers-panel
                                        x
                                        y
                                        width
                                        height)
                        (ui:make-widget :label
                                        :layers-title
                                        (+ x 0f0)
                                        y
                                        width
                                        ui:+row-height+
                                        :label "Layers"
                                        :align :left
                                        :color (scene:theme-accent (app-theme app))))))
    (setf (app-layer-scroll app) (max 0 (min (app-layer-scroll app) (max 0 (- (length layers) capacity)))))
    (loop for layer in (nthcdr (app-layer-scroll app) layers)
          for row from 0 below capacity
          do (let* ((row-y (+ rows-top (* row ui:+row-height+)))
                    (id (dom:layer-id layer))
                    (toggle-width ui:+row-height+))
               (push (ui:make-widget :row
                                     (list :layer id)
                                     x
                                     row-y
                                     (- width (* 2 toggle-width) 4f0)
                                     ui:+row-height+
                                     :label (dom:layer-name layer)
                                     :swatch (scene:rgb->rgba (dom:layer-color layer))
                                     :active (= id current)
                                     :align :left
                                     :blocked blocked
                                     :action (let ((id id)) (lambda () (operation-set-current-layer app id)))
                                     :hint "Click to make this the current layer")
                     widgets)
               (push (ui:make-widget :button
                                     (list :layer-visible id)
                                     (+ x (- width (* 2 toggle-width) 2f0))
                                     row-y
                                     toggle-width
                                     ui:+row-height+
                                     :label "V"
                                     :active (dom:layer-visible layer)
                                     :blocked blocked
                                     :action (let ((id id)) (lambda () (operation-toggle-layer-visible app id)))
                                     :hint "Layer visible")
                     widgets)
               (push (ui:make-widget :button
                                     (list :layer-locked id)
                                     (+ x (- width toggle-width))
                                     row-y
                                     toggle-width
                                     ui:+row-height+
                                     :label "L"
                                     :active (dom:layer-locked layer)
                                     :blocked blocked
                                     :action (let ((id id)) (lambda () (operation-toggle-layer-locked app id)))
                                     :hint "Layer locked")
                     widgets)))
    (let* ((count 4)
           (button-width (/ (- width (* (1+ count) ui:+padding+)) count))
           (button-y (- (+ y height) button-height ui:+padding+)))
      (loop for (id label action hint) in
            (list (list :layer-add "Add"
                        (lambda () (open-text-prompt app
                                                     "New layer"
                                                     "Layer name:"
                                                     ""
                                                     (lambda (text) (operation-add-layer app text))))
                        "Create a layer")
                  (list :layer-rename "Rename"
                        (lambda () (open-text-prompt app
                                                     "Rename layer"
                                                     "New name:"
                                                     (dom:layer-name (dom:find-layer drawing current))
                                                     (lambda (text) (operation-rename-layer app
                                                                                            current
                                                                                            text))))
                        "Rename the current layer")
                  (list :layer-delete "Delete" (lambda () (operation-delete-layer app current))
                        "Delete the current layer if it is empty")
                  (list :layer-color "Color" (lambda () (operation-cycle-layer-color app current))
                        "Change the color of the current layer"))
            for index from 0
            do (push (ui:make-widget :button
                                     id
                                     (+ x ui:+padding+ (* index (+ button-width ui:+padding+)))
                                     button-y
                                     button-width
                                     button-height
                                     :label label
                                     :action action
                                     :blocked blocked
                                     :hint hint)
                     widgets)))
    (nreverse widgets)))

;;; Inspector

(defun format-property-value (app
                              property)
  "Return the text that shows a property's value in the inspector and in its edit field.

APP: the application.
PROPERTY: an inspector property."
  (let ((value (dom:property-value property)))
    (ecase (dom:property-kind property)
      ((:coordinate :length) (format-model-number app value))
      (:text value)
      (:integer (format nil "~d" value))
      (:boolean (if value "Yes" "No"))
      (:unit (base:unit-abbreviation value)))))

(defun format-derived-value (app
                             derived)
  "Return the text of a read-only derived quantity, with its unit.

APP: the application.
DERIVED: a derived-value."
  (let ((unit (dom:drawing-display-unit (app-drawing app)))
        (value (dom:derived-value-value derived)))
    (ecase (dom:derived-value-kind derived)
      (:length (format nil "~a ~a" (format-model-number app value) (base:unit-abbreviation unit)))
      (:area (format nil "~a ~a" (base:format-area value
                                                   unit
                                                   :precision 4) (base:unit-area-abbreviation unit)))
      (:angle (format nil "~a~a" (base:format-decimal (base:radians->degrees value) 2) *degree-sign*))
      (:count (format nil "~d" value)))))

(defun next-layer-id-after (app
                            layer-id)
  "Return the id of the layer after LAYER-ID in display order, wrapping around.

APP: the application.
LAYER-ID: a layer id."
  (let* ((ids (mapcar #'dom:layer-id (dom:layers-in-order (app-drawing app))))
         (position (position layer-id ids)))
    (nth (if position (mod (1+ position) (length ids)) 0) ids)))

(defun property-widgets (app
                         entity
                         property
                         x
                         y
                         width
                         blocked)
  "Build the label and value widget of one editable property.

APP: the application.
ENTITY: the selected entity.
PROPERTY: the inspector property.
X: left edge of the row.
Y: top edge of the row.
WIDTH: row width.
BLOCKED: true while a modal covers the panel."
  (let* ((id (list :property (dom:property-key property)))
         (label-width (* 0.42f0 width))
         (key (dom:property-key property))
         (entity-id (dom:entity-id entity))
         (kind (dom:property-kind property))
         (unit (dom:drawing-display-unit (app-drawing app)))
         (text (format-property-value app property))
         (label (ui:make-widget :label
                                (list :label id)
                                x
                                y
                                label-width
                                ui:+row-height+
                                :label (dom:property-label property)
                                :align :left
                                :color (scene:theme-text-dim (app-theme app))))
         (value-x (+ x label-width))
         (value-width (- width label-width ui:+padding+)))
    (list label
          (case kind
            (:boolean
             (ui:make-widget :button
                             id
                             value-x
                             y
                             value-width
                             ui:+row-height+
                             :label text
                             :active (dom:property-value property)
                             :blocked blocked
                             :action (lambda ()
                                       (operation-edit-property app
                                                                entity-id
                                                                key
                                                                (not (dom:property-value property))))
                             :hint "Click to toggle"))
            (:unit
             (ui:make-widget :button
                             id
                             value-x
                             y
                             value-width
                             ui:+row-height+
                             :label text
                             :blocked blocked
                             :action (lambda ()
                                       (let* ((units (base:supported-length-units))
                                              (next (or (second (member (dom:property-value property) units))
                                                        (first units))))
                                         (operation-edit-property app
                                                                  entity-id
                                                                  key
                                                                  next)))
                             :hint "Click to change the unit"))
            (t
             (ui:make-widget :field
                             id
                             value-x
                             y
                             value-width
                             ui:+row-height+
                             :label text
                             :blocked blocked
                             :edit (and (focused-p app id) (app-edit app))
                             :action (lambda ()
                                       (start-field-edit app
                                                         id
                                                         text
                                                         (lambda (typed)
                                                           (handler-case
                                                               (operation-edit-property app
                                                                                        entity-id
                                                                                        key
                                                                                        (parse-property-input kind
                                                                                                              typed
                                                                                                              unit))
                                                             (base:invalid-input (condition)
                                                               (report-error app
                                                                             condition
                                                                             "property-edit"))))))
                             :hint "Click to edit; Enter commits, Esc cancels"))))))

(defun field-group (app
                    id
                    label
                    text
                    width
                    blocked
                    commit)
  "Build the label and text field of one drawing-wide setting, positioned relative to the row.

APP: the application.
ID: field id.
LABEL: setting name.
TEXT: the value shown.
WIDTH: row width.
BLOCKED: true while a modal covers the panel.
COMMIT: function of the typed string, called when the edit is committed."
  (let ((label-width (* 0.42f0 width)))
    (list (ui:make-widget :label
                          (list :label id)
                          0f0
                          0f0
                          label-width
                          ui:+row-height+
                          :label label
                          :align :left
                          :color (scene:theme-text-dim (app-theme app)))
          (ui:make-widget :field
                          id
                          label-width
                          0f0
                          (- width label-width ui:+padding+)
                          ui:+row-height+
                          :label text
                          :blocked blocked
                          :edit (and (focused-p app id) (app-edit app))
                          :action (lambda ()
                                    (start-field-edit app
                                                      id
                                                      text
                                                      (lambda (typed)
                                                        (handler-case (funcall commit typed)
                                                          (base:borba-cad-error (condition)
                                                            (report-error app
                                                                          condition
                                                                          "setting"))))))
                          :hint "Click to edit; Enter commits, Esc cancels"))))

(defun settings-groups (app
                        width
                        blocked)
  "Build the drawing-wide settings shown when nothing is selected, as rows of widgets
positioned relative to the row.

APP: the application.
WIDTH: row width.
BLOCKED: true while a modal covers the panel."
  (let* ((drawing (app-drawing app))
         (unit (dom:drawing-display-unit drawing))
         (dim (scene:theme-text-dim (app-theme app))))
    (list (list (ui:make-widget :label
                                :settings-info
                                0f0
                                0f0
                                width
                                ui:+row-height+
                                :label (format nil "~d entities, ~d layers"
                                               (dom:drawing-entity-count drawing)
                                               (length (dom:layers-in-order drawing)))
                                :align :left
                                :color dim))
          (field-group app
                       :setting-grid
                       "Grid spacing"
                       (format-model-number app (dom:drawing-grid-spacing drawing))
                       width
                       blocked
                       (lambda (typed)
                         (dom:set-grid-spacing drawing (base:parse-length typed unit))
                         (app-request-redraw app)))
          (field-group app
                       :setting-text-height
                       "Text height"
                       (format-model-number app (dom:drawing-default-text-height drawing))
                       width
                       blocked
                       (lambda (typed)
                         (dom:set-default-text-height drawing (base:parse-length typed unit))
                         (app-request-redraw app)))
          (list (ui:make-widget :label
                                :settings-hint
                                0f0
                                0f0
                                width
                                ui:+row-height+
                                :label "Select an entity to edit it."
                                :align :left
                                :color dim)))))

(defun layer-cycle-group (app
                          entities
                          width
                          blocked)
  "Build the row that shows the layer of the selection and moves it to the next layer on click.

APP: the application.
ENTITIES: the selected entities.
WIDTH: row width.
BLOCKED: true while a modal covers the panel."
  (let* ((layer-ids (remove-duplicates (mapcar #'dom:entity-layer-id entities)))
         (layer (and (= 1 (length layer-ids)) (dom:find-layer (app-drawing app) (first layer-ids))))
         (label-width (* 0.42f0 width)))
    (list (ui:make-widget :label
                          :layer-label
                          0f0
                          0f0
                          label-width
                          ui:+row-height+
                          :label "Layer"
                          :align :left
                          :color (scene:theme-text-dim (app-theme app)))
          (ui:make-widget :button
                          :layer-cycle
                          label-width
                          0f0
                          (- width label-width ui:+padding+)
                          ui:+row-height+
                          :label (if layer (dom:layer-name layer) "(several)")
                          :swatch (and layer (scene:rgb->rgba (dom:layer-color layer)))
                          :blocked blocked
                          :align :left
                          :action (lambda ()
                                    (operation-assign-selection-layer
                                     app (next-layer-id-after app (first layer-ids))))
                          :hint "Click to move the selection to the next layer"))))

(defun entity-groups (app
                      entity
                      width
                      blocked)
  "Build the inspector rows of one selected entity: title, layer, properties, derived values.

APP: the application.
ENTITY: the selected entity.
WIDTH: row width.
BLOCKED: true while a modal covers the panel."
  (append
   (list (list (ui:make-widget :label
                               :entity-title
                               0f0
                               0f0
                               width
                               ui:+row-height+
                               :label (format nil "~:(~a~) #~d" (dom:entity-kind entity) (dom:entity-id entity))
                               :align :left))
         (layer-cycle-group app
                            (list entity)
                            width
                            blocked))
   (loop for property in (dom:entity-properties entity)
         collect (property-widgets app
                                   entity
                                   property
                                   0f0
                                   0f0
                                   width
                                   blocked))
   (loop for derived in (dom:entity-derived-values entity)
         collect (list (ui:make-widget :label
                                       (list :derived (dom:derived-value-label derived))
                                       0f0
                                       0f0
                                       width
                                       ui:+row-height+
                                       :label (format nil "~a: ~a" (dom:derived-value-label derived)
                                                      (format-derived-value app derived))
                                       :align :left
                                       :color (scene:theme-text-dim (app-theme app)))))))

(defun inspector-groups (app
                         width
                         blocked)
  "Build the inspector content for the current selection as rows of widgets positioned
relative to the row.

APP: the application.
WIDTH: row width.
BLOCKED: true while a modal covers the panel."
  (let ((selected (selected-entities app)))
    (cond ((null selected)
           (settings-groups app
                            width
                            blocked))
          ((null (rest selected))
           (entity-groups app
                          (first selected)
                          width
                          blocked))
          (t (list (list (ui:make-widget :label
                                         :multi-title
                                         0f0
                                         0f0
                                         width
                                         ui:+row-height+
                                         :label (format nil "~d entities selected" (length selected))
                                         :align :left))
                   (layer-cycle-group app
                                      selected
                                      width
                                      blocked))))))

(defun inspector-widgets (app
                          rect
                          blocked)
  "Build the property inspector: heading plus the rows for the selection, scrolled and clipped
to the panel.

APP: the application.
RECT: the panel rectangle.
BLOCKED: true while a modal covers the panel."
  (let* ((x (+ (ui:rect-x rect) ui:+padding+))
         (y (ui:rect-y rect))
         (width (- (ui:rect-width rect) (* 2 ui:+padding+)))
         (bottom (+ y (ui:rect-height rect)))
         (groups (inspector-groups app
                                   width
                                   blocked))
         (scroll (max 0 (min (app-inspector-scroll app) (max 0 (1- (length groups))))))
         (row-y (+ y ui:+row-height+ 2f0))
         (widgets (list (ui:make-widget :panel
                                        :inspector-panel
                                        (ui:rect-x rect)
                                        y
                                        (ui:rect-width rect)
                                        (ui:rect-height rect))
                        (ui:make-widget :label
                                        :inspector-title
                                        x
                                        y
                                        width
                                        ui:+row-height+
                                        :label "Properties"
                                        :align :left
                                        :color (scene:theme-accent (app-theme app))))))
    (setf (app-inspector-scroll app) scroll)
    (dolist (group (nthcdr scroll groups) widgets)
      (when (> (+ row-y ui:+row-height+) bottom)
        (return widgets))
      (dolist (widget group)
        (setf (ui:widget-y widget) row-y
              (ui:widget-x widget) (+ x (ui:widget-x widget))))
      (setf widgets (append widgets group))
      (incf row-y (+ ui:+row-height+ 2f0)))))

;;; Status bar

(defun cursor-text (app)
  "Return the pointer position and zoom as status text.

APP: the application."
  (let* ((cursor (app-cursor app))
         (unit (dom:drawing-display-unit (app-drawing app)))
         (zoom (format nil "~a px/~a"
                       (base:format-decimal (* (view:viewport-scale (app-viewport app))
                                               (base:unit->millimeters 1d0 unit))
                                            2)
                       (base:unit-abbreviation unit))))
    (if cursor
        (format nil "X ~a  Y ~a ~a | ~a~@[ | snap: ~(~a~)~]"
                (base:format-length (cursor-x cursor)
                                    unit
                                    :precision 3)
                (base:format-length (cursor-y cursor)
                                    unit
                                    :precision 3)
                (base:unit-abbreviation unit)
                zoom
                (cursor-snap-kind cursor))
        (format nil "~a" zoom))))

(defun app-status-texts (app
                         hover-hint)
  "Return the status bar content as three values: the left text, its severity (:info or :error)
and the right text.

APP: the application.
HOVER-HINT: hint of the widget under the pointer, or NIL."
  (let ((message (app-message app)))
    (values (cond (message (car message))
                  (hover-hint hover-hint)
                  (t (tool-prompt (app-tool app) app)))
            (if message (cdr message) :info)
            (cursor-text app))))

(defun status-widgets (app
                       hover-hint)
  "Build the status bar.

APP: the application.
HOVER-HINT: hint of the widget under the pointer, or NIL."
  (multiple-value-bind (left severity right)
      (app-status-texts app hover-hint)
    (let* ((width (float (app-window-width app) 1f0))
           (y (float (- (app-window-height app) ui:+status-height+) 1f0))
           (right-width (+ (ui:text-width-pixels right) (* 2 ui:+padding+)))
           (theme (app-theme app)))
      (list (ui:make-widget :panel
                            :status
                            0f0
                            y
                            width
                            ui:+status-height+)
            (ui:make-widget :label
                            :status-left
                            0f0
                            y
                            (- width right-width)
                            ui:+status-height+
                            :label left
                            :align :left
                            :color (if (eq severity :error) (scene:theme-danger theme) (scene:theme-text theme)))
            (ui:make-widget :label
                            :status-right
                            (- width right-width)
                            y
                            right-width
                            ui:+status-height+
                            :label right
                            :align :left
                            :color (scene:theme-text-dim theme))))))

;;; Modal prompt

(defun prompt-widgets (app)
  "Build the modal prompt: a dimming scrim, the dialog, its field and its buttons.

APP: the application."
  (let* ((prompt (app-prompt app))
         (theme (app-theme app))
         (window-width (float (app-window-width app) 1f0))
         (window-height (float (app-window-height app) 1f0))
         (text-p (eq (prompt-kind prompt) :text))
         (height (if text-p 190f0 150f0))
         (x (/ (- window-width +prompt-width+) 2))
         (y (/ (- window-height height) 3))
         (inner (- +prompt-width+ (* 2 ui:+padding+ 2)))
         (widgets (list (ui:make-widget :swatch
                                        :scrim
                                        0f0
                                        0f0
                                        window-width
                                        window-height
                                        :swatch (scene:make-rgba 0f0
                                                                 0f0
                                                                 0f0
                                                                 0.55f0))
                        (ui:make-widget :panel
                                        :prompt
                                        x
                                        y
                                        +prompt-width+
                                        height)
                        (ui:make-widget :label
                                        :prompt-title
                                        (+ x ui:+padding+)
                                        (+ y ui:+padding+)
                                        inner
                                        ui:+row-height+
                                        :label (prompt-title prompt)
                                        :align :left
                                        :color (scene:theme-accent theme))
                        (ui:make-widget :label
                                        :prompt-message
                                        (+ x ui:+padding+)
                                        (+ y ui:+padding+ ui:+row-height+ 4f0)
                                        inner
                                        ui:+row-height+
                                        :label (prompt-message prompt)
                                        :align :left))))
    (when text-p
      (setf widgets
            (append widgets
                    (list (ui:make-widget :field
                                          :prompt-field
                                          (+ x ui:+padding+ 4f0)
                                          (+ y 84f0)
                                          (- +prompt-width+ (* 2 ui:+padding+) 8f0)
                                          ui:+row-height+
                                          :label (ui:edit-state-text (prompt-edit prompt))
                                          :edit (prompt-edit prompt))))))
    (let* ((choices (prompt-choices prompt))
           (button-width 96f0)
           (button-y (- (+ y height) ui:+row-height+ ui:+padding+ 4f0))
           (buttons-width (* (length choices) (+ button-width ui:+padding+)))
           (start-x (- (+ x +prompt-width+) ui:+padding+ buttons-width (- ui:+padding+))))
      (loop for (label) in choices
            for index from 0
            do (let ((index index))
                 (setf widgets
                       (append widgets
                               (list (ui:make-widget :button
                                                     (list :prompt-button index)
                                                     (+ start-x (* index (+ button-width ui:+padding+)))
                                                     button-y
                                                     button-width
                                                     ui:+row-height+
                                                     :label label
                                                     :action (lambda () (choose-prompt-button app index)))))))))
    widgets))

(defun build-widgets (app)
  "Build every widget of the window, in drawing order, from the application state.

APP: the application."
  (let* ((layout (ui:compute-layout (app-window-width app) (app-window-height app)))
         (blocked (and (app-prompt app) t))
         (base-widgets (append (toolbar-widgets app blocked)
                               (layers-panel-widgets app
                                                     (ui:layout-layers-panel layout)
                                                     blocked)
                               (inspector-widgets app
                                                  (ui:layout-inspector-panel layout)
                                                  blocked)))
         (hovered (ui:widget-at base-widgets
                                (app-pointer-x app)
                                (app-pointer-y app)))
         (all (append base-widgets (status-widgets app (and hovered (ui:widget-hint hovered))))))
    (if (app-prompt app)
        (append all (prompt-widgets app))
        all)))
