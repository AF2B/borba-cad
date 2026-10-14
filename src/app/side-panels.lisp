(in-package #:borba-cad/app)

;;;; The side panel: a card with the layers and a card with the properties of the selection. Both
;;;; are rebuilt from the application state each time, like every other panel.

(defconstant +card-header-height+ 40f0
  "Height of the title row of a card, in pixels.")

(defconstant +layer-row-pitch+ 30f0
  "Distance between the tops of two rows of the layers card, in pixels.")

(defconstant +property-row-pitch+ 32f0
  "Distance between the tops of two rows of the properties card, in pixels.")

(defconstant +toggle-size+ 26f0
  "Side of the visibility and lock buttons of a layer row, in pixels.")

(defconstant +action-bar-height+ 44f0
  "Height of the strip of buttons at the foot of the layers card, in pixels.")

(defconstant +action-button-height+ 28f0
  "Height of the buttons in that strip, in pixels.")

(defconstant +label-fraction+ 0.4f0
  "Share of a property row taken by its label; the value gets the rest.")

(defconstant +scrollbar-width+ 3f0
  "Width of the scroll indicator of a card, in pixels.")

(defconstant +scrollbar-inset+ 3f0
  "Space between the scroll indicator and the right edge of its card, in pixels.")

(defconstant +scrollbar-opacity+ 0.6f0
  "Opacity of the scroll indicator.")

(defconstant +toggle-gap+ 2f0
  "Gap between the visibility and the lock buttons of a layer row, in pixels.")

(defconstant +card-content-gap+ 4f0
  "Space between the title row of a card and its content, and between its content and its foot.")

(defconstant +value-gap+ 4f0
  "Gap between the label of an inspector row and its value, in pixels.")

(defconstant +minimum-thumb-height+ 24f0
  "Shortest scroll indicator, in pixels.")

(defparameter *degree-sign* (string (code-char 176))
  "The degree sign, built from its code point to keep sources ASCII.")

;;; Cards

(defun card-bounds (rect
                    top-margin
                    bottom-margin)
  "Return the left edge, top edge, width and height of the card that sits in a part of the side
panel.

RECT: the part of the side panel.
TOP-MARGIN: gap above the card.
BOTTOM-MARGIN: gap below the card."
  (values (+ (ui:rect-x rect) ui:+panel-margin+)
          (+ (ui:rect-y rect) top-margin)
          (- (ui:rect-width rect) (* 2 ui:+panel-margin+))
          (- (ui:rect-height rect) top-margin bottom-margin)))

(defun card-header-widgets (id
                            icon
                            title
                            detail
                            x
                            y
                            width
                            height
                            theme
                            &key (detail-inset 0f0))
  "Build the surface of a card and its title row: an icon, the title, and a figure on the right.

ID: card id.
ICON: icon name.
TITLE: heading text.
DETAIL: the figure at the right of the title row, or NIL.
X: left edge of the card.
Y: top edge of the card.
WIDTH: width of the card.
HEIGHT: height of the card.
THEME: the theme.
DETAIL-INSET: how far the figure keeps from the right edge of the title row, for a button there."
  (list (ui:make-widget :card
                        id
                        x
                        y
                        width
                        height)
        (make-label (list id :title)
                    title
                    (+ x ui:+padding+)
                    y
                    (- width (* 2 ui:+padding+))
                    +card-header-height+
                    :font :ui-strong
                    :icon icon
                    :color (scene:theme-text theme))
        (make-label (list id :detail)
                    (or detail "")
                    (+ x ui:+padding+)
                    y
                    (- width (* 2 ui:+padding+))
                    +card-header-height+
                    :align :right
                    :size ui:+small-font-pixels+
                    :color (scene:theme-text-faint theme)
                    :trailing detail-inset)))

(defun scrollbar-widget (id
                         card-right
                         top
                         bottom
                         total
                         visible
                         offset
                         theme)
  "Build the thin indicator that shows which part of a long list is in view, or NIL when the
whole list fits.

ID: widget id.
CARD-RIGHT: x of the right edge of the card.
TOP: y where the list begins.
BOTTOM: y where the list ends.
TOTAL: number of items.
VISIBLE: number of items in view.
OFFSET: number of items scrolled past.
THEME: the theme."
  (when (and (> total visible) (plusp visible))
    (let* ((track (- bottom top))
           (thumb (max +minimum-thumb-height+ (* track (/ (float visible 1f0) total))))
           (travel (- track thumb))
           (position (* travel (/ (float offset 1f0) (max 1 (- total visible))))))
      (ui:make-widget :swatch
                      id
                      (- card-right +scrollbar-width+ +scrollbar-inset+)
                      (+ top position)
                      +scrollbar-width+
                      thumb
                      :swatch (scene:with-alpha (scene:theme-text-faint theme) +scrollbar-opacity+)))))

;;; Layers

(defun layer-toggle-widgets (app
                             layer
                             right
                             row-y
                             blocked)
  "Build the visibility and lock buttons of one layer row.

APP: the application.
LAYER: the layer.
RIGHT: x of the right edge of the row.
ROW-Y: top edge of the row.
BLOCKED: true while a modal covers the panel."
  (let* ((theme (app-theme app))
         (id (dom:layer-id layer))
         (visible (dom:layer-visible layer))
         (locked (dom:layer-locked layer))
         (y (+ row-y (/ (- ui:+row-height+ +toggle-size+) 2))))
    (list (make-icon-button (list :layer-visible id)
                            (if visible :eye :eye-off)
                            (- right (* 2 +toggle-size+) +toggle-gap+)
                            y
                            :action (lambda () (operation-toggle-layer-visible app id))
                            :active visible
                            :blocked blocked
                            :hint "Show or hide the layer"
                            :variant :ghost
                            :size +toggle-size+
                            :color (if visible
                                       (scene:theme-text-dim theme)
                                       (scene:theme-text-faint theme)))
          (make-icon-button (list :layer-locked id)
                            (if locked :lock :unlock)
                            (- right +toggle-size+)
                            y
                            :action (lambda () (operation-toggle-layer-locked app id))
                            :active locked
                            :blocked blocked
                            :hint "Lock or unlock the layer"
                            :variant :ghost
                            :size +toggle-size+
                            :color (if locked
                                       (scene:theme-warning theme)
                                       (scene:theme-text-faint theme))))))

(defun add-layer-button (app
                         card-right
                         card-top
                         blocked)
  "Build the button at the right of the layers card's title that asks for a new layer's name.

APP: the application.
CARD-RIGHT: x of the right edge of the card.
CARD-TOP: y of the top edge of the card.
BLOCKED: true while a modal covers the card."
  (make-icon-button :layer-add
                    :plus
                    (- card-right ui:+padding+ +toggle-size+)
                    (+ card-top (/ (- +card-header-height+ +toggle-size+) 2))
                    :action (lambda ()
                              (open-text-prompt app
                                                "New layer"
                                                "Layer name:"
                                                ""
                                                (lambda (text) (operation-add-layer app text))))
                    :blocked blocked
                    :hint "Create a layer"
                    :variant :ghost
                    :size +toggle-size+
                    :color (scene:theme-text (app-theme app))))

(defun layer-action-specs (app)
  "Return the buttons at the foot of the layers card as (id label icon action hint).

APP: the application."
  (let* ((drawing (app-drawing app))
         (current (dom:drawing-current-layer-id drawing)))
    (list (list :layer-rename "Rename" :edit
                (lambda ()
                  (open-text-prompt app
                                    "Rename layer"
                                    "New name:"
                                    (dom:layer-name (dom:find-layer drawing current))
                                    (lambda (text) (operation-rename-layer app
                                                                           current
                                                                           text))))
                "Rename the current layer")
          (list :layer-delete "Delete" :trash
                (lambda () (operation-delete-layer app current))
                "Delete the current layer if it is empty")
          (list :layer-color "Color" :palette
                (lambda () (operation-cycle-layer-color app current))
                "Change the color of the current layer"))))

(defun layers-panel-widgets (app
                             rect
                             blocked)
  "Build the layers card: its title, one row per layer with visibility and lock buttons, and the
Add, Rename, Delete and Color buttons.

APP: the application.
RECT: the part of the side panel the card sits in.
BLOCKED: true while a modal covers the card."
  (multiple-value-bind (x y width height)
      (card-bounds rect
                   ui:+panel-margin+
                   (/ ui:+panel-margin+ 2))
    (let* ((theme (app-theme app))
           (drawing (app-drawing app))
           (layers (dom:layers-in-order drawing))
           (current (dom:drawing-current-layer-id drawing))
           (inner-x (+ x ui:+padding+))
           (inner-width (- width (* 2 ui:+padding+)))
           (rows-top (+ y +card-header-height+ +card-content-gap+))
           (actions-top (- (+ y height) +action-bar-height+))
           (rows-bottom (- actions-top +card-content-gap+))
           (capacity (max 0 (floor (- rows-bottom rows-top) +layer-row-pitch+)))
           (widgets (reverse (card-header-widgets :layers-card
                                                  :layers
                                                  "Layers"
                                                  (format nil "~d" (length layers))
                                                  x
                                                  y
                                                  width
                                                  height
                                                  theme
                                                  :detail-inset (+ +toggle-size+ ui:+gap+)))))
      (flet ((add (widget)
               (when widget
                 (push widget widgets))))
        (add (add-layer-button app
                               (+ x width)
                               y
                               blocked))
        (setf (app-layer-scroll app) (max 0 (min (app-layer-scroll app) (max 0 (- (length layers) capacity)))))
        (loop for layer in (nthcdr (app-layer-scroll app) layers)
              for row from 0 below capacity
              do (let ((row-y (+ rows-top (* row +layer-row-pitch+)))
                       (id (dom:layer-id layer)))
                   (add (ui:make-widget :row
                                        (list :layer id)
                                        inner-x
                                        row-y
                                        inner-width
                                        ui:+row-height+
                                        :label (dom:layer-name layer)
                                        :swatch (scene:rgb->rgba (dom:layer-color layer))
                                        :active (= id current)
                                        :align :left
                                        :blocked blocked
                                        :trailing (+ (* 2 +toggle-size+) +toggle-gap+ ui:+padding+)
                                        :action (lambda () (operation-set-current-layer app id))
                                        :hint "Click to make this the current layer"))
                   (dolist (toggle (layer-toggle-widgets app
                                                         layer
                                                         (+ inner-x inner-width)
                                                         row-y
                                                         blocked))
                     (add toggle))))
        (add (scrollbar-widget :layers-scrollbar
                               (+ x width)
                               rows-top
                               rows-bottom
                               (length layers)
                               capacity
                               (app-layer-scroll app)
                               theme))
        (add (make-separator :layers-separator
                             inner-x
                             actions-top
                             inner-width
                             1f0))
        (let* ((count (length (layer-action-specs app)))
               (gap ui:+gap+)
               (button-width (/ (- inner-width (* (1- count) gap)) count)))
          (loop for (id label icon action hint) in (layer-action-specs app)
                for index from 0
                do (add (ui:make-widget :button
                                        id
                                        (+ inner-x (* index (+ button-width gap)))
                                        (+ actions-top (/ (- +action-bar-height+ +action-button-height+) 2))
                                        button-width
                                        +action-button-height+
                                        :label label
                                        :icon icon
                                        :action action
                                        :blocked blocked
                                        :hint hint
                                        :variant :ghost
                                        :size ui:+small-font-pixels+)))))
      (nreverse widgets))))

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

(defun property-label (app
                       id
                       text
                       width)
  "Build the dimmed name at the left of an inspector row, positioned relative to the row.

APP: the application.
ID: widget id.
TEXT: the name.
WIDTH: width of the row."
  (make-label id
              text
              0f0
              0f0
              (* +label-fraction+ width)
              ui:+row-height+
              :size ui:+small-font-pixels+
              :color (scene:theme-text-dim (app-theme app))))

(defun value-x (width)
  "Return where the value column of an inspector row begins, relative to the row.

WIDTH: width of the row."
  (+ (* +label-fraction+ width) +value-gap+))

(defun property-widgets (app
                         entity
                         property
                         width
                         blocked)
  "Build the label and value widget of one editable property, positioned relative to the row.

APP: the application.
ENTITY: the selected entity.
PROPERTY: the inspector property.
WIDTH: row width.
BLOCKED: true while a modal covers the panel."
  (let* ((id (list :property (dom:property-key property)))
         (key (dom:property-key property))
         (entity-id (dom:entity-id entity))
         (kind (dom:property-kind property))
         (unit (dom:drawing-display-unit (app-drawing app)))
         (text (format-property-value app property))
         (column (value-x width))
         (value-width (- width column)))
    (list (property-label app
                          (list :label id)
                          (dom:property-label property)
                          width)
          (case kind
            (:boolean
             (ui:make-widget :button
                             id
                             column
                             0f0
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
                             column
                             0f0
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
                             column
                             0f0
                             value-width
                             ui:+row-height+
                             :label text
                             :align :left
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
  (let ((column (value-x width)))
    (list (property-label app
                          (list :label id)
                          label
                          width)
          (ui:make-widget :field
                          id
                          column
                          0f0
                          (- width column)
                          ui:+row-height+
                          :label text
                          :align :left
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

(defun note-widget (app
                    id
                    text
                    width)
  "Build a line of dimmed explanatory text, positioned relative to the row.

APP: the application.
ID: widget id.
TEXT: what it says.
WIDTH: row width."
  (make-label id
              text
              0f0
              0f0
              width
              ui:+row-height+
              :size ui:+small-font-pixels+
              :color (scene:theme-text-faint (app-theme app))))

(defun settings-groups (app
                        width
                        blocked)
  "Build the drawing-wide settings shown when nothing is selected, as rows of widgets
positioned relative to the row.

APP: the application.
WIDTH: row width.
BLOCKED: true while a modal covers the panel."
  (let* ((drawing (app-drawing app))
         (unit (dom:drawing-display-unit drawing)))
    (list (list (note-widget app
                             :settings-info
                             (format nil "~d entities, ~d layers"
                                     (dom:drawing-entity-count drawing)
                                     (length (dom:layers-in-order drawing)))
                             width))
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
          (list (note-widget app
                             :settings-hint
                             "Select an entity to edit it."
                             width)))))

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
         (column (value-x width)))
    (list (property-label app
                          :layer-label
                          "Layer"
                          width)
          (ui:make-widget :button
                          :layer-cycle
                          column
                          0f0
                          (- width column)
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
  (let ((theme (app-theme app)))
    (append
     (list (list (make-label :entity-title
                             (format nil "~:(~a~) #~d" (dom:entity-kind entity) (dom:entity-id entity))
                             0f0
                             0f0
                             width
                             ui:+row-height+
                             :font :ui-strong
                             :icon (dom:entity-kind entity)
                             :color (scene:theme-accent theme)))
           (layer-cycle-group app
                              (list entity)
                              width
                              blocked))
     (loop for property in (dom:entity-properties entity)
           collect (property-widgets app
                                     entity
                                     property
                                     width
                                     blocked))
     (loop for derived in (dom:entity-derived-values entity)
           collect (list (make-label (list :derived (dom:derived-value-label derived))
                                     (format nil "~a: ~a" (dom:derived-value-label derived)
                                             (format-derived-value app derived))
                                     0f0
                                     0f0
                                     width
                                     ui:+row-height+
                                     :size ui:+small-font-pixels+
                                     :color (scene:theme-text-dim theme)))))))

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
          (t (list (list (make-label :multi-title
                                     (format nil "~d entities selected" (length selected))
                                     0f0
                                     0f0
                                     width
                                     ui:+row-height+
                                     :font :ui-strong
                                     :color (scene:theme-accent (app-theme app))))
                   (layer-cycle-group app
                                      selected
                                      width
                                      blocked))))))

(defun inspector-widgets (app
                          rect
                          blocked)
  "Build the properties card: its title and the rows for the selection, scrolled and clipped
to the card.

APP: the application.
RECT: the part of the side panel the card sits in.
BLOCKED: true while a modal covers the card."
  (multiple-value-bind (x y width height)
      (card-bounds rect
                   (/ ui:+panel-margin+ 2)
                   ui:+panel-margin+)
    (let* ((theme (app-theme app))
           (inner-x (+ x ui:+padding+))
           (inner-width (- width (* 2 ui:+padding+)))
           (top (+ y +card-header-height+ +card-content-gap+))
           (bottom (- (+ y height) ui:+padding+))
           (groups (inspector-groups app
                                     inner-width
                                     blocked))
           (capacity (max 1 (floor (- bottom top) +property-row-pitch+)))
           (scroll (max 0 (min (app-inspector-scroll app) (max 0 (- (length groups) capacity)))))
           (selected (selected-entities app))
           (widgets (reverse (card-header-widgets :inspector-card
                                                  :edit
                                                  "Properties"
                                                  (cond ((null selected) "Drawing")
                                                        ((rest selected) "Selection")
                                                        (t "Entity"))
                                                  x
                                                  y
                                                  width
                                                  height
                                                  theme)))
           (row-y top))
      (setf (app-inspector-scroll app) scroll)
      (dolist (group (nthcdr scroll groups))
        (when (> (+ row-y ui:+row-height+) bottom)
          (return))
        (dolist (widget group)
          (setf (ui:widget-y widget) row-y
                (ui:widget-x widget) (+ inner-x (ui:widget-x widget)))
          (push widget widgets))
        (incf row-y +property-row-pitch+))
      (let ((thumb (scrollbar-widget :inspector-scrollbar
                                     (+ x width)
                                     top
                                     bottom
                                     (length groups)
                                     capacity
                                     scroll
                                     theme)))
        (when thumb
          (push thumb widgets)))
      (nreverse widgets))))
