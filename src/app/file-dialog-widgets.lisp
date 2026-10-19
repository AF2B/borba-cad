(in-package #:borba-cad/app)

;;;; The widgets of the file dialog, built from its state every time the window is drawn: the
;;;; title row, the side column of places and recent files, the folder trail and search field, the
;;;; list, and the file name and buttons at the foot. What the widgets do is in file-dialog.lisp.

(defun dialog-chrome-widgets (app
                              dialog
                              layout
                              blocked)
  "Build the veil, the card, the title row and the line under it.

APP: the application.
DIALOG: the file dialog.
LAYOUT: the dialog's layout.
BLOCKED: true while a prompt covers the dialog."
  (let ((theme (app-theme app))
        (x (browser-layout-x layout))
        (y (browser-layout-y layout))
        (width (browser-layout-width layout)))
    (list (ui:make-widget :scrim
                          :dialog-scrim
                          0f0
                          0f0
                          (float (app-window-width app) 1f0)
                          (float (app-window-height app) 1f0))
          (ui:make-widget :card
                          :dialog
                          x
                          y
                          width
                          (browser-layout-height layout))
          (make-label :dialog-title
                      (dialog-title dialog)
                      (+ x +browser-inset+)
                      y
                      (- width (* 2 +browser-inset+) +browser-control-height+)
                      +browser-header-height+
                      :font :ui-strong
                      :size ui:+heading-font-pixels+
                      :color (scene:theme-text theme))
          (make-icon-button :dialog-close
                            :close
                            (- (+ x width) +browser-inset+ +toggle-size+)
                            (+ y (/ (- +browser-header-height+ +toggle-size+) 2))
                            :action (lambda () (close-file-dialog app))
                            :blocked blocked
                            :hint (i18n:tr "Close")
                            :shortcut "Esc"
                            :variant :ghost
                            :size +toggle-size+)
          (make-separator :dialog-header-line
                          (+ x +browser-inset+)
                          (+ y +browser-header-height+)
                          (- width (* 2 +browser-inset+))
                          +separator-width+))))

(defun place-row (dialog
                  id
                  label
                  icon
                  path
                  x
                  y
                  blocked)
  "Build a row of the side column that takes the dialog to a folder.

DIALOG: the file dialog.
ID: widget id.
LABEL: the folder's name.
ICON: icon name.
PATH: the folder's pathname.
X: left edge.
Y: top edge.
BLOCKED: true while a prompt covers the dialog."
  (ui:make-widget :row
                  id
                  x
                  y
                  +browser-sidebar-width+
                  +sidebar-row-height+
                  :label label
                  :icon icon
                  :active (equal (file-dialog-directory dialog) path)
                  :align :left
                  :blocked blocked
                  :action (lambda ()
                            (when (open-directory dialog path)
                              (leave-location-edit dialog)))
                  :hint (uiop:native-namestring path)))

(defun sidebar-widgets (app
                        dialog
                        layout
                        blocked)
  "Build the side column: the places, and under them the recent files.

APP: the application.
DIALOG: the file dialog.
LAYOUT: the dialog's layout.
BLOCKED: true while a prompt covers the dialog."
  (let* ((theme (app-theme app))
         (x (browser-layout-side-x layout))
         (y (browser-layout-body-top layout))
         (widgets '())
         (recent (subseq (file-dialog-recent dialog)
                         0
                         (min +recent-rows-shown+ (length (file-dialog-recent dialog))))))
    (flet ((add (widget)
             (push widget widgets))
           (heading (id text)
             (make-label id
                         text
                         (+ x ui:+padding+)
                         y
                         +browser-sidebar-width+
                         +sidebar-heading-height+
                         :size ui:+small-font-pixels+
                         :color (scene:theme-text-faint theme))))
      (add (heading :dialog-places-heading (i18n:tr "PLACES")))
      (incf y +sidebar-heading-height+)
      (loop for (label icon path) in (file-dialog-places dialog)
            for index from 0
            do (add (place-row dialog
                               (list :place index)
                               label
                               icon
                               path
                               x
                               y
                               blocked))
               (incf y +sidebar-row-height+))
      (when recent
        (incf y ui:+gap+)
        (add (heading :dialog-recent-heading (i18n:tr "RECENT")))
        (incf y +sidebar-heading-height+)
        (loop for path in recent
              for index from 0
              do (add (ui:make-widget :row
                                      (list :recent index)
                                      x
                                      y
                                      +browser-sidebar-width+
                                      +sidebar-row-height+
                                      :label (file-namestring path)
                                      :icon :clock
                                      :align :left
                                      :blocked blocked
                                      :action (let ((path path))
                                                (lambda () (choose-recent-file app
                                                                               dialog
                                                                               path)))
                                      :hint (uiop:native-namestring path)))
                 (incf y +sidebar-row-height+))))
    (nreverse widgets)))

(defun trail-steps (dialog
                    available)
  "Choose which steps of the folder trail to show so that they fit in AVAILABLE pixels: the
last ones, since those are the ones the user is near.

DIALOG: the file dialog.
AVAILABLE: width the trail may take."
  (let ((steps (breadcrumbs (file-dialog-directory dialog)))
        (shown '())
        (used 0f0))
    (loop for step in (reverse steps)
          do (let ((width (+ (ui:text-width-pixels (car step) ui:+small-font-pixels+)
                             (* 2 ui:+gap+)
                             +breadcrumb-separator-width+)))
               (when (and shown (> (+ used width) available))
                 (return))
               (push step shown)
               (incf used width)))
    shown))

(defun trail-widgets (app
                      dialog
                      layout
                      blocked)
  "Build the folder trail, each step a button that goes to that folder, and the button that
turns the trail into a field holding the path.

APP: the application.
DIALOG: the file dialog.
LAYOUT: the dialog's layout.
BLOCKED: true while a prompt covers the dialog."
  (let* ((theme (app-theme app))
         (top (browser-layout-body-top layout))
         (left (+ (browser-layout-main-x layout) +browser-control-height+ ui:+gap+))
         (right (- (+ (browser-layout-main-x layout) (browser-layout-main-width layout))
                   +browser-search-width+
                   ui:+gap+))
         (edit-size +browser-control-height+)
         (x left)
         (widgets '()))
    (if (file-dialog-location-edit dialog)
        (list (ui:make-widget :field
                              :dialog-location
                              left
                              top
                              (- right left)
                              +browser-control-height+
                              :label (ui:edit-state-text (file-dialog-location-edit dialog))
                              :align :left
                              :edit (and (eq (file-dialog-focus dialog) :location)
                                         (file-dialog-location-edit dialog))
                              :blocked blocked
                              :action (lambda () (setf (file-dialog-focus dialog) :location))))
        (progn
          (dolist (step (trail-steps dialog (- right left edit-size ui:+gap+)))
            (let* ((label (car step))
                   (width (+ (ui:text-width-pixels label ui:+small-font-pixels+) (* 2 ui:+gap+)))
                   (path (cdr step)))
              (push (ui:make-widget :button
                                    (list :crumb (length widgets))
                                    x
                                    (+ top (/ (- +browser-control-height+ +breadcrumb-button-height+) 2))
                                    width
                                    +breadcrumb-button-height+
                                    :label label
                                    :variant :ghost
                                    :size ui:+small-font-pixels+
                                    :blocked blocked
                                    :action (lambda ()
                                              (when (open-directory dialog path)
                                                (leave-location-edit dialog)))
                                    :hint (uiop:native-namestring path))
                    widgets)
              (incf x width)
              (push (ui:make-widget :icon
                                    (list :crumb-arrow (length widgets))
                                    (+ x (/ (- +breadcrumb-separator-width+ ui:+small-icon-size+) 2))
                                    (+ top (/ (- +browser-control-height+ ui:+small-icon-size+) 2))
                                    ui:+small-icon-size+
                                    ui:+small-icon-size+
                                    :icon :chevron-right
                                    :color (scene:theme-text-faint theme))
                    widgets)
              (incf x +breadcrumb-separator-width+)))
          (push (make-icon-button :dialog-edit-location
                                  :edit
                                  (- right edit-size)
                                  top
                                  :action (lambda () (start-location-edit dialog))
                                  :blocked blocked
                                  :hint (i18n:tr "Type a path")
                                  :shortcut "Ctrl+L"
                                  :variant :ghost
                                  :size edit-size)
                widgets)
          (nreverse widgets)))))

(defun toolbar-widgets-for-dialog (app
                                   dialog
                                   layout
                                   blocked)
  "Build the row above the list: the button to the parent folder, the trail and the search field.

APP: the application.
DIALOG: the file dialog.
LAYOUT: the dialog's layout.
BLOCKED: true while a prompt covers the dialog."
  (let* ((top (browser-layout-body-top layout))
         (main-x (browser-layout-main-x layout))
         (right (+ main-x (browser-layout-main-width layout)))
         (filter-edit (file-dialog-filter-edit dialog)))
    (append (list (make-icon-button :dialog-up
                                    :folder-up
                                    main-x
                                    top
                                    :action (lambda () (open-parent-directory dialog))
                                    :enabled (and (parent-directory (file-dialog-directory dialog)) t)
                                    :blocked blocked
                                    :hint (i18n:tr "Up one folder")
                                    :shortcut "Alt+Up"
                                    :variant :ghost
                                    :size +browser-control-height+))
            (trail-widgets app
                           dialog
                           layout
                           blocked)
            (list (ui:make-widget :field
                                  :dialog-search
                                  (- right +browser-search-width+)
                                  top
                                  +browser-search-width+
                                  +browser-control-height+
                                  :label (if (plusp (length (ui:edit-state-text filter-edit)))
                                             (ui:edit-state-text filter-edit)
                                             (i18n:tr "Search"))
                                  :align :left
                                  :color (and (zerop (length (ui:edit-state-text filter-edit)))
                                              (scene:theme-text-faint (app-theme app)))
                                  :edit (and (eq (file-dialog-focus dialog) :filter) filter-edit)
                                  :blocked blocked
                                  :icon :search
                                  :action (lambda () (focus-field dialog :filter))
                                  :hint (i18n:tr "Type to find a file or a folder"))))))

(defun empty-list-text (dialog)
  "Return the sentence shown in place of an empty list.

DIALOG: the file dialog."
  (let ((text (string-trim " " (ui:edit-state-text (file-dialog-filter-edit dialog)))))
    (cond ((plusp (length text))
           (i18n:trf "Nothing here matches ~s." text))
          ((file-dialog-show-all dialog)
           (i18n:tr "This folder is empty."))
          (t
           (i18n:tr "No drawings in this folder.")))))

(defun list-widgets (app
                     dialog
                     layout
                     blocked)
  "Build the column headings, one row per visible entry in view, the scroll indicator, and the
sentence shown when the list is empty. Records how many rows fit.

APP: the application.
DIALOG: the file dialog.
LAYOUT: the dialog's layout.
BLOCKED: true while a prompt covers the dialog."
  (let* ((theme (app-theme app))
         (main-x (browser-layout-main-x layout))
         (main-width (browser-layout-main-width layout))
         (heading-top (- (browser-layout-list-top layout) +browser-column-header-height+))
         (list-top (browser-layout-list-top layout))
         (capacity (max 1 (floor (browser-layout-list-height layout) +browser-row-height+)))
         (entries (progn
                    (setf (file-dialog-capacity dialog) capacity)
                    (clamp-scroll dialog)
                    (visible-entries dialog)))
         (modified-x (- (+ main-x main-width) +modified-column-width+))
         (size-x (- modified-x +size-column-width+))
         (widgets (list (make-label :dialog-heading-name
                                    (i18n:tr "Name")
                                    (+ main-x ui:+padding+)
                                    heading-top
                                    main-width
                                    +browser-column-header-height+
                                    :size ui:+small-font-pixels+
                                    :color (scene:theme-text-faint theme))
                        (make-label :dialog-heading-size
                                    (i18n:tr "Size")
                                    size-x
                                    heading-top
                                    (- +size-column-width+ ui:+gap+)
                                    +browser-column-header-height+
                                    :align :right
                                    :size ui:+small-font-pixels+
                                    :color (scene:theme-text-faint theme))
                        (make-label :dialog-heading-modified
                                    (i18n:tr "Modified")
                                    (+ modified-x ui:+padding+)
                                    heading-top
                                    (- +modified-column-width+ ui:+padding+)
                                    +browser-column-header-height+
                                    :size ui:+small-font-pixels+
                                    :color (scene:theme-text-faint theme)))))
    (setf widgets (reverse widgets))
    (loop for entry in (nthcdr (file-dialog-scroll dialog) entries)
          for row from 0 below capacity
          do (let ((row-y (+ list-top (* row +browser-row-height+)))
                   (entry entry))
               (push (ui:make-widget :row
                                     (list :entry (file-entry-name entry))
                                     main-x
                                     row-y
                                     main-width
                                     (- +browser-row-height+ +browser-row-gap+)
                                     :label (file-entry-name entry)
                                     :icon (if (eq (file-entry-kind entry) :directory) :folder :file)
                                     :active (equal (file-dialog-selected dialog) (file-entry-name entry))
                                     :align :left
                                     :blocked blocked
                                     :trailing (+ +size-column-width+ +modified-column-width+)
                                     :action (lambda () (click-entry app
                                                                     dialog
                                                                     entry))
                                     :hint (uiop:native-namestring (file-entry-path entry)))
                     widgets)
               (push (make-label (list :entry-size (file-entry-name entry))
                                 (format-entry-size entry)
                                 size-x
                                 row-y
                                 (- +size-column-width+ ui:+gap+)
                                 (- +browser-row-height+ +browser-row-gap+)
                                 :align :right
                                 :size ui:+small-font-pixels+
                                 :color (scene:theme-text-dim theme))
                     widgets)
               (push (make-label (list :entry-modified (file-entry-name entry))
                                 (format-modified-time (file-entry-modified entry))
                                 (+ modified-x ui:+padding+)
                                 row-y
                                 (- +modified-column-width+ ui:+padding+)
                                 (- +browser-row-height+ +browser-row-gap+)
                                 :size ui:+small-font-pixels+
                                 :color (scene:theme-text-dim theme))
                     widgets)))
    (when (null entries)
      (push (make-label :dialog-empty
                        (empty-list-text dialog)
                        main-x
                        list-top
                        main-width
                        (* 3 +browser-row-height+)
                        :align :center
                        :color (scene:theme-text-faint theme))
            widgets))
    (let ((thumb (scrollbar-widget :dialog-scrollbar
                                   (+ main-x main-width)
                                   list-top
                                   (+ list-top (* capacity +browser-row-height+))
                                   (length entries)
                                   capacity
                                   (file-dialog-scroll dialog)
                                   theme)))
      (when thumb
        (push thumb widgets)))
    (nreverse widgets)))

(defun footer-widgets (app
                       dialog
                       layout
                       blocked)
  "Build the line of feedback under the list, the file name field, the All files chip, the New
folder button, and the Cancel and Open or Save buttons.

APP: the application.
DIALOG: the file dialog.
LAYOUT: the dialog's layout.
BLOCKED: true while a prompt covers the dialog."
  (let* ((theme (app-theme app))
         (x (browser-layout-x layout))
         (width (browser-layout-width layout))
         (main-x (browser-layout-main-x layout))
         (main-width (browser-layout-main-width layout))
         (top (browser-layout-footer-top layout))
         (message (file-dialog-message dialog))
         (error-p (eq (file-dialog-message-severity dialog) :error))
         (name-y (+ top ui:+gap+))
         (buttons-y (+ name-y +browser-control-height+ ui:+padding+))
         (right (- (+ x width) +browser-inset+))
         (open-p (eq (file-dialog-mode dialog) :open))
         (inner-left (+ x +browser-inset+))
         (name-label (i18n:tr "File name"))
         (name-label-width (max +name-label-width+
                                (+ (ui:text-width-pixels name-label ui:+small-font-pixels+) ui:+gap+)))
         (all-files-label (i18n:tr "All files"))
         (all-files-width (max +all-files-chip-width+ (chip-width all-files-label nil)))
         (new-folder-label (i18n:tr "New folder"))
         (new-folder-width (max +new-folder-button-width+
                                (+ (ui:text-width-pixels new-folder-label ui:+small-font-pixels+)
                                   ui:+small-icon-size+
                                   (* 4 ui:+gap+)))))
    (append
     (list (make-separator :dialog-footer-line
                           (+ x +browser-inset+)
                           top
                           (- width (* 2 +browser-inset+))
                           +separator-width+))
     (when message
       (list (make-label :dialog-message
                         message
                         main-x
                         (- top +browser-message-height+)
                         main-width
                         +browser-message-height+
                         :size ui:+small-font-pixels+
                         :icon (if error-p :warning :info)
                         :color (if error-p (scene:theme-danger theme) (scene:theme-text-dim theme)))))
     (list (make-label :dialog-name-label
                       name-label
                       inner-left
                       name-y
                       name-label-width
                       +browser-control-height+
                       :size ui:+small-font-pixels+
                       :color (scene:theme-text-dim theme))
           (ui:make-widget :field
                           :dialog-filename
                           (+ inner-left name-label-width)
                           name-y
                           (- right inner-left name-label-width)
                           +browser-control-height+
                           :label (ui:edit-state-text (file-dialog-name-edit dialog))
                           :align :left
                           :edit (and (eq (file-dialog-focus dialog) :name) (file-dialog-name-edit dialog))
                           :blocked blocked
                           :action (lambda () (focus-field dialog :name)))
           (ui:make-widget :button
                           :dialog-all-files
                           inner-left
                           (+ buttons-y (/ (- +action-button-height+ ui:+chip-height+) 2))
                           all-files-width
                           ui:+chip-height+
                           :label all-files-label
                           :active (file-dialog-show-all dialog)
                           :variant :chip
                           :size ui:+small-font-pixels+
                           :blocked blocked
                           :action (lambda ()
                                     (setf (file-dialog-show-all dialog) (not (file-dialog-show-all dialog)))
                                     (clamp-scroll dialog))
                           :hint (i18n:tr "List every file, not only drawings")))
     (unless open-p
       (list (ui:make-widget :button
                             :dialog-new-folder
                             (+ inner-left all-files-width ui:+gap+)
                             buttons-y
                             new-folder-width
                             +action-button-height+
                             :label new-folder-label
                             :icon :new-folder
                             :variant :ghost
                             :size ui:+small-font-pixels+
                             :blocked blocked
                             :action (lambda () (ask-for-folder-name app dialog))
                             :hint (i18n:tr "Create a folder here")
                             :shortcut "Ctrl+Shift+N")))
     (list (ui:make-widget :button
                           :dialog-cancel
                           (- right (* 2 +accept-button-width+) ui:+gap+)
                           buttons-y
                           +accept-button-width+
                           +action-button-height+
                           :label (i18n:tr "Cancel")
                           :blocked blocked
                           :action (lambda () (close-file-dialog app)))
           (ui:make-widget :button
                           :dialog-accept
                           (- right +accept-button-width+)
                           buttons-y
                           +accept-button-width+
                           +action-button-height+
                           :label (dialog-accept-label dialog)
                           :variant :primary
                           :enabled (accept-enabled-p dialog)
                           :blocked blocked
                           :action (lambda () (accept-dialog app)))))))

(defun dialog-title (dialog)
  "Return the heading of the dialog: what it asks the user to do.

DIALOG: the file dialog."
  (let ((opening (eq (file-dialog-mode dialog) :open)))
    (ecase (file-dialog-purpose dialog)
      (:dxf (if opening (i18n:tr "Import a DXF file") (i18n:tr "Export as a DXF file")))
      (:script (if opening (i18n:tr "Run a script") (i18n:tr "Export the drawing as a script")))
      (:recording (i18n:tr "Save the recorded actions as a script"))
      (:shape (i18n:tr "Add a shape from an SVG file"))
      (:image (i18n:tr "Export as a JPEG image"))
      (:pdf (i18n:tr "Export as a PDF file"))
      (:svg (i18n:tr "Export as an SVG file"))
      (:drawing (if opening (i18n:tr "Open a drawing") (i18n:tr "Save drawing as"))))))

(defun dialog-accept-label (dialog)
  "Return the text of the button that does what the dialog is for.

DIALOG: the file dialog."
  (cond ((eq :dxf (file-dialog-purpose dialog))
         (if (eq (file-dialog-mode dialog) :open) (i18n:tr "Import") (i18n:tr "Export")))
        ((eq :script (file-dialog-purpose dialog))
         (if (eq (file-dialog-mode dialog) :open) (i18n:tr "Choose") (i18n:tr "Export")))
        ((eq :shape (file-dialog-purpose dialog))
         (i18n:tr "Add"))
        ((member (file-dialog-purpose dialog) '(:image :pdf :svg))
         (i18n:tr "Export"))
        ((eq (file-dialog-mode dialog) :open) (i18n:tr "Open"))
        (t (i18n:tr "Save"))))

(defun file-dialog-widgets (app
                            blocked)
  "Build every widget of the file dialog, in drawing order.

APP: the application.
BLOCKED: true while a prompt covers the dialog."
  (let* ((dialog (app-dialog app))
         (layout (browser-layout-for app)))
    (append (dialog-chrome-widgets app
                                   dialog
                                   layout
                                   blocked)
            (sidebar-widgets app
                             dialog
                             layout
                             blocked)
            (toolbar-widgets-for-dialog app
                                        dialog
                                        layout
                                        blocked)
            (list-widgets app
                          dialog
                          layout
                          blocked)
            (footer-widgets app
                            dialog
                            layout
                            blocked))))
