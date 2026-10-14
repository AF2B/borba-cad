(in-package #:borba-cad/app)

;;;; The file dialog's widgets and how it reacts to the keyboard, to typed text and to the wheel.
;;;; Its state and everything it can do without a window are in file-browser.lisp.

(defconstant +browser-maximum-width+ 940f0
  "Widest the file dialog grows, in pixels.")

(defconstant +browser-maximum-height+ 600f0
  "Tallest the file dialog grows, in pixels.")

(defconstant +browser-window-margin-x+ 80f0
  "Least space between the file dialog and the left and right edges of the window, in pixels.")

(defconstant +browser-window-margin-y+ 60f0
  "Least space between the file dialog and the top and bottom edges of the window, in pixels.")

(defconstant +browser-header-height+ 52f0
  "Height of the title row of the file dialog, in pixels.")

(defconstant +browser-footer-height+ 100f0
  "Height of the file name and buttons area of the file dialog, in pixels.")

(defconstant +browser-sidebar-width+ 200f0
  "Width of the column of places and recent files, in pixels.")

(defconstant +browser-gutter+ 16f0
  "Space between the side column and the list, in pixels.")

(defconstant +browser-control-height+ 34f0
  "Height of the fields and the buttons of the dialog's toolbar and name row, in pixels.")

(defconstant +browser-search-width+ 200f0
  "Width of the search field, in pixels.")

(defconstant +browser-column-header-height+ 26f0
  "Height of the strip that names the columns of the list, in pixels.")

(defconstant +browser-row-height+ 30f0
  "Height of a row of the file list, in pixels.")

(defconstant +browser-message-height+ 24f0
  "Height of the line under the list, in pixels.")

(defconstant +size-column-width+ 84f0
  "Width of the size column of the list, in pixels.")

(defconstant +modified-column-width+ 140f0
  "Width of the modified-time column of the list, in pixels.")

(defconstant +sidebar-row-height+ 30f0
  "Height of a place or a recent file in the side column, in pixels.")

(defconstant +sidebar-heading-height+ 30f0
  "Height of a heading of the side column, in pixels.")

(defconstant +recent-rows-shown+ 5
  "Most recent files listed in the side column.")

(defconstant +name-label-width+ 84f0
  "Width of the label before the file name field, in pixels.")

(defconstant +accept-button-width+ 108f0
  "Width of the Cancel and Open or Save buttons, in pixels.")

(defconstant +all-files-chip-width+ 100f0
  "Width of the chip that lists every file, in pixels.")

(defconstant +new-folder-button-width+ 124f0
  "Width of the New folder button, in pixels.")

(defconstant +browser-inset+ 20f0
  "Space between the edge of the file dialog and its content, in pixels.")

(defconstant +browser-row-gap+ 2f0
  "Space between two rows of the file list, in pixels.")

(defconstant +breadcrumb-separator-width+ 16f0
  "Width taken by the arrow between two steps of the folder trail, in pixels.")

(defconstant +breadcrumb-button-height+ 28f0
  "Height of a step of the folder trail, in pixels.")

(defstruct (browser-layout (:constructor make-browser-layout (x
                                                              y
                                                              width
                                                              height
                                                              side-x
                                                              main-x
                                                              main-width
                                                              body-top
                                                              list-top
                                                              list-height
                                                              footer-top)))
  "Where the parts of the file dialog sit, in window pixels.

X: left edge of the dialog.
Y: top edge of the dialog.
WIDTH: width of the dialog.
HEIGHT: height of the dialog.
SIDE-X: left edge of the side column.
MAIN-X: left edge of the toolbar and the list.
MAIN-WIDTH: width of the toolbar and the list.
BODY-TOP: top edge of the area under the title row.
LIST-TOP: top edge of the first row of the list.
LIST-HEIGHT: height of the rows' area.
FOOTER-TOP: top edge of the file name and buttons area."
  x y width height side-x main-x main-width body-top list-top list-height footer-top)

(defun browser-layout-for (app)
  "Compute where the parts of the file dialog sit in the current window.

APP: the application."
  (let* ((window-width (float (app-window-width app) 1f0))
         (window-height (float (app-window-height app) 1f0))
         (width (min +browser-maximum-width+ (- window-width (* 2 +browser-window-margin-x+))))
         (height (min +browser-maximum-height+ (- window-height (* 2 +browser-window-margin-y+))))
         (x (/ (- window-width width) 2))
         (y (/ (- window-height height) 2))
         (side-x (+ x +browser-inset+))
         (main-x (+ side-x +browser-sidebar-width+ +browser-gutter+))
         (body-top (+ y +browser-header-height+ ui:+gap+))
         (footer-top (- (+ y height) +browser-footer-height+))
         (list-top (+ body-top +browser-control-height+ ui:+gap+ +browser-column-header-height+))
         (list-height (max 0f0 (- footer-top list-top +browser-message-height+))))
    (make-browser-layout x
                         y
                         width
                         height
                         side-x
                         main-x
                         (- (+ x width) +browser-inset+ main-x)
                         body-top
                         list-top
                         list-height
                         footer-top)))

;;; Opening and closing

(defun open-file-dialog (app
                         mode
                         &optional continuation)
  "Open the file dialog to choose a drawing to open or a place and a name to save to.

APP: the application.
MODE: :open or :save.
CONTINUATION: function of no arguments run after a successful save, or NIL."
  (setf (app-dialog app) (make-file-dialog app
                                           mode
                                           continuation)
        (app-focus app) :dialog)
  (app-request-redraw app))

(defun close-file-dialog (app)
  "Close the file dialog without doing anything.

APP: the application."
  (setf (app-dialog app) nil
        (app-focus app) nil)
  (app-request-redraw app))

;;; Focus and text

(defun focused-edit (dialog)
  "Return the edit state of the field that has the keyboard, or NIL when none does.

DIALOG: the file dialog."
  (ecase (file-dialog-focus dialog)
    (:name (file-dialog-name-edit dialog))
    (:filter (file-dialog-filter-edit dialog))
    (:location (file-dialog-location-edit dialog))))

(defun store-focused-edit (dialog
                           edit)
  "Replace the edit state of the field that has the keyboard.

DIALOG: the file dialog.
EDIT: the new edit state."
  (ecase (file-dialog-focus dialog)
    (:name (setf (file-dialog-name-edit dialog) edit))
    (:filter (setf (file-dialog-filter-edit dialog) edit))
    (:location (setf (file-dialog-location-edit dialog) edit))))

(defun focus-field (dialog
                    field)
  "Give the keyboard to a field and select its text so that typing replaces it.

DIALOG: the file dialog.
FIELD: :name or :filter."
  (setf (file-dialog-focus dialog) field
        (file-dialog-location-edit dialog) nil)
  (store-focused-edit dialog (ui:edit-select-all-replace (focused-edit dialog))))

(defun start-location-edit (dialog)
  "Replace the folder trail by a field holding the path, ready to be typed over.

DIALOG: the file dialog."
  (setf (file-dialog-location-edit dialog)
        (ui:edit-select-all-replace
         (ui:make-edit-state :text (uiop:native-namestring (file-dialog-directory dialog))
                             :maximum-length +location-maximum-length+)))
  (setf (file-dialog-focus dialog) :location))

(defun leave-location-edit (dialog)
  "Go back from the path field to the folder trail, with the keyboard on the field the dialog
starts with.

DIALOG: the file dialog."
  (setf (file-dialog-location-edit dialog) nil
        (file-dialog-focus dialog) (default-focus (file-dialog-mode dialog))))

(defun typed-text (edit)
  "Return the text of an edit state, without the blanks around it.

EDIT: an edit state."
  (string-trim " " (ui:edit-state-text edit)))

;;; Accepting

(defun show-failure-in-dialog (app
                               dialog)
  "Show the reason of the file operation that just failed under the list, where the user is
looking.

APP: the application.
DIALOG: the file dialog."
  (let ((message (app-message app)))
    (dialog-say dialog
                (if message (car message) "That did not work.")
                :error)))

(defun accept-open (app
                    dialog
                    text)
  "Open the drawing that TEXT names, relative to the folder shown. A folder is entered instead.
When the file cannot be opened, the dialog stays and says why.

APP: the application.
DIALOG: the file dialog.
TEXT: a name or a path."
  (handler-case
      (let* ((path (resolve-open-path (uiop:native-namestring (typed-path dialog text))))
             (kind (sbcl:file-kind path)))
        (cond ((eq kind :directory)
               (open-directory dialog path)
               (leave-location-edit dialog))
              ((null kind)
               (dialog-say dialog
                           (format nil "~a does not exist." (string-trim " " text))
                           :error))
              ((load-drawing-into-app app (uiop:native-namestring path))
               (close-file-dialog app))
              (t
               (show-failure-in-dialog app dialog))))
    (base:invalid-input (condition)
      (dialog-say dialog
                  (base:invalid-input-reason condition)
                  :error))))

(defun finish-save (app
                    dialog
                    path)
  "Save the drawing to PATH and close the dialog; when the save fails the dialog stays and says
why.

APP: the application.
DIALOG: the file dialog.
PATH: the pathname to save to."
  (cond ((save-app-drawing app (uiop:native-namestring path))
         (let ((continuation (file-dialog-continuation dialog)))
           (close-file-dialog app)
           (when continuation
             (funcall continuation))))
        (t
         (show-failure-in-dialog app dialog))))

(defun accept-save (app
                    dialog
                    text)
  "Save the drawing under the name TEXT in the folder shown. A name that is an existing folder
enters it; a name that is a different existing file asks first whether to replace it.

APP: the application.
DIALOG: the file dialog.
TEXT: a name or a path."
  (cond ((zerop (length text))
         (dialog-say dialog
                     "Type a name for the file."
                     :error))
        (t
         (handler-case
             (let* ((typed (typed-path dialog text))
                    (path (persist:drawing-file-path (uiop:native-namestring typed)))
                    (kind (sbcl:file-kind path)))
               (cond ((eq (sbcl:file-kind typed) :directory)
                      (open-directory dialog typed))
                     ((and (eq kind :file)
                           (not (same-file-p path (app-file-path app))))
                      (open-choice-prompt app
                                          "Replace file?"
                                          (format nil "~a already exists. Replace it?"
                                                  (file-namestring path))
                                          (list (cons "Replace" (lambda () (finish-save app
                                                                                        dialog
                                                                                        path)))
                                                (cons "Cancel" nil))))
                     (t
                      (finish-save app
                                   dialog
                                   path))))
           (base:invalid-input (condition)
             (dialog-say dialog
                         (base:invalid-input-reason condition)
                         :error))))))

(defun activate-entry (app
                       dialog
                       entry)
  "Do what choosing an entry means: enter a folder; open a drawing, or when saving take its name
and ask to replace it.

APP: the application.
DIALOG: the file dialog.
ENTRY: a file entry."
  (cond ((eq (file-entry-kind entry) :directory)
         (open-directory dialog (file-entry-path entry)))
        ((eq (file-dialog-mode dialog) :open)
         (accept-open app
                      dialog
                      (uiop:native-namestring (file-entry-path entry))))
        (t
         (select-entry dialog entry)
         (accept-save app
                      dialog
                      (file-entry-name entry)))))

(defun path-like-p (text)
  "Return true when TEXT is a path rather than a name to search for: it has a slash, or it
starts with ~ or names a file:// address.

TEXT: what the user typed."
  (let ((trimmed (string-trim " " text)))
    (and (plusp (length trimmed))
         (or (find-if (lambda (character) (find character "/\\")) trimmed)
             (char= #\~ (char trimmed 0)))
         t)))

(defun go-to-path (app
                   dialog
                   text)
  "Go where a typed path leads: into a folder, or onto a file, which is opened when the dialog
opens drawings and otherwise gives the name field its name.

APP: the application.
DIALOG: the file dialog.
TEXT: the path typed in the location or search field."
  (handler-case
      (let* ((path (typed-path dialog text))
             (kind (sbcl:file-kind path)))
        (case kind
          (:directory
           (open-directory dialog path))
          (:file
           (if (eq (file-dialog-mode dialog) :open)
               (accept-open app
                            dialog
                            (uiop:native-namestring path))
               (when (open-directory dialog (persist:directory-of path))
                 (setf (file-dialog-name-edit dialog)
                       (ui:make-edit-state :text (file-namestring path)
                                           :maximum-length +name-field-maximum-length+)))))
          (t
           (let ((resolved (resolve-open-path (uiop:native-namestring path))))
             (if (and (eq (file-dialog-mode dialog) :open)
                      (eq :file (sbcl:file-kind resolved)))
                 (accept-open app
                              dialog
                              (uiop:native-namestring resolved))
                 (dialog-say dialog
                             (format nil "~a does not exist." (string-trim " " text))
                             :error))))))
    (base:invalid-input (condition)
      (dialog-say dialog
                  (base:invalid-input-reason condition)
                  :error))))

(defun accept-enabled-p (dialog)
  "Return true when there is something for the Open or Save button to act on: when opening, a
selected entry, a typed name or a typed path; when saving, a typed name.

DIALOG: the file dialog."
  (let ((name (typed-text (file-dialog-name-edit dialog))))
    (if (eq (file-dialog-mode dialog) :save)
        (plusp (length name))
        (and (or (plusp (length name))
                 (selected-entry dialog)
                 (path-like-p (ui:edit-state-text (file-dialog-filter-edit dialog)))
                 (file-dialog-location-edit dialog))
             t))))

(defun accept-dialog (app)
  "Do what Enter does in the dialog: go where the location field says; otherwise act on the
selected entry when the search field has the keyboard, or on the typed name.

APP: the application."
  (let* ((dialog (app-dialog app))
         (selected (selected-entry dialog)))
    (case (file-dialog-focus dialog)
      (:location
       (go-to-path app
                   dialog
                   (ui:edit-state-text (file-dialog-location-edit dialog))))
      (:filter
       (cond (selected
              (activate-entry app
                              dialog
                              selected))
             ((path-like-p (ui:edit-state-text (file-dialog-filter-edit dialog)))
              (go-to-path app
                          dialog
                          (ui:edit-state-text (file-dialog-filter-edit dialog))))
             ((eq (file-dialog-mode dialog) :save)
              (accept-save app
                           dialog
                           (typed-text (file-dialog-name-edit dialog))))
             (t
              (dialog-say dialog
                          "Choose a drawing from the list."
                          :error))))
      (t
       (let ((text (typed-text (file-dialog-name-edit dialog))))
         (if (eq (file-dialog-mode dialog) :open)
             (cond ((plusp (length text))
                    (accept-open app
                                 dialog
                                 text))
                   (selected
                    (activate-entry app
                                    dialog
                                    selected))
                   (t
                    (dialog-say dialog
                                "Choose a drawing from the list."
                                :error)))
             (accept-save app
                          dialog
                          text)))))
    (app-request-redraw app)))

(defun click-entry (app
                    dialog
                    entry)
  "Handle a click on a row of the list: it selects the entry, and a second click soon after
chooses it.

APP: the application.
DIALOG: the file dialog.
ENTRY: the entry that was clicked."
  (let* ((now (sbcl:monotonic-nanoseconds))
         (previous (file-dialog-last-click dialog))
         (again (and previous
                     (string= (car previous) (file-entry-name entry))
                     (< (- now (cdr previous)) +double-click-nanoseconds+))))
    (cond (again
           (setf (file-dialog-last-click dialog) nil)
           (activate-entry app
                           dialog
                           entry))
          (t
           (select-entry dialog entry)
           (setf (file-dialog-last-click dialog) (cons (file-entry-name entry) now))))))

(defun choose-recent-file (app
                           dialog
                           path)
  "Handle a click on a recent file: open it, or when saving show its folder with its name ready.

APP: the application.
DIALOG: the file dialog.
PATH: the file's pathname."
  (if (eq (file-dialog-mode dialog) :open)
      (accept-open app
                   dialog
                   (uiop:native-namestring path))
      (when (open-directory dialog (persist:directory-of path))
        (setf (file-dialog-name-edit dialog)
              (ui:edit-select-all-replace
               (ui:make-edit-state :text (file-namestring path)
                                   :maximum-length +name-field-maximum-length+))
              (file-dialog-focus dialog) :name))))

(defun ask-for-folder-name (app
                            dialog)
  "Ask for the name of a new folder in the folder shown, and create it.

APP: the application.
DIALOG: the file dialog."
  (open-text-prompt app
                    "New folder"
                    "Name of the new folder:"
                    ""
                    (lambda (text)
                      (create-folder dialog text))))

;;; Keyboard, text and wheel

(defun cycle-dialog-focus (dialog)
  "Move the keyboard to the other field of the dialog: the file name, then the search.

DIALOG: the file dialog."
  (focus-field dialog (if (eq (file-dialog-focus dialog) :name) :filter :name)))

(defun edit-focused-field (dialog
                           key)
  "Let the field that has the keyboard handle a key. Returns true when it did.

DIALOG: the file dialog.
KEY: key keyword."
  (let ((edited (ui:edit-handle-key (focused-edit dialog) key)))
    (when edited
      (store-focused-edit dialog edited)
      (when (eq (file-dialog-focus dialog) :filter)
        (filter-changed dialog))
      (dialog-say dialog nil)
      t)))

(defun handle-dialog-key (app
                          key
                          modifiers)
  "Handle a key press while the file dialog is open.

APP: the application.
KEY: key keyword.
MODIFIERS: modifier keys held."
  (let ((dialog (app-dialog app))
        (control (modifier-p modifiers :control))
        (shift (modifier-p modifiers :shift))
        (alt (modifier-p modifiers :alt)))
    (cond ((eq key :escape)
           (if (file-dialog-location-edit dialog)
               (leave-location-edit dialog)
               (close-file-dialog app)))
          ((eq key :enter)
           (accept-dialog app))
          ((eq key :tab)
           (cycle-dialog-focus dialog))
          ((and alt (eq key :up))
           (open-parent-directory dialog))
          ((and control (eq key :l))
           (start-location-edit dialog))
          ((and control shift (eq key :n) (eq (file-dialog-mode dialog) :save))
           (ask-for-folder-name app dialog))
          ((eq key :up)
           (move-selection dialog -1))
          ((eq key :down)
           (move-selection dialog 1))
          ((eq key :page-up)
           (move-selection dialog (- (file-dialog-capacity dialog))))
          ((eq key :page-down)
           (move-selection dialog (file-dialog-capacity dialog)))
          ((and (eq key :backspace)
                (eq (file-dialog-focus dialog) :filter)
                (zerop (length (ui:edit-state-text (file-dialog-filter-edit dialog)))))
           (open-parent-directory dialog))
          (t
           (edit-focused-field dialog key)))
    (app-request-redraw app)))

(defun handle-dialog-text (app
                           text)
  "Type text into the field of the file dialog that has the keyboard.

APP: the application.
TEXT: the characters typed."
  (let ((dialog (app-dialog app)))
    (store-focused-edit dialog (ui:edit-insert (focused-edit dialog) text))
    (when (eq (file-dialog-focus dialog) :filter)
      (filter-changed dialog))
    (dialog-say dialog nil)
    (app-request-redraw app)))

(defun handle-dialog-wheel (app
                            delta)
  "Scroll the file list.

APP: the application.
DELTA: wheel notches; positive scrolls up."
  (scroll-list (app-dialog app) (- (round delta)))
  (app-request-redraw app))
