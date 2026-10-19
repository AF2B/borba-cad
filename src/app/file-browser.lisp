(in-package #:borba-cad/app)

;;;; The state of the file dialog and everything it can do without a window: listing a folder,
;;;; sorting and filtering it, moving the selection, walking up and down the folder tree, the
;;;; places and recent files on the side, and the names the user types. The dialog's widgets and
;;;; key handling are in file-dialog.lisp.

(defconstant +name-field-maximum-length+ 255
  "Longest file name the dialog's name field accepts, the usual limit of file systems.")

(defconstant +filter-maximum-length+ 128
  "Longest text the dialog's search field accepts.")

(defconstant +location-maximum-length+ 1024
  "Longest path the dialog's location field accepts.")

(defconstant +default-list-capacity+ 10
  "Rows of the file list assumed before the dialog has been drawn once.")

(defconstant +double-click-nanoseconds+ 450000000
  "Longest time between two clicks on the same row that still count as a double click.")

(defparameter *hidden-name-prefix* #\.
  "Names that begin with this character are not listed.")

(defparameter *dxf-extensions* (list "dxf")
  "Extensions of the files that the dialog lists when it is about DXF files.")

(defparameter *examples-folder-name* "examples"
  "Name of the folder of sample drawings in the project, listed among the places when it exists.")

(defstruct (file-entry (:constructor make-file-entry (name
                                                      kind
                                                      path
                                                      size
                                                      modified
                                                      &aux (key (sbcl:fold-for-search name)))))
  "One line of a folder listing.

NAME: the file or folder name.
KIND: :directory or :file.
PATH: its absolute pathname; a folder's ends in a slash.
SIZE: the byte count of a file, NIL for a folder.
MODIFIED: universal time of the last change.
KEY: the name in lower case without accents, by which entries are sorted and searched."
  name kind path size modified key)

(defstruct (file-dialog (:constructor %make-file-dialog (mode
                                                         continuation
                                                         &optional (purpose :drawing))))
  "The dialog where the user chooses a drawing to open or a place and a name to save to.

MODE: :open or :save.
CONTINUATION: function of no arguments run after a successful save, or NIL.
PURPOSE: :drawing for a drawing of the program, :dxf to import or export a DXF file, or :script to run a
script or export one.
DIRECTORY: the folder shown, as a directory pathname.
ENTRIES: every entry of that folder, folders first, each group by name.
SELECTED: the name of the selected entry, or NIL.
SCROLL: how many visible entries are scrolled past.
CAPACITY: how many rows of the list fit, known once the dialog has been drawn.
SHOW-ALL: true to list every file, false to list only drawings.
FILTER-EDIT: edit state of the search field.
NAME-EDIT: edit state of the file name field.
LOCATION-EDIT: edit state while the path is being typed, or NIL when it is shown as a trail.
FOCUS: which field has the keyboard: :name, :filter or :location.
MESSAGE: a sentence under the list, or NIL.
MESSAGE-SEVERITY: :info or :error.
PLACES: the shortcuts on the side, as (label icon pathname).
RECENT: the recently used drawings, as pathnames, latest first.
LAST-CLICK: the row last clicked and when, to recognize a double click.
VISIBLE-CACHE: the entries the list showed last time with what they were computed from, so that
a folder of tens of thousands of files is not filtered again for every widget of every frame."
  mode
  continuation
  purpose
  (directory nil)
  (entries '())
  (selected nil)
  (scroll 0 :type fixnum)
  (capacity +default-list-capacity+ :type fixnum)
  (show-all nil)
  (filter-edit (ui:make-edit-state :maximum-length +filter-maximum-length+))
  (name-edit (ui:make-edit-state :maximum-length +name-field-maximum-length+))
  (location-edit nil)
  (focus :name)
  (message nil)
  (message-severity :info)
  (places '())
  (recent '())
  (last-click nil)
  (visible-cache nil))

;;; Formatting

(defun format-modified-time (universal-time)
  "Return a time of day as the dialog shows it, in the local time zone: 2026-10-14 03:22.

UNIVERSAL-TIME: a universal time, or NIL."
  (if universal-time
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time universal-time)
        (declare (ignore second))
        (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute))
      ""))

(defun format-entry-size (entry)
  "Return the size column of an entry: the size of a file, nothing for a folder.

ENTRY: a file entry."
  (if (eq (file-entry-kind entry) :file)
      (base:format-byte-size (file-entry-size entry))
      ""))

;;; Listing

(defun entry-pathname (directory
                       name
                       kind)
  "Return the absolute pathname of an entry of DIRECTORY.

DIRECTORY: the folder, as a directory pathname.
NAME: the entry's name.
KIND: :directory or :file."
  (let ((path (uiop:parse-native-namestring name
                                            :ensure-directory (eq kind :directory)
                                            :defaults directory)))
    (merge-pathnames path directory)))

(defun entry-name< (first
                    second)
  "Return true when entry FIRST sorts before SECOND: folders before files, then by name ignoring
case and accents (a name that starts with A-acute sorts among the As), and by the exact name when
those are the same.

FIRST: a file entry.
SECOND: a file entry."
  (let ((first-directory (eq (file-entry-kind first) :directory))
        (second-directory (eq (file-entry-kind second) :directory)))
    (cond ((and first-directory (not second-directory)) t)
          ((and second-directory (not first-directory)) nil)
          ((string/= (file-entry-key first) (file-entry-key second))
           (and (string< (file-entry-key first) (file-entry-key second)) t))
          (t (and (string< (file-entry-name first) (file-entry-name second)) t)))))

(defun list-entries (directory)
  "Return the entries of the folder DIRECTORY, folders first and each group by name, and a second
value that is NIL or a sentence that says why the folder cannot be read. Devices, sockets and
hidden entries are left out.

DIRECTORY: a directory pathname."
  (multiple-value-bind (raw problem)
      (sbcl:directory-listing directory)
    (values (sort (loop for (name kind size modified) in raw
                        when (and (member kind '(:directory :file))
                                  (char/= *hidden-name-prefix* (char name 0)))
                          collect (make-file-entry name
                                                   kind
                                                   (entry-pathname directory
                                                                   name
                                                                   kind)
                                                   size
                                                   modified))
                  #'entry-name<)
            problem)))

(defun drawing-name-p (name
                       &optional (extensions persist:*known-drawing-extensions*))
  "Return true when a file name ends in one of the extensions of the files the dialog is about:
those the program writes, unless told others.

NAME: a file name.
EXTENSIONS: the extensions, without the dot."
  (let ((type (pathname-type (uiop:parse-native-namestring name))))
    (and type
         (member type extensions :test #'string-equal)
         t)))

(defun dialog-extensions (dialog)
  "Return the extensions of the files that the dialog lists when it is not asked to list all: those of
a drawing of the program, of a DXF file, of a script, or of a picture.

DIALOG: the file dialog."
  (ecase (file-dialog-purpose dialog)
    (:drawing persist:*known-drawing-extensions*)
    (:dxf *dxf-extensions*)
    ((:script :recording) *script-extensions*)
    ((:svg :shape) *svg-extensions*)
    (:image *image-extensions*)
    (:pdf *pdf-extensions*)
    (:quantities *csv-extensions*)))

(defun compute-visible-entries (entries
                                show-all
                                text
                                &optional (extensions persist:*known-drawing-extensions*))
  "Return the entries of ENTRIES the list shows: folders always, files only when they are drawings
unless all files were asked for, and only those whose names contain TEXT, ignoring case and
accents (agua finds the name that begins with A-acute and gua).

ENTRIES: every entry of the folder.
SHOW-ALL: true to list every file.
TEXT: the search text, without the blanks around it.
EXTENSIONS: the extensions of the files listed when not all are."
  (let ((wanted (sbcl:fold-for-search text)))
    (remove-if-not (lambda (entry)
                     (and (or (eq (file-entry-kind entry) :directory)
                              show-all
                              (drawing-name-p (file-entry-name entry) extensions))
                          (or (zerop (length wanted))
                              (search wanted (file-entry-key entry)))))
                   entries)))

(defun visible-entries (dialog)
  "Return the entries the list shows (see COMPUTE-VISIBLE-ENTRIES). The answer is kept until the
folder, the search text or the All files switch changes.

DIALOG: the file dialog."
  (let ((entries (file-dialog-entries dialog))
        (show-all (file-dialog-show-all dialog))
        (text (string-trim " " (ui:edit-state-text (file-dialog-filter-edit dialog))))
        (cache (file-dialog-visible-cache dialog)))
    (destructuring-bind (&optional cached-entries cached-show-all cached-text cached-result)
        cache
      (if (and cache
               (eq entries cached-entries)
               (eq show-all cached-show-all)
               (string= text cached-text))
          cached-result
          (let ((result (compute-visible-entries entries
                                                 show-all
                                                 text
                                                 (dialog-extensions dialog))))
            (setf (file-dialog-visible-cache dialog) (list entries show-all text result))
            result)))))

(defun find-entry (dialog
                   name)
  "Return the entry of the folder shown that has NAME, or NIL.

DIALOG: the file dialog.
NAME: an entry name."
  (find name (file-dialog-entries dialog) :key #'file-entry-name :test #'string=))

(defun selected-entry (dialog)
  "Return the selected entry, or NIL when none is selected or it is not in the list.

DIALOG: the file dialog."
  (let ((name (file-dialog-selected dialog)))
    (and name
         (find name (visible-entries dialog) :key #'file-entry-name :test #'string=))))

;;; Messages

(defun dialog-say (dialog
                   text
                   &optional (severity :info))
  "Show a sentence under the list.

DIALOG: the file dialog.
TEXT: the sentence, or NIL to clear it.
SEVERITY: :info or :error."
  (setf (file-dialog-message dialog) text
        (file-dialog-message-severity dialog) severity))

;;; Scrolling and selection

(defun clamp-scroll (dialog)
  "Keep the list scrolled to a valid place after the entries or the capacity changed.

DIALOG: the file dialog."
  (let ((most (max 0 (- (length (visible-entries dialog)) (file-dialog-capacity dialog)))))
    (setf (file-dialog-scroll dialog) (max 0 (min (file-dialog-scroll dialog) most)))))

(defun reveal-selection (dialog)
  "Scroll the list just enough that the selected entry is in view.

DIALOG: the file dialog."
  (let ((index (position (file-dialog-selected dialog)
                         (visible-entries dialog)
                         :key #'file-entry-name
                         :test #'string=))
        (capacity (file-dialog-capacity dialog)))
    (when index
      (cond ((< index (file-dialog-scroll dialog))
             (setf (file-dialog-scroll dialog) index))
            ((>= index (+ (file-dialog-scroll dialog) capacity))
             (setf (file-dialog-scroll dialog) (1+ (- index capacity)))))
      (clamp-scroll dialog))))

(defun select-entry (dialog
                     entry)
  "Select an entry. Selecting a file puts its name in the file name field; selecting a folder
leaves the field alone.

DIALOG: the file dialog.
ENTRY: a file entry."
  (setf (file-dialog-selected dialog) (file-entry-name entry))
  (when (eq (file-entry-kind entry) :file)
    (setf (file-dialog-name-edit dialog)
          (ui:edit-select-all-replace
           (ui:make-edit-state :text (file-entry-name entry)
                               :maximum-length +name-field-maximum-length+))))
  (dialog-say dialog nil)
  (reveal-selection dialog))

(defun move-selection (dialog
                       delta)
  "Move the selection up or down the list, stopping at its ends. With nothing selected, going
down selects the first entry and going up the last.

DIALOG: the file dialog.
DELTA: how many rows to move; negative goes up."
  (let* ((entries (visible-entries dialog))
         (count (length entries))
         (current (position (file-dialog-selected dialog) entries :key #'file-entry-name :test #'string=)))
    (when (plusp count)
      (let ((target (cond (current (max 0 (min (1- count) (+ current delta))))
                          ((plusp delta) 0)
                          (t (1- count)))))
        (select-entry dialog (nth target entries))))))

(defun scroll-list (dialog
                    rows)
  "Scroll the list without changing the selection.

DIALOG: the file dialog.
ROWS: how many rows to scroll; positive goes down."
  (incf (file-dialog-scroll dialog) rows)
  (clamp-scroll dialog))

(defun filter-changed (dialog)
  "Reset what depends on the search text after it changed: the list starts from the top and the
first match is selected when the old selection no longer shows.

DIALOG: the file dialog."
  (setf (file-dialog-scroll dialog) 0)
  (let ((entries (visible-entries dialog)))
    (unless (selected-entry dialog)
      (setf (file-dialog-selected dialog)
            (and entries
                 (plusp (length (ui:edit-state-text (file-dialog-filter-edit dialog))))
                 (file-entry-name (first entries)))))))

;;; Navigation

(defun default-focus (mode)
  "Return the field that has the keyboard when nothing else was chosen: the search field when
opening, since the user is looking for a file, and the name field when saving.

MODE: :open or :save."
  (if (eq mode :open) :filter :name))

(defun open-directory (dialog
                       directory)
  "Show the folder DIRECTORY. When it cannot be read, say why and stay where the dialog was.
Returns true when the folder was shown.

DIALOG: the file dialog.
DIRECTORY: a directory pathname."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (multiple-value-bind (entries problem)
        (list-entries directory)
      (cond (problem
             (dialog-say dialog
                         problem
                         :error)
             nil)
            (t
             (setf (file-dialog-directory dialog) directory
                   (file-dialog-entries dialog) entries
                   (file-dialog-selected dialog) nil
                   (file-dialog-scroll dialog) 0
                   (file-dialog-filter-edit dialog) (ui:make-edit-state :maximum-length +filter-maximum-length+)
                   (file-dialog-location-edit dialog) nil)
             (when (eq (file-dialog-focus dialog) :location)
               (setf (file-dialog-focus dialog) (default-focus (file-dialog-mode dialog))))
             (dialog-say dialog nil)
             t)))))

(defun parent-directory (directory)
  "Return the folder that holds DIRECTORY, or NIL when DIRECTORY is the root.

DIRECTORY: a directory pathname."
  (let ((components (pathname-directory directory)))
    (and (rest components)
         (make-pathname :directory (butlast components)
                        :name nil
                        :type nil
                        :version nil
                        :defaults directory))))

(defun open-parent-directory (dialog)
  "Show the folder that holds the one shown, and select the folder just left so that going back
down is one key press. Returns true when there was a parent.

DIALOG: the file dialog."
  (let* ((directory (file-dialog-directory dialog))
         (parent (parent-directory directory))
         (left (car (last (pathname-directory directory)))))
    (when (and parent (open-directory dialog parent))
      (let ((entry (and (stringp left) (find-entry dialog left))))
        (when entry
          (select-entry dialog entry)))
      t)))

(defun breadcrumbs (directory)
  "Return the trail from the root to DIRECTORY as (label . directory) pairs, the root first.

DIRECTORY: a directory pathname."
  (let ((components (pathname-directory directory)))
    (loop for count from 1 to (length components)
          collect (let ((path (make-pathname :directory (subseq components 0 count)
                                             :name nil
                                             :type nil
                                             :version nil
                                             :defaults directory)))
                    (cons (if (= count 1)
                              (root-label directory)
                              (let ((last (car (last (pathname-directory path)))))
                                (if (stringp last) last (princ-to-string last))))
                          path)))))

(defun root-label (directory)
  "Return the label of the first step of a trail: a slash on Linux, the drive on Windows.

DIRECTORY: a directory pathname."
  (let ((device (pathname-device directory)))
    (if (stringp device)
        (format nil "~a:" device)
        "/")))

;;; Places

(defun existing-directory (path)
  "Return PATH when it names a folder that exists, otherwise NIL.

PATH: a pathname, or NIL."
  (and path
       (eq :directory (sbcl:file-kind path))
       path))

(defun folder-label (directory)
  "Return the last name of a folder, which is how the user's language calls it (Documents,
Documentos).

DIRECTORY: a directory pathname."
  (let ((last (car (last (pathname-directory directory)))))
    (if (stringp last) last (root-label directory))))

(defun default-places (config)
  "Return the shortcuts that the dialog offers on its side, as (label icon pathname): the home
folder, the documents, desktop and downloads folders in the language of the desktop, and the
sample drawings of the project, each only when it exists.

CONFIG: the application configuration."
  (let* ((home (user-homedir-pathname))
         (candidates
           (list (list (i18n:tr "Home") :home home)
                 (list nil :folder (persist:default-drawings-directory))
                 (list nil :folder (or (existing-directory (persist:user-directories-entry "XDG_DESKTOP_DIR"))
                                       (existing-directory (merge-pathnames "Desktop/" home))))
                 (list nil :folder (or (existing-directory (persist:user-directories-entry "XDG_DOWNLOAD_DIR"))
                                       (existing-directory (merge-pathnames "Downloads/" home))))
                 (list (i18n:tr "Examples") :folder
                       (and (base:config-project-root config)
                            (existing-directory (merge-pathnames (concatenate 'string
                                                                              *examples-folder-name*
                                                                              "/")
                                                                 (base:config-project-root config)))))))
         (places '()))
    (loop for (label icon path) in candidates
          do (when (and path
                        (not (find path places :key #'third :test #'equal)))
               (push (list (or label (folder-label path))
                           icon
                           path)
                     places)))
    (nreverse places)))

;;; Creating the dialog

(defun initial-name (app
                     mode
                     &optional (purpose :drawing))
  "Return what the file name field starts with: the name of the file being edited, or a new
name, when saving; nothing when opening.

APP: the application.
MODE: :open or :save.
PURPOSE: :drawing, :dxf, :script, :image, :pdf or :svg; a DXF file, a script, a picture or a sheet is offered the
name of the drawing with its own extension."
  (cond ((eq mode :open) "")
        ((member purpose '(:dxf :script :recording :image :pdf :svg :quantities))
         (concatenate 'string
                      (if (app-file-path app)
                          (pathname-name (app-file-path app))
                          (pathname-name *untitled-file-name*))
                      "."
                      (first (ecase purpose
                               (:dxf *dxf-extensions*)
                               ((:script :recording) *script-extensions*)
                               (:image *image-extensions*)
                               (:pdf *pdf-extensions*)
                               (:svg *svg-extensions*)
                               (:quantities *csv-extensions*)))))
        ((app-file-path app) (file-namestring (app-file-path app)))
        (t *untitled-file-name*)))

(defun make-file-dialog (app
                         mode
                         continuation
                         &optional (purpose :drawing))
  "Create the file dialog, showing the folder of the file being edited, or the documents folder.

APP: the application.
MODE: :open or :save.
CONTINUATION: function of no arguments run after a successful save, or NIL.
PURPOSE: :drawing, :dxf, :script, :image, :pdf or :svg."
  (let* ((dialog (%make-file-dialog mode
                                    continuation
                                    purpose))
         (state (base:config-state-directory (app-config app))))
    (setf (file-dialog-places dialog) (default-places (app-config app))
          (file-dialog-recent dialog) (remove-if-not (lambda (path)
                                                       (eq :file (sbcl:file-kind path)))
                                                     (persist:load-recent-files state))
          (file-dialog-focus dialog) (default-focus mode)
          (file-dialog-name-edit dialog)
          (ui:edit-select-all-replace
           (ui:make-edit-state :text (initial-name app
                                                   mode
                                                   purpose)
                               :maximum-length +name-field-maximum-length+)))
    (unless (open-directory dialog (drawings-directory app))
      (open-directory dialog (user-homedir-pathname)))
    dialog))

;;; Typed names

(defun typed-path (dialog
                   text)
  "Return the absolute pathname that TEXT, typed in the dialog, names: relative to the folder
shown, unless it is absolute or starts with ~.

DIALOG: the file dialog.
TEXT: what the user typed or pasted."
  (let ((cleaned (persist:clean-path-text text)))
    (if (or (find (char cleaned 0) "/\\~")
            (and (uiop:os-windows-p)
                 (> (length cleaned) 1)
                 (char= #\: (char cleaned 1))))
        (persist:native-path cleaned)
        (merge-pathnames (uiop:parse-native-namestring cleaned)
                         (file-dialog-directory dialog)))))

(defun create-folder (dialog
                      name)
  "Create a folder in the folder shown and select it. Says why when it cannot be done.
Returns true when the folder was created.

DIALOG: the file dialog.
NAME: the new folder's name."
  (let ((trimmed (string-trim " " name)))
    (cond ((zerop (length trimmed))
           (dialog-say dialog
                       (i18n:tr "Type a name for the folder.")
                       :error)
           nil)
          ((find-if (lambda (character) (find character "/\\")) trimmed)
           (dialog-say dialog
                       (i18n:tr "A folder name cannot contain a slash.")
                       :error)
           nil)
          (t
           (let* ((path (entry-pathname (file-dialog-directory dialog)
                                        trimmed
                                        :directory))
                  (problem (if (sbcl:file-kind path)
                               (i18n:trf "~a already exists." trimmed)
                               (sbcl:make-directory path))))
             (cond (problem
                    (dialog-say dialog
                                problem
                                :error)
                    nil)
                   (t
                    (open-directory dialog (file-dialog-directory dialog))
                    (let ((entry (find-entry dialog trimmed)))
                      (when entry
                        (select-entry dialog entry)))
                    (dialog-say dialog (i18n:trf "Created the folder ~a." trimmed))
                    t)))))))
