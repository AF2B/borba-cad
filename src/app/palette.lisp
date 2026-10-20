(in-package #:borba-cad/app)

;;;; The command palette, Ctrl+K: a field to type in and the list of everything the program can be told to do, narrowed
;;;; as the words are typed. It lists the commands that the user defined, those saved from recordings, and the choices
;;;; of the menus and the title bar, so that anything can be reached from the keyboard and a command can be found by
;;;; what it does. Enter runs the one that is chosen, Escape leaves.

(defconstant +palette-visible-rows+ 9
  "Most commands the palette lists at once; the arrows and the wheel bring the others.")

(defconstant +palette-row-height+ 30f0
  "Height of one command in the list of the command palette, in pixels.")

(defconstant +palette-padding+ 12f0
  "Space between the edge of the command palette and its content, in pixels.")

(defconstant +palette-field-height+ 38f0
  "Height of the field that the command palette is searched in, in pixels.")

(defconstant +palette-gap+ 8f0
  "Space between the field of the command palette and its list, in pixels.")

(defconstant +palette-window-margin+ 16f0
  "Space the command palette keeps clear of the top and the bottom of the window, in pixels.")

(defconstant +palette-shortcut-gap+ 16f0
  "Space, in pixels, that the label of a command keeps clear of its keys in the command palette.")

(defstruct (palette-entry (:constructor make-palette-entry (label
                                                            hint
                                                            shortcut
                                                            action)))
  "One thing the palette can run.

LABEL: what it is called, which is also what a key can be bound to.
HINT: what it does in a sentence, or NIL.
SHORTCUT: the keys that do the same, or NIL.
ACTION: function of no arguments that does it."
  label hint shortcut action)

(defun menu-palette-entries (app
                             kind
                             group)
  "Return an entry for each choice of a menu, labeled with the name of the menu.

APP: the application.
KIND: the kind of menu, :draw, :modify, :measure or :more.
GROUP: the name of the menu, translated."
  (loop for item in (menu-items app kind)
        when item
          collect (let ((item item))
                    (make-palette-entry (format nil "~a: ~a" group (menu-item-label item))
                                        nil
                                        (menu-item-shortcut item)
                                        (menu-item-action item)))))

(defparameter *layout-palette-choices*
  (list (list (i18n:mark "Layout") (i18n:mark "Add a sheet") nil 'operation-add-sheet)
        (list (i18n:mark "Layout") (i18n:mark "Change the name, paper and title block of the sheet") nil
              'operation-sheet-properties)
        (list (i18n:mark "Layout") (i18n:mark "Delete the sheet") nil 'operation-delete-sheet)
        (list (i18n:mark "Layout") (i18n:mark "Show the next sheet") nil 'operation-show-next-sheet)
        (list (i18n:mark "Layout") (i18n:mark "Show the sheet before") nil 'operation-show-previous-sheet)
        (list (i18n:mark "Layout") (i18n:mark "Put a new viewport on the sheet") nil 'operation-arm-viewport)
        (list (i18n:mark "Layout") (i18n:mark "Change what the viewport shows") "Enter" 'operation-edit-viewport)
        (list (i18n:mark "Layout") (i18n:mark "Delete the viewport") "Delete" 'operation-delete-viewport))
  "What the palette offers only while the sheets are shown, as (group label shortcut function), as the others are.")

(defparameter *general-palette-choices*
  (list (list (i18n:mark "Edit") (i18n:mark "Select all") "Ctrl+A" 'operation-select-all)
        (list (i18n:mark "Edit") (i18n:mark "Delete the selection") "Delete" 'operation-delete-selection)
        (list (i18n:mark "Edit") (i18n:mark "Copy") "Ctrl+C" 'operation-copy)
        (list (i18n:mark "Edit") (i18n:mark "Cut") "Ctrl+X" 'operation-cut)
        (list (i18n:mark "Edit") (i18n:mark "Paste") "Ctrl+V" 'operation-paste)
        (list (i18n:mark "View") (i18n:mark "Zoom to fit") "Home" 'operation-zoom-extents)
        (list (i18n:mark "View") (i18n:mark "Zoom in") nil 'operation-zoom-in)
        (list (i18n:mark "View") (i18n:mark "Zoom out") nil 'operation-zoom-out)
        (list (i18n:mark "View") (i18n:mark "Show or hide the grid") "G" 'operation-toggle-grid)
        (list (i18n:mark "View") (i18n:mark "Snap to the grid") "S" 'operation-toggle-snap)
        (list (i18n:mark "View") (i18n:mark "Snap to points of entities") "O" 'operation-toggle-object-snap)
        (list (i18n:mark "View") (i18n:mark "Hold lines straight (ortho)") "F8" 'operation-toggle-ortho)
        (list (i18n:mark "View") (i18n:mark "Hold lines to angles (polar)") "F10" 'operation-toggle-polar)
        (list (i18n:mark "View") (i18n:mark "Change the unit of lengths") "U" 'operation-cycle-display-unit)
        (list (i18n:mark "View") (i18n:mark "Switch the light and dark themes") nil 'operation-toggle-theme)
        (list (i18n:mark "View") (i18n:mark "Make the interface larger") "Ctrl++" 'operation-interface-zoom-in)
        (list (i18n:mark "View") (i18n:mark "Make the interface smaller") "Ctrl+-" 'operation-interface-zoom-out)
        (list (i18n:mark "View") (i18n:mark "Reset the size of the interface") "Ctrl+0"
              'operation-interface-zoom-reset)
        (list (i18n:mark "Underlay") (i18n:mark "Add a picture or a PDF under the drawing...") "Ctrl+Shift+U"
              'operation-add-underlay)
        (list (i18n:mark "Underlay") (i18n:mark "Calibrate an underlay") nil 'operation-calibrate-underlay)
        (list (i18n:mark "Underlay") (i18n:mark "Manage the underlays...") nil 'operation-underlays)
        (list (i18n:mark "Underlay") (i18n:mark "Show or hide the underlays") nil 'operation-toggle-underlays)
        (list (i18n:mark "Layout") (i18n:mark "Show or leave the sheets") "Ctrl+L" 'operation-toggle-layout)
        (list (i18n:mark "Layout") (i18n:mark "Export the sheets as PDF...") nil 'operation-export-sheets)
        (list (i18n:mark "Settings") (i18n:mark "Run the init file again") nil 'operation-reload-init))
  "What the program does that no menu offers, as (group label shortcut function): the group and the label in English,
translated where the palette is built, and the function that does it, of the application.")

(defun general-palette-entries (app)
  "Return the entries for what the program does that is not in a menu: the title bar, the editing, the view and the
settings.

APP: the application."
  (append
   (loop for spec in (title-button-specs app)
         when (and spec (sixth spec))
           collect (let ((spec spec))
                     (make-palette-entry (format nil "~a: ~a" (i18n:tr "File") (fourth spec))
                                         nil
                                         (fifth spec)
                                         (third spec))))
   (loop for choice in (append *general-palette-choices*
                               (and (layout-mode-p app) *layout-palette-choices*))
         collect (let ((function (fourth choice)))
                   (make-palette-entry (format nil "~a: ~a" (i18n:tr (first choice)) (i18n:tr (second choice)))
                                       nil
                                       (third choice)
                                       (lambda () (funcall function app)))))))

(defun palette-entries (app)
  "Return everything the palette lists, in the order it lists it when nothing is typed: the commands that the user
defined, those saved from recordings, then the choices of the menus and the rest of what the program does.

APP: the application."
  (append (loop for command in (script:customizations-commands (app-customizations app))
                collect (let ((command command))
                          (make-palette-entry (script:user-command-name command)
                                              (script:user-command-description command)
                                              nil
                                              (lambda () (run-user-command app command)))))
          (loop for generator in (block-generators app)
                collect (let ((generator generator))
                          (make-palette-entry (format nil "~a: ~a"
                                                      (i18n:tr "Block")
                                                      (script:block-generator-name generator))
                                              (script:block-generator-description generator)
                                              nil
                                              (lambda () (ask-block-parameters app generator)))))
          (loop for name in (app-recorded-commands app)
                collect (let ((name name))
                          (make-palette-entry (format nil "~a: ~a" (i18n:tr "Recorded") name)
                                              nil
                                              nil
                                              (lambda () (run-recorded-command app name)))))
          (menu-palette-entries app
                                :draw
                                (i18n:tr "Draw"))
          (menu-palette-entries app
                                :modify
                                (i18n:tr "Modify"))
          (menu-palette-entries app
                                :measure
                                (i18n:tr "Measure"))
          (menu-palette-entries app
                                :more
                                (i18n:tr "More"))
          (general-palette-entries app)))

(defun find-palette-entry (app
                           name)
  "Return the entry that is called NAME, in any case, or NIL.

APP: the application.
NAME: what it is called."
  (find name
        (palette-entries app)
        :key #'palette-entry-label
        :test #'string-equal))

;;; Narrowing the list

(defun palette-entry-text (entry)
  "Return what the words typed in the palette are looked for in: the label and the hint, folded to lower case and
without accents.

ENTRY: a palette entry."
  (sbcl:fold-for-search (format nil "~a ~@[~a~]" (palette-entry-label entry) (palette-entry-hint entry))))

(defun palette-rank (entry
                     tokens)
  "Return how well an entry answers the words typed: 0 when its label begins with the first, 1 when a word of it does,
2 otherwise.

ENTRY: a palette entry.
TOKENS: the words typed, folded."
  (let ((label (sbcl:fold-for-search (palette-entry-label entry)))
        (first-token (first tokens)))
    (cond ((and (>= (length label) (length first-token))
                (string= first-token label :end2 (length first-token)))
           0)
          ((search (concatenate 'string " " first-token) (concatenate 'string " " label))
           1)
          (t 2))))

(defun palette-matches (entries
                        query)
  "Return the entries that answer what was typed: those that have every word of it somewhere in their label or hint,
the ones that begin with it first and the rest in the order they were listed. All of them when nothing is typed.

ENTRIES: the palette entries.
QUERY: the text typed."
  (let ((tokens (remove "" (uiop:split-string (sbcl:fold-for-search query) :separator '(#\Space)) :test #'string=)))
    (if (null tokens)
        entries
        (mapcar #'cdr
                (stable-sort (loop for entry in entries
                                   when (let ((text (palette-entry-text entry)))
                                          (every (lambda (token) (search token text)) tokens))
                                     collect (cons (palette-rank entry tokens) entry))
                             #'<
                             :key #'car)))))

(defun palette-prompt-matches (prompt)
  "Return the entries of an open palette that answer what is typed in it.

PROMPT: the :palette prompt."
  (palette-matches (prompt-fields prompt)
                   (ui:edit-state-text (prompt-edit prompt))))

;;; Moving about and running

(defun palette-capacity (app)
  "Return how many commands the palette lists at once: the usual number, or fewer in a window that is too short for
them.

APP: the application."
  (let ((room (- (float (app-window-height app) 1f0)
                 (* 2 +palette-window-margin+)
                 (* 2 +palette-padding+)
                 +palette-field-height+
                 +palette-gap+)))
    (max 1 (min +palette-visible-rows+ (floor room +palette-row-height+)))))

(defun keep-palette-selection-in-view (app
                                       prompt)
  "Scroll the list of an open palette so that the chosen entry is in it.

APP: the application.
PROMPT: the :palette prompt."
  (let ((active (prompt-active prompt))
        (scroll (prompt-scroll prompt))
        (capacity (palette-capacity app)))
    (setf (prompt-scroll prompt)
          (cond ((< active scroll) active)
                ((>= active (+ scroll capacity)) (- (1+ active) capacity))
                (t scroll)))))

(defun move-palette-selection (app
                               step
                               &key wrap)
  "Choose another entry of the open palette.

APP: the application.
STEP: how many entries down, negative for up.
WRAP: true to go from the last to the first and the other way, as the arrows do; false to stop at the ends, as the page
keys do."
  (let* ((prompt (app-prompt app))
         (count (length (palette-prompt-matches prompt))))
    (when (plusp count)
      (setf (prompt-active prompt)
            (if wrap
                (mod (+ (prompt-active prompt) step) count)
                (max 0 (min (1- count) (+ (prompt-active prompt) step)))))
      (keep-palette-selection-in-view app prompt))
    (app-request-redraw app)))

(defun scroll-palette (app
                       step)
  "Scroll the list of the open palette by rows, as far as there are rows.

APP: the application.
STEP: how many rows, positive to go down."
  (let* ((prompt (app-prompt app))
         (count (length (palette-prompt-matches prompt))))
    (setf (prompt-scroll prompt) (max 0 (min (max 0 (- count (palette-capacity app)))
                                             (+ (prompt-scroll prompt) step))))
    (app-request-redraw app)))

(defun run-palette-entry (app
                          entry)
  "Close the palette and run an entry of it.

APP: the application.
ENTRY: the palette entry."
  (close-prompt app)
  (funcall (palette-entry-action entry))
  (app-request-redraw app))

(defun run-palette-selection (app)
  "Run the entry of the open palette that is chosen, if there is one.

APP: the application."
  (let* ((prompt (app-prompt app))
         (entry (nth (prompt-active prompt) (palette-prompt-matches prompt))))
    (if entry
        (run-palette-entry app entry)
        (app-request-redraw app))))

(defun operation-command-palette (app)
  "Open the command palette.

APP: the application."
  (let ((prompt (make-prompt :palette
                             (i18n:tr "Command palette")
                             ""
                             (ui:make-edit-state :text ""
                                                 :maximum-length +prompt-maximum-text-length+)
                             nil)))
    (setf (prompt-fields prompt) (palette-entries app)
          (app-prompt app) prompt
          (app-menu app) nil
          (app-focus app) :prompt)
    (app-request-redraw app)))
