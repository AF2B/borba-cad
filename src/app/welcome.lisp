(in-package #:borba-cad/app)

;;;; The start screen: a card in the middle of the window, shown when the program starts without a drawing,
;;;; that offers the drawings that were worked on last, a new drawing, a drawing to open, a script to run and
;;;; the way out. It is a list of choices like a menu is, and it is kept the same way: the arrows move, Enter
;;;; chooses, and the pointer moves the highlight. Escape closes it and goes on with the empty drawing.

(defconstant +welcome-width+ 520f0
  "Width of the start screen card, in pixels.")

(defconstant +welcome-padding+ 20f0
  "Space between the edge of the card and its contents, in pixels.")

(defconstant +welcome-header-height+ 64f0
  "Height of the brand and the question at the top of the card, in pixels.")

(defconstant +welcome-section-height+ 28f0
  "Height of the heading above the recent drawings, in pixels.")

(defconstant +welcome-row-height+ 34f0
  "Height of one choice, in pixels.")

(defconstant +welcome-group-gap+ 12f0
  "Space between the recent drawings and the other choices, in pixels.")

(defconstant +welcome-action-count+ 4
  "How many choices follow the recent drawings: new, open, run a script and quit.")

(defconstant +welcome-maximum-recents+ 5
  "Most recent drawings the start screen lists.")

(defconstant +welcome-clearance+ 360f0
  "Height of the window that the card needs besides its list of recent drawings, in pixels.")

(defconstant +welcome-logo-size+ 40f0
  "Side of the brand mark on the card, in pixels.")

(defun last-folder-name (path)
  "Return the name of the folder a file is in, to tell files of the same name apart.

PATH: the pathname of a file."
  (let ((directory (pathname-directory path)))
    (if (and (consp directory) (stringp (car (last directory))))
        (car (last directory))
        "")))

(defun welcome-recent-paths (app)
  "Return the recent drawings the start screen lists: those that still exist, as many as the window has room
for, the latest first.

APP: the application."
  (let* ((state (base:config-state-directory (app-config app)))
         (room (max 0 (min +welcome-maximum-recents+
                           (floor (- (app-window-height app) +welcome-clearance+)
                                  +welcome-row-height+))))
         (existing (remove-if-not (lambda (path)
                                    (eq :file (sbcl:file-kind path)))
                                  (persist:load-recent-files state))))
    (subseq existing 0 (min room (length existing)))))

(defun open-welcome (app)
  "Show the start screen over the window.

APP: the application."
  (setf (app-welcome app) (make-welcome (welcome-recent-paths app) 0)
        (app-menu app) nil)
  (app-request-redraw app))

(defun close-welcome (app)
  "Close the start screen, if it is open, and go on with the drawing as it is.

APP: the application."
  (when (app-welcome app)
    (setf (app-welcome app) nil)
    (app-request-redraw app)))

(defun open-recent-from-start (app
                               path)
  "Open a recent drawing from the start screen. The screen goes when the drawing opens, and stays when it
cannot be opened, so that another choice can be made.

APP: the application.
PATH: the pathname of the drawing."
  (load-drawing-into-app app (uiop:native-namestring path)))

(defun welcome-items (app)
  "Return the choices of the start screen, top to bottom: the recent drawings, then the actions.

APP: the application."
  (let ((welcome (app-welcome app)))
    (append (loop for path in (welcome-recents welcome)
                  for index from 0
                  collect (let ((path path))
                            (make-menu-item (list :recent index)
                                            (file-namestring path)
                                            :file
                                            (last-folder-name path)
                                            (lambda () (open-recent-from-start app path))
                                            nil)))
            (list (make-menu-item :new
                                  (i18n:tr "New drawing")
                                  :new
                                  "Ctrl+N"
                                  (lambda () (close-welcome app))
                                  nil)
                  (make-menu-item :open
                                  (i18n:tr "Open a drawing...")
                                  :open
                                  "Ctrl+O"
                                  (lambda ()
                                    (close-welcome app)
                                    (operation-open app))
                                  nil)
                  (make-menu-item :script
                                  (i18n:tr "Run a script...")
                                  :console
                                  nil
                                  (lambda ()
                                    (close-welcome app)
                                    (operation-run-script app))
                                  nil)
                  (make-menu-item :quit
                                  (i18n:tr "Quit")
                                  :close
                                  "Ctrl+Q"
                                  (lambda ()
                                    (close-welcome app)
                                    (operation-quit app))
                                  nil)))))

;;; Keys and pointer

(defun move-welcome-highlight (app
                               target)
  "Put the keyboard on another choice of the start screen.

APP: the application.
TARGET: :next or :previous, which wrap, or :first or :last."
  (let* ((welcome (app-welcome app))
         (count (length (welcome-items app)))
         (current (or (welcome-highlight welcome) 0)))
    (setf (welcome-highlight welcome)
          (ecase target
            (:next (mod (1+ current) count))
            (:previous (mod (1- current) count))
            (:first 0)
            (:last (1- count))))
    (app-request-redraw app)))

(defun handle-welcome-key (app
                           key)
  "Handle a key press while the start screen is open: the arrows move, Enter chooses and Escape goes on with
the empty drawing. Any other key is ignored.

APP: the application.
KEY: key keyword."
  (let ((welcome (app-welcome app)))
    (case key
      (:escape (close-welcome app))
      (:up (move-welcome-highlight app :previous))
      (:down (move-welcome-highlight app :next))
      (:home (move-welcome-highlight app :first))
      (:end (move-welcome-highlight app :last))
      (:enter (let ((item (nth (welcome-highlight welcome) (welcome-items app))))
                (when item
                  (funcall (menu-item-action item))))))
    (app-request-redraw app)))

(defun follow-pointer-in-welcome (app
                                  x
                                  y)
  "Move the highlight of the start screen to the choice under the pointer, if there is one.

APP: the application.
X: pointer window x.
Y: pointer window y."
  (let* ((welcome (app-welcome app))
         (hit (ui:widget-at (build-widgets app)
                            x
                            y))
         (id (and hit (consp (ui:widget-id hit)) (eq :welcome-item (first (ui:widget-id hit)))
                  (second (ui:widget-id hit))))
         (position (and id (position id
                                     (welcome-items app)
                                     :key #'menu-item-id
                                     :test #'equal))))
    (when (and position (not (eql position (welcome-highlight welcome))))
      (setf (welcome-highlight welcome) position)
      (app-request-redraw app))))

;;; Widgets

(defun welcome-card-height (recent-count)
  "Return the height of the start screen card for a number of recent drawings.

RECENT-COUNT: how many recent drawings it lists."
  (+ (* 2 +welcome-padding+)
     +welcome-header-height+
     +welcome-section-height+
     (* (max 1 recent-count) +welcome-row-height+)
     +welcome-group-gap+
     (* +welcome-action-count+ +welcome-row-height+)))

(defun welcome-row-widget (item
                           position
                           highlight
                           x
                           y
                           width)
  "Build the row of one choice of the start screen.

ITEM: the menu item.
POSITION: its place among the choices.
HIGHLIGHT: the place the keyboard is on.
X: left edge.
Y: top edge.
WIDTH: width."
  (ui:make-widget :row
                  (list :welcome-item (menu-item-id item))
                  x
                  y
                  width
                  +welcome-row-height+
                  :label (menu-item-label item)
                  :align :left
                  :icon (menu-item-icon item)
                  :shortcut (menu-item-shortcut item)
                  :action (menu-item-action item)
                  :active (eql position highlight)
                  :trailing (if (menu-item-shortcut item)
                                (+ +menu-shortcut-gap+
                                   (ui:text-width-pixels (menu-item-shortcut item)
                                                         ui:+small-font-pixels+))
                                0f0)))

(defun welcome-widgets (app)
  "Build the start screen: a veil over the window, the card with the brand and a question, the recent
drawings, and the other choices. Nothing when it is closed.

APP: the application."
  (let ((welcome (app-welcome app)))
    (when welcome
      (let* ((theme (app-theme app))
             (items (welcome-items app))
             (recent-count (length (welcome-recents welcome)))
             (window-width (float (app-window-width app) 1f0))
             (window-height (float (app-window-height app) 1f0))
             (height (welcome-card-height recent-count))
             (x (/ (- window-width +welcome-width+) 2))
             (y (max +menu-edge-margin+ (/ (- window-height height) 3)))
             (inner-x (+ x +welcome-padding+))
             (inner-width (- +welcome-width+ (* 2 +welcome-padding+)))
             (rows-y (+ y +welcome-padding+ +welcome-header-height+ +welcome-section-height+))
             (actions-y (+ rows-y (* (max 1 recent-count) +welcome-row-height+) +welcome-group-gap+))
             (highlight (welcome-highlight welcome)))
        (append
         (list (ui:make-widget :scrim
                               :scrim
                               0f0
                               0f0
                               window-width
                               window-height)
               (ui:make-widget :card
                               :welcome
                               x
                               y
                               +welcome-width+
                               height)
               (ui:make-widget :logo
                               :welcome-logo
                               inner-x
                               (+ y +welcome-padding+)
                               +welcome-logo-size+
                               +welcome-logo-size+)
               (make-label :welcome-title
                           "Borba CAD"
                           (+ inner-x +welcome-logo-size+ ui:+padding+)
                           (+ y +welcome-padding+)
                           (- inner-width +welcome-logo-size+ ui:+padding+)
                           (/ +welcome-logo-size+ 2)
                           :font :ui-strong
                           :size ui:+heading-font-pixels+
                           :color (scene:theme-text theme))
               (make-label :welcome-question
                           (i18n:tr "What would you like to do?")
                           (+ inner-x +welcome-logo-size+ ui:+padding+)
                           (+ y +welcome-padding+ (/ +welcome-logo-size+ 2))
                           (- inner-width +welcome-logo-size+ ui:+padding+)
                           (/ +welcome-logo-size+ 2)
                           :color (scene:theme-text-dim theme))
               (make-label :welcome-recent-heading
                           (i18n:tr "Recent drawings")
                           inner-x
                           (+ y +welcome-padding+ +welcome-header-height+)
                           inner-width
                           +welcome-section-height+
                           :color (scene:theme-text-faint theme)
                           :size ui:+small-font-pixels+))
         (if (zerop recent-count)
             (list (make-label :welcome-no-recent
                               (i18n:tr "Drawings you open or save will be listed here.")
                               inner-x
                               rows-y
                               inner-width
                               +welcome-row-height+
                               :color (scene:theme-text-faint theme)))
             '())
         (loop for item in items
               for position from 0
               collect (welcome-row-widget item
                                           position
                                           highlight
                                           inner-x
                                           (if (< position recent-count)
                                               (+ rows-y (* position +welcome-row-height+))
                                               (+ actions-y (* (- position recent-count) +welcome-row-height+)))
                                           inner-width)))))))
