(in-package #:borba-cad/app)

;;;; Drop-down menus: a card with one row per choice that opens beside the button it hangs from.
;;;; The draw menu lists every creation tool, and the language menu lists the interface languages.
;;;; While a menu is open the rest of the window ignores the pointer, a click anywhere else closes
;;;; it, and the keyboard moves through it.

(defconstant +menu-row-height+ 32f0
  "Height of one choice of a menu, in pixels.")

(defconstant +menu-padding+ 6f0
  "Space between the edge of a menu card and its rows, in pixels.")

(defconstant +menu-distance+ 8f0
  "Gap between the button a menu hangs from and the menu, in pixels.")

(defconstant +menu-edge-margin+ 8f0
  "Closest a menu comes to the edge of the window, in pixels.")

(defconstant +menu-minimum-width+ 200f0
  "Narrowest a menu card is, in pixels.")

(defconstant +menu-shortcut-gap+ 28f0
  "Space between the name of a choice and its shortcut, in pixels.")

(defconstant +menu-separator-height+ 9f0
  "Height of the space that separates two groups of choices, in pixels.")

(defconstant +menu-row-icon-room+ 24f0
  "Room a row gives its icon before its text, in pixels.")

(defparameter *draw-menu-tools*
  (list (list :line :line (i18n:mark "Line") "L")
        (list :polyline :polyline (i18n:mark "Polyline") "P")
        (list :rectangle :rectangle (i18n:mark "Rectangle") "R")
        (list :polygon :polygon (i18n:mark "Polygon") "N")
        (list :circle :circle (i18n:mark "Circle") "C")
        (list :arc :arc (i18n:mark "Arc") "A")
        (list :ellipse :ellipse (i18n:mark "Ellipse") "E")
        nil
        (list :text :text (i18n:mark "Text") "T")
        (list :dimension :dimension (i18n:mark "Dimension") "D"))
  "The choices of the draw menu, top to bottom, as (tool-kind icon label shortcut), with NIL between
groups. The labels are in English and are translated where the menu is built.")

(defstruct (menu-item (:constructor make-menu-item (id
                                                    label
                                                    icon
                                                    shortcut
                                                    action
                                                    current)))
  "One choice of a menu.

ID: identifies the choice.
LABEL: what it says.
ICON: the name of its icon, or NIL.
SHORTCUT: the keys that do the same, or NIL.
ACTION: function of no arguments run when it is chosen.
CURRENT: true for the choice that is in force now."
  id
  label
  icon
  shortcut
  action
  current)

(defun close-menu (app)
  "Close the open menu, if there is one.

APP: the application."
  (when (app-menu app)
    (setf (app-menu app) nil)
    (app-request-redraw app)))

(defun choose-menu-tool (app
                         kind)
  "Close the menu and make KIND the active tool.

APP: the application.
KIND: a tool kind keyword."
  (close-menu app)
  (operation-set-tool app kind))

(defun draw-menu-items (app)
  "Return the choices of the draw menu, with NIL between groups.

APP: the application."
  (let ((active (tool-kind (app-tool app))))
    (loop for spec in *draw-menu-tools*
          collect (when spec
                    (destructuring-bind (kind icon label shortcut) spec
                      (let ((kind kind))
                        (make-menu-item kind
                                        (i18n:tr label)
                                        icon
                                        shortcut
                                        (lambda () (choose-menu-tool app kind))
                                        (eq kind active))))))))

(defun modify-menu-items (app)
  "Return the choices of the modify menu: the tools that change the selection.

APP: the application."
  (let ((active (tool-kind (app-tool app))))
    (loop for (kind icon label shortcut) in *modify-menu-tools*
          collect (let ((kind kind))
                    (make-menu-item kind
                                    (i18n:tr label)
                                    icon
                                    shortcut
                                    (lambda () (choose-menu-tool app kind))
                                    (eq kind active))))))

(defun language-menu-items (app)
  "Return the choices of the language menu: one per language, each named in its own language.

APP: the application."
  (loop for language in (i18n:supported-languages)
        collect (let ((language language))
                  (make-menu-item language
                                  (i18n:language-name language)
                                  nil
                                  nil
                                  (lambda ()
                                    (close-menu app)
                                    (operation-set-language app language))
                                  (eq language i18n:*language*)))))

(defun more-menu-items (app)
  "Return the choices of the menu of the other things the program does with files.

APP: the application."
  (flet ((choice (id label icon shortcut function)
           (make-menu-item id
                           label
                           icon
                           shortcut
                           (lambda ()
                             (close-menu app)
                             (funcall function app))
                           nil)))
    (list (choice :import-dxf (i18n:tr "Import DXF...") :import "Ctrl+Shift+I" #'operation-import-dxf)
          (choice :export-dxf (i18n:tr "Export DXF...") :export "Ctrl+Shift+E" #'operation-export-dxf)
          (choice :export-image (i18n:tr "Export as JPEG...") :export nil #'operation-export-image)
          nil
          (choice :console (i18n:tr "Console") :console "F2" #'operation-toggle-console)
          (choice :run-script (i18n:tr "Run script...") :open nil #'operation-run-script)
          (choice :export-script (i18n:tr "Export as script...") :export nil #'operation-export-script))))

(defun menu-items (app
                   kind)
  "Return the choices of the menu KIND, with NIL between groups.

APP: the application.
KIND: :draw, :modify, :language or :more."
  (ecase kind
    (:draw (draw-menu-items app))
    (:modify (modify-menu-items app))
    (:language (language-menu-items app))
    (:more (more-menu-items app))))

(defun selectable-items (items)
  "Return the choices among ITEMS, without the NILs that separate groups.

ITEMS: a list of menu items and NILs."
  (remove nil items))

(defun open-menu (app
                  kind
                  anchor)
  "Open the menu KIND hanging from the button ANCHOR, with the keyboard on the choice in force.

APP: the application.
KIND: :draw, :language or :more.
ANCHOR: id of the button."
  (let* ((items (selectable-items (menu-items app kind)))
         (current (position-if #'menu-item-current items)))
    (setf (app-menu app) (make-menu kind
                                    anchor
                                    (or current 0)))
    (app-request-redraw app)))

(defun toggle-menu (app
                    kind
                    anchor)
  "Open the menu KIND, or close it when it is already open.

APP: the application.
KIND: :draw, :language or :more.
ANCHOR: id of the button it hangs from."
  (if (and (app-menu app) (eq (menu-kind (app-menu app)) kind))
      (close-menu app)
      (open-menu app
                 kind
                 anchor)))

(defun move-menu-highlight (app
                            target)
  "Put the keyboard on another choice of the open menu.

APP: the application.
TARGET: a position among the choices, or :next or :previous to step and wrap, :first or :last."
  (let* ((menu (app-menu app))
         (count (length (selectable-items (menu-items app (menu-kind menu)))))
         (current (or (menu-highlight menu) 0)))
    (setf (menu-highlight menu)
          (cond ((integerp target) (max 0 (min (1- count) target)))
                ((eq target :next) (mod (1+ current) count))
                ((eq target :previous) (mod (1- current) count))
                ((eq target :first) 0)
                (t (1- count))))
    (app-request-redraw app)))

(defun activate-menu-highlight (app)
  "Run the action of the choice the keyboard is on.

APP: the application."
  (let* ((menu (app-menu app))
         (items (selectable-items (menu-items app (menu-kind menu))))
         (item (and (menu-highlight menu) (nth (menu-highlight menu) items))))
    (when item
      (funcall (menu-item-action item)))))

(defun handle-menu-key (app
                        key)
  "Handle a key press while a menu is open: the arrows move, Enter chooses, Escape closes, and a
tool's own letter chooses that tool from the draw menu. Any other key is ignored.

APP: the application.
KEY: key keyword."
  (let* ((menu (app-menu app))
         (tool (cdr (assoc key *tool-shortcuts*))))
    (cond ((eq key :escape) (close-menu app))
          ((eq key :up) (move-menu-highlight app :previous))
          ((eq key :down) (move-menu-highlight app :next))
          ((eq key :home) (move-menu-highlight app :first))
          ((eq key :end) (move-menu-highlight app :last))
          ((eq key :enter) (activate-menu-highlight app))
          ((and tool (eq (menu-kind menu) :draw)) (choose-menu-tool app tool)))
    (app-request-redraw app)))

;;; Widgets

(defun menu-row-width (items)
  "Return the width of the rows of a menu: its widest name, the room of the icon, and the
widest shortcut.

ITEMS: the selectable choices."
  (let ((label (reduce #'max (mapcar (lambda (item) (ui:text-width-pixels (menu-item-label item))) items)
                       :initial-value 0f0))
        (shortcut (reduce #'max
                          (mapcar (lambda (item)
                                    (if (menu-item-shortcut item)
                                        (ui:text-width-pixels (menu-item-shortcut item) ui:+small-font-pixels+)
                                        0f0))
                                  items)
                          :initial-value 0f0)))
    (+ (* 2 ui:+padding+)
       +menu-row-icon-room+
       label
       (if (plusp shortcut) (+ +menu-shortcut-gap+ shortcut) 0f0))))

(defun menu-height (items)
  "Return the height of a menu card holding ITEMS.

ITEMS: a list of menu items, with NIL between groups."
  (+ (* 2 +menu-padding+)
     (loop for item in items
           sum (if item +menu-row-height+ +menu-separator-height+))))

(defun menu-origin (app
                    anchor
                    width
                    height)
  "Return the top-left corner of a menu card: beside a palette button, under a title bar button,
and kept inside the window either way.

APP: the application.
ANCHOR: the button it hangs from.
WIDTH: the width of the card.
HEIGHT: the height of the card."
  (let ((window-width (float (app-window-width app) 1f0))
        (window-height (float (app-window-height app) 1f0)))
    (multiple-value-bind (x y)
        (if (eq (ui:widget-variant anchor) :tool)
            (values (+ (ui:widget-x anchor) (ui:widget-width anchor) +menu-distance+)
                    (ui:widget-y anchor))
            (values (- (+ (ui:widget-x anchor) (ui:widget-width anchor)) width)
                    (+ (ui:widget-y anchor) (ui:widget-height anchor) +menu-distance+)))
      (values (max +menu-edge-margin+ (min x (- window-width width +menu-edge-margin+)))
              (max +menu-edge-margin+
                   (min y (- window-height ui:+status-height+ height +menu-edge-margin+)))))))

(defun menu-widgets (app
                     widgets)
  "Build the open menu: its card and its rows. Nothing is built when no menu is open, or when its
button is not among WIDGETS.

APP: the application.
WIDGETS: the widgets of the window, in which the button the menu hangs from is looked for."
  (let* ((menu (app-menu app))
         (anchor (and menu (find (menu-anchor menu) widgets :key #'ui:widget-id :test #'equal))))
    (when anchor
      (let* ((items (menu-items app (menu-kind menu)))
             (selectable (selectable-items items))
             (row-width (menu-row-width selectable))
             (width (max +menu-minimum-width+ (+ row-width (* 2 +menu-padding+))))
             (height (menu-height items)))
        (multiple-value-bind (x y)
            (menu-origin app
                         anchor
                         width
                         height)
          (let ((cursor-y (+ y +menu-padding+))
                (position 0)
                (separators 0)
                (built (list (ui:make-widget :card
                                               :menu
                                               x
                                               y
                                               width
                                               height))))
            (dolist (item items)
              (cond ((null item)
                     (push (make-separator (list :menu-separator (incf separators))
                                           (+ x ui:+padding+)
                                           (+ cursor-y (/ +menu-separator-height+ 2))
                                           (- width (* 2 ui:+padding+))
                                           +separator-width+)
                           built)
                     (incf cursor-y +menu-separator-height+))
                    (t
                     (push (ui:make-widget :row
                                           (list :menu-item (menu-item-id item))
                                           (+ x +menu-padding+)
                                           cursor-y
                                           (- width (* 2 +menu-padding+))
                                           +menu-row-height+
                                           :label (menu-item-label item)
                                           :align :left
                                           :icon (menu-item-icon item)
                                           :shortcut (menu-item-shortcut item)
                                           :action (menu-item-action item)
                                           :active (eql position (menu-highlight menu))
                                           :trailing (if (menu-item-shortcut item)
                                                         (+ +menu-shortcut-gap+
                                                            (ui:text-width-pixels (menu-item-shortcut item)
                                                                                  ui:+small-font-pixels+))
                                                         0f0))
                           built)
                     (incf position)
                     (incf cursor-y +menu-row-height+))))
            (nreverse built)))))))
