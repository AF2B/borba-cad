(in-package #:borba-cad/app)

;;;; Drawing with a constraint and with typed numbers. Ortho holds the next point to a horizontal or vertical
;;;; line from the last one, polar to the nearest multiple of an angle; and a tool that waits for a point can be
;;;; given it by typing: a length (toward the pointer), x;y, @x;y or length<angle.

(defconstant +polar-increment-degrees+ 15d0
  "The angle, in degrees, whose multiples polar holds the next point to.")

(defun constrain-point (mode
                        anchor
                        x
                        y)
  "Return the point that MODE holds (X, Y) to, as two values: with :ortho, the nearest point of the horizontal or
the vertical line through the anchor; with :polar, the nearest point of the ray from the anchor at a multiple
of the polar increment.

MODE: :ortho or :polar.
ANCHOR: an (x . y) cons, the last point.
X: world x of the point.
Y: world y of the point."
  (let ((dx (- x (car anchor)))
        (dy (- y (cdr anchor))))
    (ecase mode
      (:ortho
       (if (>= (abs dx) (abs dy))
           (values x (cdr anchor))
           (values (car anchor) y)))
      (:polar
       (let* ((step (base:degrees->radians +polar-increment-degrees+))
              (angle (* step (round (atan dy dx) step)))
              (distance (+ (* dx (cos angle)) (* dy (sin angle)))))
         (values (+ (car anchor) (* distance (cos angle)))
                 (+ (cdr anchor) (* distance (sin angle)))))))))

(defun point-input-problem (text
                            reason)
  "Signal INVALID-INPUT for a point that was typed.

TEXT: what was typed.
REASON: why it cannot be a point."
  (error 'base:invalid-input
         :text text
         :reason reason))

(defun parse-point-input (text
                          anchor
                          pointer
                          unit)
  "Return the point that typed TEXT names, as two values in millimetres. Signals INVALID-INPUT when it names none.
The forms are: a length, which goes from the anchor toward the pointer; x;y, an absolute point; @x;y, a point
at that distance from the anchor along x and y; and length<angle, a point at that length from the anchor in
that direction in degrees (with the @ or without it). Without an anchor the origin takes its place.

TEXT: what was typed.
ANCHOR: an (x . y) cons, the last point, or NIL for the first.
POINTER: an (x . y) cons, where the pointer is, or NIL.
UNIT: the display unit, assumed for numbers without a unit."
  (let* ((trimmed (string-trim " " text))
         (relative (and (plusp (length trimmed)) (char= #\@ (char trimmed 0))))
         (body (string-trim " " (if relative (subseq trimmed 1) trimmed)))
         (base-x (if anchor (car anchor) 0d0))
         (base-y (if anchor (cdr anchor) 0d0))
         (angle-mark (position #\< body))
         (separator (position #\; body)))
    (cond ((zerop (length body))
           (point-input-problem text (i18n:tr "type a length, x;y, @x;y or length<angle")))
          (angle-mark
           (let ((distance (base:parse-length (subseq body 0 angle-mark) unit))
                 (angle (base:degrees->radians (base:parse-decimal (subseq body (1+ angle-mark))))))
             (values (+ base-x (* distance (cos angle)))
                     (+ base-y (* distance (sin angle))))))
          (separator
           (let ((x (base:parse-length (subseq body 0 separator) unit))
                 (y (base:parse-length (subseq body (1+ separator)) unit)))
             (if relative
                 (values (+ base-x x) (+ base-y y))
                 (values x y))))
          ((null anchor)
           (point-input-problem text (i18n:tr "type the first point as x;y")))
          (t
           (let* ((distance (base:parse-length body unit))
                  (dx (if pointer (- (car pointer) (car anchor)) 0d0))
                  (dy (if pointer (- (cdr pointer) (cdr anchor)) 0d0))
                  (length (sqrt (+ (* dx dx) (* dy dy)))))
             (when (zerop length)
               (point-input-problem text
                                    (i18n:tr "move the pointer to say which way, or type x;y or length<angle")))
             (values (+ (car anchor) (* distance (/ dx length)))
                     (+ (cdr anchor) (* distance (/ dy length)))))))))

;;; Toggling

(defun set-constraint (app
                       mode)
  "Hold the next point to MODE, or release it.

APP: the application.
MODE: :ortho, :polar or NIL."
  (setf (app-constraint app) mode)
  (refresh-cursor app)
  (app-request-redraw app))

(defun operation-toggle-ortho (app)
  "Turn ortho on, which holds the next point to a horizontal or vertical line from the last one, or off. It
replaces polar.

APP: the application."
  (set-constraint app (if (eq (app-constraint app) :ortho) nil :ortho)))

(defun operation-toggle-polar (app)
  "Turn polar on, which holds the next point to the nearest multiple of 15 degrees from the last one, or off. It
replaces ortho.

APP: the application."
  (set-constraint app (if (eq (app-constraint app) :polar) nil :polar)))

;;; Typing a point

(defun coordinate-start-character-p (character)
  "Return true when CHARACTER can begin a point that is typed.

CHARACTER: a character."
  (or (digit-char-p character)
      (find character "-+@.")))

(defun coordinate-input-available-p (app)
  "Return true when a typed point would be taken: a drawing tool is active and nothing else has the keyboard.

APP: the application."
  (and (not (eq :select (tool-kind (app-tool app))))
       (not (or (app-prompt app) (app-dialog app) (app-menu app) (app-welcome app) (app-focus app)))))

(defun cancel-coordinate-input (app)
  "Throw away what is being typed as a point.

APP: the application."
  (when (app-coordinate-edit app)
    (setf (app-coordinate-edit app) nil)
    (app-request-redraw app)))

(defun type-into-coordinate-input (app
                                   text)
  "Add typed text to the point being typed, or begin one when the text can begin a point.

APP: the application.
TEXT: what was typed."
  (when (coordinate-input-available-p app)
    (cond ((app-coordinate-edit app)
           (setf (app-coordinate-edit app) (ui:edit-insert (app-coordinate-edit app) text)))
          ((and (plusp (length text)) (coordinate-start-character-p (char text 0)))
           (setf (app-coordinate-edit app) (ui:edit-insert (ui:make-edit-state :maximum-length 64) text))))
    (app-request-redraw app)))

(defun plain-number (text)
  "Return the number TEXT is when it is only a number, with no @, no semicolon and no angle mark, and no unit;
otherwise NIL.

TEXT: what was typed."
  (and (not (find-if (lambda (character) (find character "@;<")) text))
       (handler-case (base:parse-decimal (string-trim " " text))
         (base:invalid-input ()
           nil))))

(defun commit-coordinate-input (app)
  "Give the tool the point that was typed, as if it had been clicked. A text that names no point is said and kept
for correcting.

APP: the application."
  (let* ((tool (app-tool app))
         (cursor (app-cursor app))
         (text (ui:edit-state-text (app-coordinate-edit app)))
         (number (plain-number text)))
    (when (and number (tool-typed-number tool
                                         app
                                         number))
      (setf (app-coordinate-edit app) nil)
      (app-request-redraw app)
      (return-from commit-coordinate-input nil))
    (handler-case
        (multiple-value-bind (x y)
            (parse-point-input text
                               (tool-anchor tool)
                               (and cursor (cons (cursor-x cursor) (cursor-y cursor)))
                               (dom:drawing-display-unit (app-drawing app)))
          (setf (app-coordinate-edit app) nil)
          (tool-pointer-down tool
                             app
                             (make-cursor x
                                          y
                                          x
                                          y
                                          nil
                                          nil))
          (refresh-cursor app))
      (base:invalid-input (condition)
        (set-message app
                     (remove #\Newline (princ-to-string condition))
                     :error)))
    (app-request-redraw app)))

(defun handle-coordinate-key (app
                              key
                              modifiers)
  "Handle a key press while a point is being typed: Enter gives it to the tool, Escape throws it away, and the
editing keys edit it.

APP: the application.
KEY: key keyword.
MODIFIERS: modifier keys held."
  (case key
    (:enter (commit-coordinate-input app))
    (:escape (cancel-coordinate-input app))
    (t (let ((edited (edit-with-clipboard app
                                          (app-coordinate-edit app)
                                          key
                                          modifiers)))
         (when edited
           (setf (app-coordinate-edit app) edited)))))
  (app-request-redraw app))

(defun coordinate-input-widgets (app)
  "Build the box where a point is typed, at the bottom of the canvas, with what it accepts under it. Nothing
when no point is being typed.

APP: the application."
  (let ((edit (app-coordinate-edit app)))
    (when edit
      (let* ((canvas (canvas-rect app))
             (theme (app-theme app))
             (x (+ (ui:rect-x canvas) ui:+padding+))
             (width (min 420f0 (- (ui:rect-width canvas) (* 2 ui:+padding+))))
             (bottom (- (+ (ui:rect-y canvas) (ui:rect-height canvas)) ui:+padding+)))
        (list (make-label :coordinate-hint
                          (i18n:tr "Length, x;y, @x;y or length<angle. Enter accepts, Esc cancels.")
                          x
                          (- bottom 20f0)
                          width
                          20f0
                          :color (scene:theme-text-dim theme)
                          :size ui:+small-font-pixels+)
              (ui:make-widget :field
                              :coordinate-input
                              x
                              (- bottom 20f0 ui:+row-height+)
                              width
                              ui:+row-height+
                              :label (ui:edit-state-text edit)
                              :align :left
                              :font :mono
                              :edit edit))))))
