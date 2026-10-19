(in-package #:borba-cad/app)

;;;; The console: a strip under the canvas where commands of the language of the program are typed. What
;;;; is typed and what comes back are kept as lines, the arrows walk through the commands already run,
;;;; and a command whose parentheses are not closed waits for its other lines. A command is run by the
;;;; script module as one step of undo, and is taken back whole if it fails.

(defconstant +console-padding+ 10f0
  "Space between the edge of the console and its contents, in pixels.")

(defconstant +console-title-height+ 30f0
  "Height of the title row of the console, in pixels.")

(defconstant +console-line-height+ 17f0
  "Height of one line of the output, in pixels.")

(defconstant +console-input-height+ 30f0
  "Height of the field where commands are typed, in pixels.")

(defconstant +console-font-pixels+ 12f0
  "Size of the text of the console, in pixels.")

(defconstant +console-prompt-width+ 26f0
  "Room the prompt gets before the field where commands are typed, in pixels.")

(defconstant +console-wheel-lines+ 3
  "Lines the output scrolls for each notch of the mouse wheel.")

(defconstant +console-wrapped-line-limit+ 40
  "Most rows a single line of the output may take once it is wrapped to the width of the console.")

(defparameter *console-prompt* ">"
  "The prompt shown before a command.")

(defparameter *console-continuation-prompt* "..."
  "The prompt shown before the next line of a command whose parentheses are not closed.")

(defparameter *console-value-marker* "=> "
  "What is written before a value that a command returned.")

(declaim (ftype function operation-zoom-extents))

;;; State

(defun console-open-p (app)
  "Return true while the console is shown.

APP: the application."
  (console-open (app-console app)))

(defun console-focused-p (app)
  "Return true when the keyboard is on the field where commands are typed.

APP: the application."
  (eq (app-focus app) :console-input))

(defun new-console-edit (&optional (text ""))
  "Return the edit state of the field where commands are typed, with the caret at the end of TEXT.

TEXT: what the field holds."
  (ui:make-edit-state :text text
                      :maximum-length +console-input-limit+))

(defun focus-console-input (app)
  "Give the keyboard to the field where commands are typed.

APP: the application."
  (setf (app-focus app) :console-input
        (app-edit app) (new-console-edit)
        (app-field-commit app) nil
        (app-field-original app) "")
  (app-request-redraw app))

(defun split-output-lines (text)
  "Return the lines of TEXT, without the empty lines at its end.

TEXT: what a command printed."
  (uiop:split-string (string-right-trim '(#\Newline) text)
                     :separator '(#\Newline)))

(defun console-say (app
                    kind
                    text)
  "Add TEXT to the output of the console, a line for each line of text, and show the end of the output.

APP: the application.
KIND: :input, :output, :value, :error or :note.
TEXT: what to say."
  (let ((console (app-console app)))
    (dolist (line (split-output-lines text))
      (push (cons kind line) (console-lines console)))
    (when (> (length (console-lines console)) +console-line-limit+)
      (setf (console-lines console) (subseq (console-lines console) 0 +console-line-limit+)))
    (setf (console-scroll console) 0)
    (app-request-redraw app)))

(defun resize-after-console-change (app)
  "Fit the viewport to the canvas as the console left it, keeping the zoom.

APP: the application."
  (setf (app-viewport app) (fit-viewport-to-window app (app-viewport app)))
  (refresh-cursor app)
  (app-request-redraw app))

(defun set-console-open (app
                         open)
  "Show or hide the console. Opening it gives it the keyboard.

APP: the application.
OPEN: true to show it, NIL to hide it."
  (let ((console (app-console app)))
    (unless (eq (and open t) (and (console-open console) t))
      (setf (console-open console) (and open t))
      (resize-after-console-change app))
    (cond (open
           (unless (console-lines console)
             (console-say app
                          :note
                          (i18n:tr
                           "Type a command and press Enter, for instance (line 0 0 1000 0). (help) lists them.")))
           (focus-console-input app))
          ((console-focused-p app)
           (stop-field-edit app)))))

(defun operation-toggle-console (app)
  "Show the console, or hide it when it is shown.

APP: the application."
  (set-console-open app (not (console-open-p app))))

;;; Running commands

(defun console-context (app)
  "Return the script context that commands run in: the drawing being edited, with the selection and the
view of the window.

APP: the application."
  (script:make-script-context (app-drawing app)
                              :selection-reader (lambda () (selection-ids app))
                              :selection-writer (lambda (ids) (select-ids app ids))
                              :view-fitter (lambda () (operation-zoom-extents app))))

(defun run-evaluation (app
                       evaluate
                       event)
  "Run something against the drawing as a single command. What it changed is recorded as one step of undo; if it
failed, ran out of time or ends inside an expression, nothing was changed. The definitions that it makes (commands,
key bindings, hooks) go to the customizations of the application. Returns the SCRIPT-RESULT and, as a second value, how
many entities it changed.

APP: the application.
EVALUATE: function of the script context that runs what is to be run and returns the SCRIPT-RESULT.
EVENT: the name the operation is timed and logged under."
  (let* ((context (console-context app))
         (started (sbcl:monotonic-nanoseconds))
         (result (let ((script:*customizations* (app-customizations app)))
                   (funcall evaluate context)))
         (changes (script:context-changes context)))
    (when changes
      (dom:history-record (app-history app) changes))
    (prune-selection app)
    (script:context-reset context)
    (obs:record-operation-result (app-telemetry app)
                                 event
                                 (- (sbcl:monotonic-nanoseconds) started)
                                 :info
                                 :forms (script:script-result-forms result)
                                 :changed (length changes)
                                 :entity-count (dom:drawing-entity-count (app-drawing app))
                                 :result (script:script-result-status result))
    (when changes
      (set-message app
                   (i18n:trf "~d ~:*~[changes~;change~:;changes~] made; Ctrl+Z takes them all back." (length changes))))
    (app-request-redraw app)
    (values result (length changes))))

(defun run-source (app
                   text
                   &key (timeout script:+default-timeout-seconds+)
                        (event "console.eval"))
  "Run TEXT as a single command on the drawing. What it changed is recorded as one step of undo; if it
failed, ran out of time or ends inside an expression, nothing was changed. Returns the SCRIPT-RESULT.

APP: the application.
TEXT: Lisp source.
TIMEOUT: the longest time to run, in seconds.
EVENT: the name the operation is timed and logged under."
  (run-evaluation app
                  (lambda (context)
                    (script:evaluate-source context
                                            text
                                            :timeout timeout))
                  event))

(defun run-function (app
                     function
                     &key (timeout script:+default-timeout-seconds+)
                          (event "command.run"))
  "Call a function as a single command on the drawing, as RUN-SOURCE does with a text. Returns the SCRIPT-RESULT and how
many entities it changed.

APP: the application.
FUNCTION: function of no arguments, such as a command that the init file defined.
TIMEOUT: the longest time to run, in seconds.
EVENT: the name the operation is timed and logged under."
  (run-evaluation app
                  (lambda (context)
                    (script:evaluate-function context
                                              function
                                              :timeout timeout))
                  event))

(defun show-source-result (app
                           result)
  "Write in the console what a command printed and returned, or why it failed.

APP: the application.
RESULT: the SCRIPT-RESULT."
  (let ((output (script:script-result-output result)))
    (when (plusp (length output))
      (console-say app
                   :output
                   output)))
  (ecase (script:script-result-status result)
    (:ok
     (dolist (value (script:script-result-values result))
       (console-say app
                    :value
                    (concatenate 'string *console-value-marker* value))))
    ((:error :timeout)
     (console-say app
                  :error
                  (script:script-result-message result)))
    (:incomplete nil)))

(defun source-on-one-line (text)
  "Return TEXT without its comments and on a single line, as it can be put in the field of the console.

TEXT: Lisp source."
  (substitute #\Space #\Newline (script:strip-comments text)))

(defun remember-command (app
                         source)
  "Keep a command that was run, for the up arrow, unless it is the one before.

APP: the application.
SOURCE: the text that was run."
  (let* ((console (app-console app))
         (line (string-trim " " (source-on-one-line source))))
    (when (and (plusp (length line))
               (not (equal line (first (console-history console)))))
      (push line (console-history console))
      (when (> (length (console-history console)) +console-history-limit+)
        (setf (console-history console) (subseq (console-history console) 0 +console-history-limit+))))
    (setf (console-history-position console) nil
          (console-draft console) "")))

(defun console-submit (app)
  "Run the line typed in the console, together with the lines before it that left an expression open; if
the expression is still open afterwards, wait for the next line.

APP: the application."
  (let* ((console (app-console app))
         (typed (ui:edit-state-text (app-edit app)))
         (continued (plusp (length (console-pending console))))
         (source (if continued
                     (format nil "~a~%~a" (console-pending console) typed)
                     typed)))
    (console-say app
                 :input
                 (format nil "~a ~a"
                         (if continued *console-continuation-prompt* *console-prompt*)
                         typed))
    (setf (app-edit app) (new-console-edit))
    (cond ((zerop (length (string-trim '(#\Space #\Tab #\Newline) source)))
           (setf (console-pending console) ""))
          (t
           (let ((result (run-source app source)))
             (cond ((eq (script:script-result-status result) :incomplete)
                    (setf (console-pending console) source))
                   (t
                    (setf (console-pending console) "")
                    (remember-command app source)
                    (show-source-result app result))))))))

(defun console-history-step (app
                             direction)
  "Put an earlier or a later command of the history in the field.

APP: the application.
DIRECTION: :older or :newer."
  (let* ((console (app-console app))
         (history (console-history console))
         (position (console-history-position console)))
    (flet ((show (text)
             (setf (app-edit app) (new-console-edit text))))
      (ecase direction
        (:older
         (when history
           (unless position
             (setf (console-draft console) (ui:edit-state-text (app-edit app))))
           (let ((next (if position
                           (min (1+ position) (1- (length history)))
                           0)))
             (setf (console-history-position console) next)
             (show (nth next history)))))
        (:newer
         (when position
           (cond ((zerop position)
                  (setf (console-history-position console) nil)
                  (show (console-draft console)))
                 (t
                  (setf (console-history-position console) (1- position))
                  (show (nth (1- position) history))))))))))

;;; Looking at the output

(defun console-rect (app)
  "Return the strip of the window the console takes.

APP: the application."
  (ui:layout-console (app-layout app)))

(defun console-output-width (rect)
  "Return the width the lines of the output may have, in pixels.

RECT: the strip of the console."
  (max 1f0 (- (ui:rect-width rect) (* 2 +console-padding+))))

(defun console-visible-line-count (rect)
  "Return how many lines of the output the strip shows.

RECT: the strip of the console."
  (max 0 (floor (- (ui:rect-height rect)
                   +console-title-height+
                   +console-input-height+
                   (* 2 +console-padding+))
                +console-line-height+)))

(defun wrapped-console-rows (line
                             width)
  "Return the rows a line of the output takes at a width, as (kind . text) in reading order.

LINE: a (kind . text) line of the console.
WIDTH: the width of a row in pixels."
  (loop for row in (ui:wrap-text (cdr line)
                                 width
                                 +console-wrapped-line-limit+
                                 +console-font-pixels+
                                 :mono)
        collect (cons (car line) row)))

(defun console-rows (app
                     width
                     count)
  "Return the rows of the output that are in view, the oldest first: COUNT rows ending where the scroll
says, each as (kind . text).

APP: the application.
WIDTH: the width of a row in pixels.
COUNT: how many rows fit."
  (let* ((console (app-console app))
         (wanted (+ count (console-scroll console)))
         (rows '()))
    (loop for line in (console-lines console)
          while (< (length rows) wanted)
          do (setf rows (append rows (reverse (wrapped-console-rows line width)))))
    (reverse (subseq rows
                     (min (console-scroll console) (length rows))
                     (min wanted (length rows))))))

(defun console-row-count (app
                          width)
  "Return how many rows all the output takes at a width.

APP: the application.
WIDTH: the width of a row in pixels."
  (loop for line in (console-lines (app-console app))
        sum (length (wrapped-console-rows line width))))

(defun console-scroll-by (app
                          rows)
  "Scroll the output up by ROWS, or down when negative, within the output.

APP: the application.
ROWS: how many rows."
  (let* ((console (app-console app))
         (rect (console-rect app))
         (hidden (max 0 (- (console-row-count app (console-output-width rect))
                           (console-visible-line-count rect)))))
    (setf (console-scroll console) (max 0 (min hidden (+ (console-scroll console) rows))))
    (app-request-redraw app)))

(defun console-clear (app)
  "Take the output out of the console.

APP: the application."
  (let ((console (app-console app)))
    (setf (console-lines console) '()
          (console-scroll console) 0)
    (app-request-redraw app)))

;;; Keys

(defun console-paste (app)
  "Put the text of the clipboard in the field, without its comments and on one line.

APP: the application."
  (let ((text (clipboard-text app)))
    (when text
      (setf (app-edit app) (ui:edit-insert (app-edit app)
                                           (source-on-one-line text))))))

(defun console-escape (app)
  "Give up the command being continued, or when there is none give the keyboard back to the canvas.

APP: the application."
  (let ((console (app-console app)))
    (cond ((plusp (length (console-pending console)))
           (setf (console-pending console) "")
           (console-say app
                        :note
                        (i18n:tr "The unfinished command was dropped.")))
          (t
           (stop-field-edit app)))))

(defun handle-console-key (app
                           key
                           modifiers)
  "Handle a key press while the keyboard is on the field of the console.

APP: the application.
KEY: key keyword.
MODIFIERS: modifier keys held."
  (let ((control (modifier-p modifiers :control))
        (page (max 1 (1- (console-visible-line-count (console-rect app))))))
    (cond ((eq key :enter) (console-submit app))
          ((eq key :escape) (console-escape app))
          ((eq key :f2) (set-console-open app nil))
          ((eq key :up) (console-history-step app :older))
          ((eq key :down) (console-history-step app :newer))
          ((eq key :page-up) (console-scroll-by app page))
          ((eq key :page-down) (console-scroll-by app (- page)))
          ((and control (eq key :l)) (console-clear app))
          ((and control (eq key :k)) (operation-command-palette app))
          ((and control (eq key :v)) (console-paste app))
          (t
           (let ((edited (edit-with-clipboard app
                                              (app-edit app)
                                              key
                                              modifiers)))
             (when edited
               (setf (app-edit app) edited))))))
  (app-request-redraw app))

;;; Widgets

(defun console-row-color (theme
                          kind)
  "Return the color a row of the output is drawn in.

THEME: the theme.
KIND: :input, :output, :value, :error or :note."
  (ecase kind
    (:input (scene:theme-text-dim theme))
    (:output (scene:theme-text theme))
    (:value (scene:theme-accent theme))
    (:error (scene:theme-danger theme))
    (:note (scene:theme-text-faint theme))))

(defun console-widgets (app
                        blocked)
  "Build the widgets of the console: its strip, the title row with the button that closes it, the rows of
the output in view, and the prompt with the field where commands are typed. Nothing while it is closed.

APP: the application.
BLOCKED: true while a modal covers the window."
  (let ((rect (console-rect app)))
    (when (and (console-open-p app) (plusp (ui:rect-height rect)))
      (let* ((theme (app-theme app))
             (console (app-console app))
             (left (+ (ui:rect-x rect) +console-padding+))
             (top (ui:rect-y rect))
             (width (console-output-width rect))
             (count (console-visible-line-count rect))
             (rows (console-rows app
                                 width
                                 count))
             (lines-top (+ top +console-title-height+))
             (input-top (- (+ top (ui:rect-height rect)) +console-padding+ +console-input-height+)))
        (append
         (list (ui:make-widget :panel
                               :console
                               (ui:rect-x rect)
                               top
                               (ui:rect-width rect)
                               (ui:rect-height rect)
                               :variant :console)
               (make-label :console-title
                           (i18n:tr "Console")
                           left
                           top
                           120f0
                           +console-title-height+
                           :font :ui-strong
                           :color (scene:theme-text theme))
               (make-label :console-hint
                           (i18n:tr "Lengths are in millimeters and angles in degrees; Up and Down recall commands.")
                           (+ left 90f0)
                           top
                           (max 0f0 (- width 90f0 +console-title-height+))
                           +console-title-height+
                           :color (scene:theme-text-faint theme)
                           :size ui:+small-font-pixels+)
               (make-icon-button :console-close
                                 :close
                                 (- (+ left width) ui:+icon-button-size+)
                                 (+ top (/ (- +console-title-height+ ui:+icon-button-size+) 2))
                                 :action (lambda () (set-console-open app nil))
                                 :blocked blocked
                                 :hint (i18n:tr "Close the console")
                                 :shortcut "F2"
                                 :variant :ghost
                                 :size ui:+icon-button-size+))
         (loop for (kind . text) in rows
               for index from (- count (length rows))
               collect (make-label (list :console-row index)
                                   text
                                   left
                                   (+ lines-top (* index +console-line-height+))
                                   width
                                   +console-line-height+
                                   :font :mono
                                   :size +console-font-pixels+
                                   :color (console-row-color theme kind)))
         (list (make-label :console-prompt
                           (if (plusp (length (console-pending console)))
                               *console-continuation-prompt*
                               *console-prompt*)
                           left
                           input-top
                           +console-prompt-width+
                           +console-input-height+
                           :font :mono
                           :size +console-font-pixels+
                           :color (scene:theme-accent theme))
               (ui:make-widget :field
                               :console-input
                               (+ left +console-prompt-width+)
                               input-top
                               (- width +console-prompt-width+)
                               +console-input-height+
                               :label ""
                               :align :left
                               :font :mono
                               :size +console-font-pixels+
                               :blocked blocked
                               :edit (and (console-focused-p app) (app-edit app))
                               :action (lambda () (focus-console-input app))
                               :hint (i18n:tr "Type a command and press Enter"))))))))
