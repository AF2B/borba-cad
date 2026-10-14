(in-package #:borba-cad/ui)

;;;; Single-line text editing over code points. The state is a value: every operation returns a
;;;; new state, so tests need no widget and no input device.

(defconstant +default-maximum-edit-length+ 256
  "Longest text a field accepts unless the caller says otherwise.")

(defstruct (edit-state (:constructor make-edit-state (&key (text "")
                                                           (caret (length text))
                                                           (maximum-length +default-maximum-edit-length+)
                                                           (replace-on-type nil))))
  "The content of a text field.

TEXT: the characters.
CARET: insertion position, from 0 to the length.
MAXIMUM-LENGTH: longest accepted text.
REPLACE-ON-TYPE: when true the whole text is selected: the next typed character replaces it."
  (text "" :type string :read-only t)
  (caret 0 :type fixnum :read-only t)
  (maximum-length +default-maximum-edit-length+ :type fixnum :read-only t)
  (replace-on-type nil :type boolean :read-only t))

(defun printable-character-p (character)
  "Return true for characters a text field accepts.

CHARACTER: a character."
  (let ((code (char-code character)))
    (and (>= code 32) (/= code 127))))

(defun edit-insert (state
                    string)
  "Insert STRING at the caret (replacing everything first when the text is selected);
characters that are not printable are dropped, and the result is cut to the maximum length.

STATE: the edit state.
STRING: typed text."
  (let* ((clean (remove-if-not #'printable-character-p string))
         (base (if (edit-state-replace-on-type state) "" (edit-state-text state)))
         (caret (if (edit-state-replace-on-type state) 0 (edit-state-caret state)))
         (room (max 0 (- (edit-state-maximum-length state) (length base))))
         (inserted (subseq clean 0 (min room (length clean)))))
    (make-edit-state :text (concatenate 'string (subseq base 0 caret) inserted (subseq base caret))
                     :caret (+ caret (length inserted))
                     :maximum-length (edit-state-maximum-length state))))

(defun edit-backspace (state)
  "Delete the character before the caret, or the whole selection.

STATE: the edit state."
  (cond ((edit-state-replace-on-type state)
         (make-edit-state :text "" :maximum-length (edit-state-maximum-length state)))
        ((zerop (edit-state-caret state)) state)
        (t (let ((text (edit-state-text state))
                 (caret (edit-state-caret state)))
             (make-edit-state :text (concatenate 'string (subseq text 0 (1- caret)) (subseq text caret))
                              :caret (1- caret)
                              :maximum-length (edit-state-maximum-length state))))))

(defun edit-delete (state)
  "Delete the character after the caret, or the whole selection.

STATE: the edit state."
  (cond ((edit-state-replace-on-type state)
         (make-edit-state :text "" :maximum-length (edit-state-maximum-length state)))
        ((>= (edit-state-caret state) (length (edit-state-text state))) state)
        (t (let ((text (edit-state-text state))
                 (caret (edit-state-caret state)))
             (make-edit-state :text (concatenate 'string (subseq text 0 caret) (subseq text (1+ caret)))
                              :caret caret
                              :maximum-length (edit-state-maximum-length state))))))

(defun edit-move (state
                  where)
  "Move the caret and clear any selection.

STATE: the edit state.
WHERE: :left, :right, :home or :end."
  (let ((length (length (edit-state-text state)))
        (caret (edit-state-caret state)))
    (make-edit-state :text (edit-state-text state)
                     :caret (ecase where
                              (:left (max 0 (1- caret)))
                              (:right (min length (1+ caret)))
                              (:home 0)
                              (:end length))
                     :maximum-length (edit-state-maximum-length state))))

(defun edit-select-all-replace (state)
  "Select the whole text so the next typed character replaces it.

STATE: the edit state."
  (make-edit-state :text (edit-state-text state)
                   :caret (length (edit-state-text state))
                   :maximum-length (edit-state-maximum-length state)
                   :replace-on-type t))

(defun edit-handle-key (state
                        key)
  "Apply an editing key to STATE; returns the new state, or NIL when KEY is not an editing key.

STATE: the edit state.
KEY: :backspace, :delete, :left, :right, :home or :end."
  (case key
    (:backspace (edit-backspace state))
    (:delete (edit-delete state))
    ((:left :right :home :end) (edit-move state key))
    (t nil)))
