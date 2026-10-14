(in-package #:borba-cad/tests)

(def-suite clipboard
  :description "Copying, cutting, pasting and selecting all in the text fields."
  :in all)

(in-suite clipboard)

(defun clipboard-app ()
  "Return an application whose clipboard is the one it keeps itself."
  (let ((application (make-test-app)))
    (app:set-clipboard-text application nil)
    application))

(defun field-text (application)
  "Return the text of the focused field of APPLICATION, whether it is a prompt's or an inspector's."
  (ui:edit-state-text (if (app:app-prompt application)
                          (app:prompt-edit (app:app-prompt application))
                          (app:app-edit application))))

;;; Lines

(test text-pasted-into-one-line-loses-its-line-breaks-and-tabs
  (is (string= "one two" (app:single-line (format nil "one~%two"))))
  (is (string= "a b" (app:single-line (format nil "a~c~c~%~cb" #\Return #\Newline #\Tab))) "a run becomes one space")
  (is (string= "kept" (app:single-line (format nil "~%~%kept~%"))) "the ends are trimmed")
  (is (string= "" (app:single-line "")))
  (is (string= "a  b" (app:single-line "a  b")) "spaces inside are left alone"))

;;; The clipboard itself

(test the-application-keeps-the-clipboard-text-when-there-is-no-system-clipboard
  (let ((application (clipboard-app)))
    (is (null (app:clipboard-text application)))
    (app:set-clipboard-text application "hello")
    (is (string= "hello" (app:clipboard-text application)))))

(test the-application-uses-the-functions-it-is-given-for-the-system-clipboard
  (let ((application (clipboard-app))
        (stored nil))
    (setf (app:app-clipboard-reader application) (lambda () stored)
          (app:app-clipboard-writer application) (lambda (text) (setf stored (format nil "[~a]" text))))
    (app:set-clipboard-text application "x")
    (is (string= "[x]" (app:clipboard-text application)))))

;;; Editing keys with the clipboard

(defun a-state (text &optional (caret (length text)))
  "Return an edit state holding TEXT with the caret at CARET."
  (ui:make-edit-state :text text :caret caret))

(test control-c-copies-the-text-and-leaves-the-field-alone
  (let* ((application (clipboard-app))
         (state (a-state "1250.5"))
         (after (app:edit-with-clipboard application state :c '(:control))))
    (is (string= "1250.5" (app:clipboard-text application)))
    (is (eq state after))))

(test control-x-copies-the-text-and-empties-the-field
  (let* ((application (clipboard-app))
         (after (app:edit-with-clipboard application (a-state "abc") :x '(:control))))
    (is (string= "abc" (app:clipboard-text application)))
    (is (string= "" (ui:edit-state-text after)))))

(test control-v-inserts-at-the-caret-and-over-a-selection
  (let ((application (clipboard-app)))
    (app:set-clipboard-text application "XY")
    (is (string= "abXYc" (ui:edit-state-text (app:edit-with-clipboard application (a-state "abc" 2) :v '(:control)))))
    (is (= 4 (ui:edit-state-caret (app:edit-with-clipboard application (a-state "abc" 2) :v '(:control)))))
    (is (string= "XY" (ui:edit-state-text (app:edit-with-clipboard application
                                                                   (ui:edit-select-all-replace (a-state "abc"))
                                                                   :v
                                                                   '(:control))))
        "a selected text is replaced")))

(defun pasted-into-empty-field (application)
  "Return what Control+V makes of an empty field when the clipboard holds what it holds."
  (ui:edit-state-text (app:edit-with-clipboard application (a-state "") :v '(:control))))

(test pasting-nothing-does-nothing-and-pasting-lines-makes-one
  (let ((application (clipboard-app)))
    (is (null (app:edit-with-clipboard application (a-state "abc") :v '(:control))) "an empty clipboard")
    (app:set-clipboard-text application (format nil "/home/me/obras~%"))
    (is (string= "/home/me/obras" (pasted-into-empty-field application)))
    (app:set-clipboard-text application (format nil "first~%second~%third"))
    (is (string= "first second third" (pasted-into-empty-field application)))))

(test a-pasted-text-respects-the-field-length-limit
  (let ((application (clipboard-app))
        (state (ui:make-edit-state :text "ab" :maximum-length 5)))
    (app:set-clipboard-text application "123456789")
    (is (string= "ab123" (ui:edit-state-text (app:edit-with-clipboard application state :v '(:control)))))))

(test control-a-selects-everything-so-typing-replaces-it
  (let* ((application (clipboard-app))
         (selected (app:edit-with-clipboard application (a-state "old") :a '(:control))))
    (is (ui:edit-state-replace-on-type selected))
    (is (string= "new" (ui:edit-state-text (ui:edit-insert selected "new"))))))

(test other-control-keys-do-nothing-to-a-field-and-plain-keys-still-edit
  (let ((application (clipboard-app)))
    (is (null (app:edit-with-clipboard application (a-state "abc") :z '(:control))))
    (is (null (app:edit-with-clipboard application (a-state "abc") :s '(:control))))
    (is (string= "ab" (ui:edit-state-text (app:edit-with-clipboard application (a-state "abc") :backspace nil))))
    (is (string= "abc" (ui:edit-state-text (app:edit-with-clipboard application (a-state "abc" 1) :end nil))))))

;;; Through the application's fields

(test the-prompt-field-takes-the-clipboard-keys
  (let ((application (clipboard-app)))
    (app:open-text-prompt application "Name" "Type:" "old" #'identity)
    (press application :c :control)                              ; copies "old"
    (is (string= "old" (app:clipboard-text application)))
    (press application :x :control)
    (is (string= "" (field-text application)))
    (press application :v :control)
    (is (string= "old" (field-text application)))
    (type-text application "er")
    (is (string= "older" (field-text application)) "the paste left the caret after the text")
    (press application :a :control)
    (type-text application "z")
    (is (string= "z" (field-text application)))))

(test an-inspector-field-takes-a-pasted-number
  (let ((application (make-test-app)))
    (press application :r)
    (click application 0 0)
    (click application 1000 500)
    (press application :escape)
    (click application 500 0)                                    ; selects the rectangle by its edge
    (let ((width-field (find-widget application '(:property :width))))
      (is-true width-field)
      (click-widget application '(:property :width))
      (app:set-clipboard-text application "2.500,5")
      (press application :v :control)
      (press application :enter)
      (is (= 2500.5d0 (dom:rectangle-width (first (app:selected-entities application))))))))

(test the-file-dialog-fields-take-the-clipboard-keys
  (with-temporary-directory (directory)
    (touch-drawing (merge-pathnames "plan-a.bcad" directory))
    (touch-drawing (merge-pathnames "other.bcad" directory))
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (app:set-clipboard-text application "PLAN")
      (press application :v :control)                            ; into the search
      (is (equal '("plan-a.bcad") (visible-names application)) "pasting a search filters at once")
      (press application :enter)
      (is (null (app:app-dialog application)))
      (is (string= "plan-a.bcad" (file-namestring (app:app-file-path application))))
      (app:operation-save-as application)
      (app:set-clipboard-text application (uiop:native-namestring (merge-pathnames "pasted.bcad" directory)))
      (press application :v :control)                            ; a whole path into the name
      (press application :enter)
      (is (probe-file (merge-pathnames "pasted.bcad" directory)))
      (app:operation-open application)
      (press application :l :control)
      (app:set-clipboard-text application (uiop:native-namestring directory))
      (press application :v :control)
      (press application :enter)
      (is (null (find-widget application :dialog-location)) "the pasted folder was entered"))))
