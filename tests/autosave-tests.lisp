(in-package #:borba-cad/tests)

(def-suite autosave
  :description "The copy of the work that is not saved, and taking it back after a crash."
  :in all)

(in-suite autosave)

;;; Helpers

(defun autosave-app (directory
                     &key (state t)
                          file)
  "Return an application whose state directory is a folder of DIRECTORY, or has none when STATE is NIL.

DIRECTORY: a folder made for the test.
STATE: true to give it a state directory.
FILE: the file the drawing belongs to, or NIL."
  (let ((config (quiet-config)))
    (setf (base:config-state-directory config)
          (and state (merge-pathnames "state/" directory)))
    (let ((application (app:make-app config :file-path file)))
      (set-test-view application)
      application)))

(defun draw-a-line (application
                    &optional (id 1))
  "Add a line to the drawing of APPLICATION, which makes it a drawing with unsaved changes."
  (app:commit-changes application
                      (dom:create-entities-changes
                       (list (dom:make-line :id id :layer-id 0 :x1 0d0 :y1 0d0 :x2 (* 100d0 id) :y2 50d0)))
                      "entity.create"))

(defun autosave-files (directory)
  "Return the names of the files of the autosave in the state folder of DIRECTORY, sorted."
  (sort (remove-if-not (lambda (name) (search "autosave" name))
                       (mapcar #'file-namestring (directory (merge-pathnames "state/*.*" directory))))
        #'string<))

(defconstant +interval+ app:+autosave-interval-nanoseconds+
  "A minute, as the autosave counts it.")

;;; Writing the copy

(test a-drawing-with-unsaved-changes-gets-a-copy-after-a-minute
  (with-temporary-directory (directory)
    (let ((application (autosave-app directory)))
      (draw-a-line application)
      (app:autosave-tick application 0)
      (is (null (autosave-files directory)) "the first look only starts the count")
      (app:autosave-tick application (floor +interval+ 2))
      (is (null (autosave-files directory)) "half a minute is not enough")
      (app:autosave-tick application +interval+)
      (is (equal '("autosave-origin.txt" "autosave.bcad") (autosave-files directory)))
      (let ((copy (persist:load-drawing (merge-pathnames "state/autosave.bcad" directory))))
        (is (= 1 (dom:drawing-entity-count copy)))
        (is (equal (entity-values (app:app-drawing application)) (entity-values copy)))))))

(test a-clean-drawing-gets-no-copy
  (with-temporary-directory (directory)
    (let ((application (autosave-app directory)))
      (app:autosave-tick application 0)
      (app:autosave-tick application (* 2 +interval+))
      (app:autosave-tick application (* 4 +interval+))
      (is (null (autosave-files directory))))))

(test a-copy-is-written-again-only-when-the-drawing-changed
  (with-temporary-directory (directory)
    (let ((application (autosave-app directory)))
      (draw-a-line application)
      (app:autosave-tick application 0)
      (app:autosave-tick application +interval+)
      (delete-file (merge-pathnames "state/autosave.bcad" directory))
      (app:autosave-tick application (* 2 +interval+))
      (is (not (probe-file (merge-pathnames "state/autosave.bcad" directory))) "nothing changed, nothing written")
      (draw-a-line application 2)
      (app:autosave-tick application (* 3 +interval+))
      (is-true (probe-file (merge-pathnames "state/autosave.bcad" directory)))
      (is (= 2 (dom:drawing-entity-count
                (persist:load-drawing (merge-pathnames "state/autosave.bcad" directory))))))))

(test the-copy-remembers-which-file-the-drawing-belonged-to
  (with-temporary-directory (directory)
    (let* ((path (merge-pathnames "plan.bcad" directory))
           (application (autosave-app directory :file path)))
      (draw-a-line application)
      (app:autosave-tick application 0)
      (app:autosave-tick application +interval+)
      (is (string= (uiop:native-namestring path)
                   (string-trim '(#\Newline)
                                (fixture-like-text (merge-pathnames "state/autosave-origin.txt" directory)))))
      (is (equal path (app::autosave-origin application))))
    (let ((application (autosave-app directory)))
      (draw-a-line application)
      (app:autosave-tick application 0)
      (app:autosave-tick application +interval+)
      (is (null (app::autosave-origin application)) "a drawing with no file has no origin"))))

(test the-copy-goes-away-when-the-drawing-is-saved
  (with-temporary-directory (directory)
    (let ((application (autosave-app directory)))
      (draw-a-line application)
      (app:autosave-tick application 0)
      (app:autosave-tick application +interval+)
      (is (autosave-files directory))
      (is (app:save-app-drawing application (uiop:native-namestring (merge-pathnames "saved.bcad" directory))))
      (app:autosave-tick application (* 2 +interval+))
      (is (null (autosave-files directory)) "nothing left to protect"))))

(test the-copy-goes-away-when-the-drawing-is-given-up
  (with-temporary-directory (directory)
    (let ((application (autosave-app directory)))
      (draw-a-line application)
      (app:autosave-tick application 0)
      (app:autosave-tick application +interval+)
      (app:discard-autosave application)
      (is (null (autosave-files directory)))
      (app:discard-autosave application)
      (is (null (autosave-files directory)) "and doing it twice is no trouble"))))

(test a-program-with-no-state-directory-does-nothing
  (with-temporary-directory (directory)
    (let ((application (autosave-app directory :state nil)))
      (draw-a-line application)
      (app:autosave-tick application 0)
      (app:autosave-tick application +interval+)
      (is (not (app:autosave-exists-p application)))
      (is (null (autosave-files directory))))))

(test a-copy-that-cannot-be-written-never-reaches-the-user
  (with-temporary-directory (directory)
    (touch-text (merge-pathnames "state" directory) "a file where the folder should be")
    (let ((application (autosave-app directory)))
      (draw-a-line application)
      (app:autosave-tick application 0)
      (finishes (app:autosave-tick application +interval+))
      (is (null (app:app-message application)) "and the drawing is as it was"))))

;;; Taking the work back

(defun crashed-session (directory
                        &key file)
  "Leave a copy of a drawing with one line in the state folder, as a session that crashed would, and return it."
  (let ((application (autosave-app directory :file file)))
    (draw-a-line application)
    (app:autosave-tick application 0)
    (app:autosave-tick application +interval+)
    application))

(test the-next-start-offers-the-work-and-recover-brings-it-back
  (with-temporary-directory (directory)
    (let ((original (crashed-session directory)))
      (let ((application (autosave-app directory)))
        (is (app:autosave-exists-p application))
        (app:offer-autosave-recovery application)
        (is (string= "Recover unsaved work?" (prompt-title-of application)))
        (is (search "a new drawing" (app:prompt-message (app:app-prompt application))))
        (is (equal '("Recover" "Discard" "Not now") (prompt-labels application)))
        (choose-in-prompt application 0)
        (is (equal (entity-values (app:app-drawing original)) (entity-values (app:app-drawing application))))
        (is (app:app-dirty-p application) "it is work that is not saved")
        (is (null (app:app-file-path application)))
        (is (search "back" (message-text application)))
        (is (autosave-files directory) "the copy stays until the drawing is saved")))))

(test a-recovered-drawing-belongs-to-its-file-again
  (with-temporary-directory (directory)
    (let ((path (merge-pathnames "plan.bcad" directory)))
      (persist:save-drawing (dom:make-drawing) path)
      (crashed-session directory :file path)
      (let ((application (autosave-app directory)))
        (app:offer-autosave-recovery application)
        (is (search "plan.bcad" (app:prompt-message (app:app-prompt application))))
        (choose-in-prompt application 0)
        (is (equal path (app:app-file-path application)))
        (is (= 1 (dom:drawing-entity-count (app:app-drawing application))) "the line the file does not have")))))

(test discard-throws-the-copy-away-and-not-now-keeps-it
  (with-temporary-directory (directory)
    (crashed-session directory)
    (let ((application (autosave-app directory)))
      (app:offer-autosave-recovery application)
      (press application :escape)
      (is (app:autosave-exists-p application) "Escape is Not now")
      (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
      (app:offer-autosave-recovery application)
      (choose-in-prompt application 1)
      (is (not (app:autosave-exists-p application)))
      (is (null (autosave-files directory))))))

(test with-nothing-to-recover-nothing-is-asked
  (with-temporary-directory (directory)
    (let ((application (autosave-app directory)))
      (app:offer-autosave-recovery application)
      (is (null (app:app-prompt application))))))

(test a-copy-that-cannot-be-read-is-said-and-kept
  (with-temporary-directory (directory)
    (touch-text (merge-pathnames "state/autosave.bcad" directory) "this is not a drawing")
    (let ((application (autosave-app directory)))
      (app:offer-autosave-recovery application)
      (choose-in-prompt application 0)
      (is (eq :error (cdr (app:app-message application))))
      (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
      (is (app:autosave-exists-p application) "so that it can be looked at"))))

(test the-question-speaks-the-language-of-the-interface
  (with-temporary-directory (directory)
    (crashed-session directory)
    (dolist (language '(:pt-br :ru-ru))
      (i18n:with-language (language)
        (let ((application (autosave-app directory)))
          (app:offer-autosave-recovery application)
          (is (string/= "Recover unsaved work?" (prompt-title-of application)) "~a" language)
          (is (null (untranslated-texts (mapcar #'car (app:prompt-choices (app:app-prompt application))) language)))
          (app:close-prompt application))))))
