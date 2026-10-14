(in-package #:borba-cad/tests)

(def-suite files
  :description "Saving, opening and naming files: what people paste, forget or get wrong."
  :in all)

(in-suite files)

;;; Paths as people write them

(defun cleaned (text)
  "Return the path that CLEAN-PATH-TEXT makes of TEXT."
  (persist:clean-path-text text))

(test blanks-and-one-pair-of-quotes-are-removed-from-a-pasted-path
  (is (string= "/a/b.bcad" (cleaned "  /a/b.bcad  ")))
  (is (string= "/a/b.bcad" (cleaned (format nil "\"/a/b.bcad\""))))
  (is (string= "/a/b c.bcad" (cleaned "'/a/b c.bcad'")))
  (is (string= "/a/b.bcad" (cleaned (format nil "~c ' /a/b.bcad ' ~c" #\Tab #\Newline))))
  (is (string= "\"/a/b.bcad'" (cleaned "\"/a/b.bcad'")) "mismatched quotes are left alone"))

(test a-file-address-becomes-the-path-it-names
  (is (string= "/home/ana/plan.bcad" (cleaned "file:///home/ana/plan.bcad")))
  (is (string= "/home/ana/my plan.bcad" (cleaned "file:///home/ana/my%20plan.bcad")))
  (is (string= "/home/ana/plan.bcad" (cleaned "file://localhost/home/ana/plan.bcad")))
  (is (string= "C:/Users/Ana/plan.bcad" (cleaned "FILE:///C:/Users/Ana/plan.bcad")))
  (is (string= (format nil "/x/~c~c.bcad" (code-char 231) (code-char 227))
               (cleaned "file:///x/%C3%A7%C3%A3.bcad")))
  (is (string= "/x/%E0%A4%A.bcad" (cleaned "file:///x/%E0%A4%A.bcad")) "bad escapes are kept"))

(test an-empty-path-is-refused-with-a-request-for-a-name
  (dolist (text (list "" "   " (format nil "~c" #\Tab) "\"\"" "''" "file://"))
    (signals base:invalid-input (persist:clean-path-text text))))

(test every-typed-path-becomes-absolute
  (dolist (text '("plan.bcad" "sub/plan.bcad" "./plan.bcad" "~/plan.bcad" "/abs/plan.bcad"))
    (is (uiop:absolute-pathname-p (persist:native-path text)) "~a" text))
  (is (string= "/abs/plan.bcad" (uiop:native-namestring (persist:native-path "  \"/abs/plan.bcad\" ")))))

(test a-name-without-an-extension-gets-the-drawing-extension
  (flet ((name (text)
           (file-namestring (persist:drawing-file-path text))))
    (is (string= "plan.bcad" (name "/x/plan")))
    (is (string= "plan.bcad" (name "/x/plan.bcad")))
    (is (string= "PLAN.BCAD" (name "/x/PLAN.BCAD")) "the case the user typed is kept")
    (is (string= "plan.txt.bcad" (name "/x/plan.txt")) "another extension is not ours")
    (is (string= "plan.bcad" (name "/x/plan.")) "a trailing dot is dropped")
    (is (string= ".hidden.bcad" (name "/x/.hidden")))
    (is (string= "v1.2 final.bcad" (name "/x/v1.2 final")) "a dot inside the name")))

(test a-folder-is-not-given-an-extension
  (is (null (pathname-name (persist:drawing-file-path "/x/folder/")))))

(test the-documents-folder-follows-the-desktop-configuration
  (with-temporary-directory (directory)
    (let ((documents (merge-pathnames "Documentos/" directory))
          (configuration (merge-pathnames "config/" directory))
          (previous (sb-posix:getenv "XDG_CONFIG_HOME")))
      (ensure-directories-exist documents)
      (ensure-directories-exist configuration)
      (with-open-file (out (merge-pathnames "user-dirs.dirs" configuration)
                           :direction :output
                           :if-exists :supersede)
        (format out "# written by a test~%XDG_DESKTOP_DIR=\"$HOME/Desktop\"~%XDG_DOCUMENTS_DIR=\"~a\"~%"
                (string-right-trim "/" (uiop:native-namestring documents))))
      (unwind-protect
           (progn
             (sb-posix:setenv "XDG_CONFIG_HOME" (uiop:native-namestring configuration) 1)
             (is (equal (truename documents) (truename (persist:default-drawings-directory)))))
        (if previous
            (sb-posix:setenv "XDG_CONFIG_HOME" previous 1)
            (sb-posix:unsetenv "XDG_CONFIG_HOME"))))))

(test the-default-folder-always-exists
  (is (eq :directory (sbcl:file-kind (persist:default-drawings-directory)))))

;;; Helpers

(defun file-test-app (directory)
  "Return an application with a small drawing, whose configuration lives in DIRECTORY."
  (app:make-app (base:default-config :project-root directory)
                :drawing (rng:generate-drawing :seed 3 :entity-count 5)))

(defun directory-listing (directory)
  "Return the sorted names of the files in DIRECTORY."
  (sort (mapcar #'file-namestring (uiop:directory-files directory))
        #'string<))

;;; Saving

(test saving-through-a-symbolic-link-updates-the-file-it-points-to
  (with-temporary-directory (directory)
    (let ((real (merge-pathnames "real.bcad" directory))
          (link (merge-pathnames "link.bcad" directory)))
      (persist:save-drawing (dom:make-drawing) real)
      (sb-posix:symlink (uiop:native-namestring real) (uiop:native-namestring link))
      (persist:save-drawing (golden-drawing) link)
      (is (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat (uiop:native-namestring link))))
          "the link is still a link")
      (is (string= *golden-text* (file-text real)) "the file it points to has the new drawing"))))

(test saving-over-a-file-keeps-its-permissions
  (with-temporary-directory (directory)
    (let ((path (merge-pathnames "private.bcad" directory)))
      (persist:save-drawing (dom:make-drawing) path)
      (sb-posix:chmod (uiop:native-namestring path) #o600)
      (persist:save-drawing (golden-drawing) path)
      (is (= #o600 (sbcl:file-permissions path))))))

(test saving-to-a-folder-or-into-a-missing-folder-says-so
  (with-temporary-directory (directory)
    (flet ((reason (path)
             (handler-case (progn (persist:save-drawing (dom:make-drawing) path) "no error")
               (base:drawing-file-io-error (condition) (princ-to-string condition)))))
      (is (search "is a folder" (reason directory)))
      (is (search "is a folder" (reason (merge-pathnames "plan.bcad/" directory))))
      (ensure-directories-exist (merge-pathnames "taken/" directory))
      (is (search "is a folder" (reason (merge-pathnames "taken" directory))))
      (is (search "does not exist" (reason (merge-pathnames "nope/plan.bcad" directory))))
      (is (null (probe-file (merge-pathnames "nope/" directory))) "saving never creates folders"))))

(test saving-over-a-read-only-file-says-so
  (unless (zerop (sb-posix:getuid))
    (with-temporary-directory (directory)
      (let ((path (merge-pathnames "locked.bcad" directory)))
        (persist:save-drawing (golden-drawing) path)
        (sb-posix:chmod (uiop:native-namestring path) #o444)
        (handler-case (progn (persist:save-drawing (dom:make-drawing) path) (fail "expected an error"))
          (base:drawing-file-io-error (condition)
            (is (search "read-only" (princ-to-string condition)))))
        (is (string= *golden-text* (file-text path)))))))

(test no-temporary-file-is-left-behind-by-any-failed-save
  (with-temporary-directory (directory)
    (let ((path (merge-pathnames "plan.bcad" directory)))
      (persist:save-drawing (golden-drawing) path)
      (signals base:drawing-file-io-error
        (persist:save-drawing (dom:make-drawing)
                              path
                              :before-commit (lambda (temporary)
                                               (declare (ignore temporary))
                                               (error "disk full"))))
      (is (equal '("plan.bcad") (directory-listing directory)))
      (is (string= *golden-text* (file-text path))))))

;;; Opening

(test opening-a-folder-or-a-device-says-so
  (with-temporary-directory (directory)
    (flet ((reason (path)
             (handler-case (progn (persist:load-drawing path) "no error")
               (base:drawing-file-io-error (condition) (princ-to-string condition)))))
      (is (search "is a folder" (reason directory)))
      (is (search "does not exist" (reason (merge-pathnames "nope.bcad" directory))))
      (is (search "not a regular file" (reason #p"/dev/null"))))))

;;; Through the application

(test save-as-adds-the-extension-and-remembers-the-file
  (with-temporary-directory (directory)
    (let ((application (file-test-app directory)))
      (is (app:save-app-drawing application (uiop:native-namestring (merge-pathnames "my plan" directory))))
      (is (equal '("my plan.bcad") (directory-listing directory)))
      (is (string= "my plan.bcad" (file-namestring (app:app-file-path application))))
      (is (search "Saved my plan.bcad" (message-text application)))
      (is (not (app:app-dirty-p application))))))

(test save-as-understands-quotes-blanks-and-file-addresses
  (with-temporary-directory (directory)
    (let ((application (file-test-app directory))
          (base (uiop:native-namestring directory)))
      ;; Relative leftovers of a misread path would land in the current folder, so make it this one.
      (uiop:with-current-directory (directory)
        (dolist (text (list (format nil "\"~aquoted.bcad\"" base)
                            (format nil "  ~aspaced.bcad  " base)
                            (format nil "file://~afrom-uri" base)))
          (is (app:save-app-drawing application text) "~s" text)))
      (is (equal '("from-uri.bcad" "quoted.bcad" "spaced.bcad") (directory-listing directory)))
      (is (null (uiop:subdirectories directory)) "no stray folder was created"))))

(test saving-to-a-folder-or-a-blank-name-shows-a-clear-message-and-changes-nothing
  (with-temporary-directory (directory)
    (let ((application (file-test-app directory)))
      (dolist (text (list (uiop:native-namestring directory) "" "   "))
        (is (not (app:save-app-drawing application text)) "~s" text)
        (is (eq :error (cdr (app:app-message application))))
        (is (null (app:app-file-path application))))
      (is (search "folder" (message-text (progn (app:save-app-drawing application (uiop:native-namestring directory))
                                                application))))
      (is (null (directory-listing directory))))))

(defun prompt-text (application)
  "Return the text in the field of the open prompt."
  (ui:edit-state-text (app:prompt-edit (app:app-prompt application))))

(defun choose (application label)
  "Run the function behind the button LABEL of the open choice prompt."
  (let ((choice (assoc label (app:prompt-choices (app:app-prompt application)) :test #'string=)))
    (unless choice (error "No button ~s" label))
    (app:close-prompt application)
    (when (cdr choice)
      (funcall (cdr choice)))))

(test save-as-asks-before-replacing-a-different-existing-file
  (with-temporary-directory (directory)
    (let* ((application (file-test-app directory))
           (existing (merge-pathnames "other.bcad" directory)))
      (persist:save-drawing (dom:make-drawing) existing)
      (let ((original (file-text existing)))
        (app:operation-save-as application)
        (type-text application (uiop:native-namestring existing))
        (press application :enter)
        (is (eq :choice (app:prompt-kind (app:app-prompt application))) "a question is asked")
        (is (search "other.bcad" (app:prompt-message (app:app-prompt application))))
        (choose application "Cancel")
        (is (string= original (file-text existing)) "cancel leaves the file alone")
        (is (null (app:app-file-path application)))
        (app:operation-save-as application)
        (type-text application (uiop:native-namestring existing))
        (press application :enter)
        (choose application "Replace")
        (is (string/= original (file-text existing)) "replace saves")
        (is (string= "other.bcad" (file-namestring (app:app-file-path application))))))))

(test save-as-onto-the-file-being-edited-does-not-ask
  (with-temporary-directory (directory)
    (let ((application (file-test-app directory))
          (path (merge-pathnames "mine.bcad" directory)))
      (app:save-app-drawing application (uiop:native-namestring path))
      (app:operation-save-as application)
      (is (search "mine.bcad" (prompt-text application)) "the prompt offers the current file")
      (press application :enter)
      (is (null (app:app-prompt application)) "no question: it is the same file"))))

(test the-save-prompt-starts-in-a-real-folder-with-a-new-name
  (with-temporary-directory (directory)
    (let ((application (file-test-app directory)))
      (app:operation-save-as application)
      (let ((suggestion (prompt-text application)))
        (is (uiop:absolute-pathname-p (uiop:parse-native-namestring suggestion)))
        (is (string= "untitled.bcad" (file-namestring (uiop:parse-native-namestring suggestion))))
        (is (eq :directory (sbcl:file-kind (persist:directory-of (uiop:parse-native-namestring suggestion)))))))))

(test opening-finds-a-drawing-whose-extension-was-left-out
  (with-temporary-directory (directory)
    (let ((application (file-test-app directory)))
      (app:save-app-drawing application (uiop:native-namestring (merge-pathnames "plan" directory)))
      (let ((other (file-test-app directory)))
        (is (app:load-drawing-into-app other (uiop:native-namestring (merge-pathnames "plan" directory))))
        (is (= 5 (dom:drawing-entity-count (app:app-drawing other))))
        (is (search "Opened plan.bcad: 5 entities" (message-text other)))))))

(test opening-a-folder-a-missing-file-or-a-blank-name-keeps-the-drawing-and-explains
  (with-temporary-directory (directory)
    (let ((application (file-test-app directory)))
      (dolist (case (list (cons (uiop:native-namestring directory) "folder")
                          (cons (uiop:native-namestring (merge-pathnames "nope.bcad" directory)) "does not exist")
                          (cons "" "name of a file")
                          (cons "   " "name of a file")))
        (is (not (app:load-drawing-into-app application (car case))) "~s" (car case))
        (is (search (cdr case) (message-text application)) "~s -> ~a" (car case) (message-text application))
        (is (= 5 (dom:drawing-entity-count (app:app-drawing application))) "the drawing is untouched")))))

(test a-drawing-with-accents-in-its-file-name-round-trips
  (with-temporary-directory (directory)
    (let* ((name (format nil "Planta T~crrea ~c~c~c.bcad"
                         (code-char 233)
                         (code-char 231)
                         (code-char 227)
                         (code-char 111)))
           (application (file-test-app directory)))
      (is (app:save-app-drawing application (uiop:native-namestring (merge-pathnames name directory))))
      (let ((other (file-test-app directory)))
        (is (app:load-drawing-into-app other (uiop:native-namestring (merge-pathnames name directory))))
        (is (= 5 (dom:drawing-entity-count (app:app-drawing other))))))))

(test a-drawing-saved-with-windows-line-endings-opens
  (with-temporary-directory (directory)
    (let* ((path (merge-pathnames "crlf.bcad" directory))
           (text (progn (persist:save-drawing (golden-drawing) path) (file-text path))))
      (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
        (loop for character across text
              do (when (char= character #\Newline)
                   (write-char #\Return out))
                 (write-char character out)))
      (is (= 6 (dom:drawing-entity-count (persist:load-drawing path)))))))

(test byte-sizes-read-naturally
  (is (string= "0 B" (base:format-byte-size 0)))
  (is (string= "1023 B" (base:format-byte-size 1023)))
  (is (string= "1.0 KB" (base:format-byte-size 1024)))
  (is (string= "1.5 KB" (base:format-byte-size 1536)))
  (is (string= "4.8 MB" (base:format-byte-size 5000000))))
