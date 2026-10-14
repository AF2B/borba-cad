(in-package #:borba-cad/tests)

(def-suite file-dialog
  :description "The file dialog: listing, filtering, navigation, saving, opening and recent files."
  :in all)

(in-suite file-dialog)

;;; Helpers

(defun touch-drawing (path)
  "Write a small drawing to PATH, creating its folder."
  (ensure-directories-exist path)
  (persist:save-drawing (rng:generate-drawing :seed 2 :entity-count 3) path))

(defun touch-text (path text)
  "Write TEXT to PATH, creating its folder."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string text out)))

(defun make-tree (directory)
  "Fill DIRECTORY with folders Alpha, beta and .hidden, and files notes.txt, Plan-a.bcad,
plan-b.bcad, zeta.bcad and .secret.bcad, and a drawing inside beta."
  (dolist (name '("Alpha/" "beta/" ".hidden/"))
    (ensure-directories-exist (merge-pathnames name directory)))
  (dolist (name '("Plan-a.bcad" "plan-b.bcad" "zeta.bcad" ".secret.bcad" "beta/inner.bcad"))
    (touch-drawing (merge-pathnames name directory)))
  (touch-text (merge-pathnames "notes.txt" directory) "remember the milk")
  directory)

(defun entry-names (entries)
  "Return the names of ENTRIES."
  (mapcar #'app:file-entry-name entries))

(defun dialog-of (application)
  "Return the open file dialog of APPLICATION."
  (app:app-dialog application))

(defun visible-names (application)
  "Return the names of the entries the open dialog lists."
  (entry-names (app:visible-entries (dialog-of application))))

(defun dialog-app (directory &key (state nil) (width 1440d0) (height 900d0))
  "Return an application whose file is in DIRECTORY, so that dialogs start there. When STATE is
true, recent files are remembered in a folder inside DIRECTORY."
  (let ((config (quiet-config)))
    (when state
      (setf (base:config-state-directory config) (merge-pathnames "state/" directory)))
    (let ((application (app:make-app config
                                     :width width
                                     :height height
                                     :drawing (rng:generate-drawing :seed 3 :entity-count 4))))
      (setf (app:app-file-path application) (merge-pathnames "current.bcad" directory))
      (set-test-view application)
      application)))

(defun type-in-dialog (application text)
  "Type TEXT into the field of the open dialog that has the keyboard."
  (type-text application text))

(defun accept-button (application)
  "Return the Open or Save button of the open dialog."
  (find-widget application :dialog-accept))

(defun dialog-message-text (application)
  "Return the sentence under the dialog's list, or NIL."
  (app:file-dialog-message (dialog-of application)))

(defun prompt-button-p (widget)
  "Return true when WIDGET is one of the buttons of the modal prompt."
  (and (consp (ui:widget-id widget))
       (eq :prompt-button (first (ui:widget-id widget)))))

(defun buttons-behind-the-prompt (application)
  "Return every button of APPLICATION except the buttons of the prompt that is open."
  (remove-if (lambda (widget)
               (or (not (eq :button (ui:widget-kind widget)))
                   (prompt-button-p widget)))
             (app:build-widgets application)))

(defun folder-name (directory)
  "Return the last name of a folder pathname."
  (car (last (pathname-directory directory))))

;;; Listing

(test a-listing-puts-folders-first-sorted-without-regard-to-case-and-leaves-out-hidden-names
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((entries (app:list-entries directory)))
      (is (equal '("Alpha" "beta" "notes.txt" "Plan-a.bcad" "plan-b.bcad" "zeta.bcad")
                 (entry-names entries)))
      (is (equal '(:directory :directory :file :file :file :file)
                 (mapcar #'app:file-entry-kind entries)))
      (let ((notes (find "notes.txt" entries :key #'app:file-entry-name :test #'string=)))
        (is (= 17 (app:file-entry-size notes)))
        (is (string= "notes.txt" (file-namestring (app:file-entry-path notes))))
        (is (typep (app:file-entry-modified notes) 'integer)))
      (let ((folder (first entries)))
        (is (null (app:file-entry-size folder)))
        (is (null (pathname-name (app:file-entry-path folder))) "a folder's path is a directory path")
        (is (eq :directory (sbcl:file-kind (app:file-entry-path folder))))))))

(test an-empty-or-missing-folder-lists-nothing-and-a-missing-one-says-why
  (with-temporary-directory (directory)
    (is (null (app:list-entries directory)))
    (multiple-value-bind (entries problem) (app:list-entries (merge-pathnames "nowhere/" directory))
      (is (null entries))
      (is (search "cannot be read" problem))
      (is (search "No such file" problem)))))

(test names-with-odd-characters-are-listed-literally
  (with-temporary-directory (directory)
    (let ((accented (format nil "acc-ent-~c.bcad" (code-char 233))))
      (dolist (name (list "a b.bcad" "star*.bcad" "bracket[1].bcad" "quote'n.bcad" accented))
        (touch-drawing (merge-pathnames (make-pathname :name name) directory)))
      (is (equal (list "a b.bcad" accented "bracket[1].bcad" "quote'n.bcad" "star*.bcad")
                 (entry-names (app:list-entries directory)))))))

(test a-symbolic-link-shows-what-it-points-to-and-a-broken-one-is-left-out
  (with-temporary-directory (directory)
    (touch-drawing (merge-pathnames "real.bcad" directory))
    (ensure-directories-exist (merge-pathnames "folder/" directory))
    (sb-posix:symlink "real.bcad" (uiop:native-namestring (merge-pathnames "link.bcad" directory)))
    (sb-posix:symlink "folder" (uiop:native-namestring (merge-pathnames "folder-link" directory)))
    (sb-posix:symlink "nowhere" (uiop:native-namestring (merge-pathnames "broken.bcad" directory)))
    (let ((entries (app:list-entries directory)))
      (is (equal '("folder" "folder-link" "link.bcad" "real.bcad") (entry-names entries)))
      (is (equal '(:directory :directory :file :file) (mapcar #'app:file-entry-kind entries))))))

(test an-unreadable-folder-is-reported-and-the-dialog-stays-where-it-was
  (unless (zerop (sb-posix:getuid))                         ; root reads everything
    (with-temporary-directory (directory)
      (make-tree directory)
      (let* ((locked (merge-pathnames "locked/" directory))
             (application (dialog-app directory)))
        (ensure-directories-exist locked)
        (sb-posix:chmod (uiop:native-namestring locked) 0)
        (unwind-protect
             (progn
               (app:operation-open application)
               (let ((dialog (dialog-of application)))
                 (is (not (app:open-directory dialog locked)))
                 (is (equal (truename directory) (truename (app:file-dialog-directory dialog))))
                 (is (eq :error (app:file-dialog-message-severity dialog)))
                 (is (search "Permission denied" (app:file-dialog-message dialog)))))
          (sb-posix:chmod (uiop:native-namestring locked) #o755))))))

;;; Filtering

(test only-drawings-are-listed-unless-every-file-is-asked-for
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (is (equal '("Alpha" "beta" "Plan-a.bcad" "plan-b.bcad" "zeta.bcad") (visible-names application)))
      (click-widget application :dialog-all-files)
      (is (app:file-dialog-show-all (dialog-of application)))
      (is (member "notes.txt" (visible-names application) :test #'string=))
      (click-widget application :dialog-all-files)
      (is (not (member "notes.txt" (visible-names application) :test #'string=))))))

(test the-search-text-keeps-what-contains-it-ignoring-case
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (type-in-dialog application "PLAN")
      (is (equal '("Plan-a.bcad" "plan-b.bcad") (visible-names application)))
      (is (string= "Plan-a.bcad" (app:file-dialog-selected (dialog-of application)))
          "the first match is selected so that Enter opens it")
      (press application :backspace)
      (press application :backspace)
      (press application :backspace)
      (press application :backspace)
      (is (equal '("Alpha" "beta" "Plan-a.bcad" "plan-b.bcad" "zeta.bcad") (visible-names application)))
      (type-in-dialog application "alp")
      (is (equal '("Alpha") (visible-names application)) "folders are searched too")
      (type-in-dialog application "xyz")
      (is (null (visible-names application)))
      (app:app-render application)
      (is (some (lambda (text) (search "Nothing here matches" text)) (all-text application))))))

;;; Navigation

(test the-folder-trail-runs-from-the-root-to-the-folder
  (let ((trail (app:breadcrumbs #p"/usr/share/fonts/")))
    (is (equal '("/" "usr" "share" "fonts") (mapcar #'car trail)))
    (is (equal '(#p"/" #p"/usr/" #p"/usr/share/" #p"/usr/share/fonts/") (mapcar #'cdr trail))))
  (is (equal '("/") (mapcar #'car (app:breadcrumbs #p"/")))))

(test going-up-selects-the-folder-that-was-left
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (let ((dialog (dialog-of application)))
        (click-widget application '(:entry "beta"))
        (click-widget application '(:entry "beta"))
        (is (string= "beta" (folder-name (app:file-dialog-directory dialog))) "a double click enters the folder")
        (is (equal '("inner.bcad") (visible-names application)))
        (press application :up :alt)
        (is (equal (truename directory) (truename (app:file-dialog-directory dialog))))
        (is (string= "beta" (app:file-dialog-selected dialog)) "the folder just left is selected")))))

(test backspace-in-an-empty-search-goes-up-and-the-up-button-does-too
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (press application :backspace)
      (is (equal (truename (persist:directory-of (string-right-trim "/" (uiop:native-namestring directory))))
                 (truename (app:file-dialog-directory (dialog-of application)))))
      (click-widget application '(:crumb 0))
      (is (string= "/" (uiop:native-namestring (app:file-dialog-directory (dialog-of application)))))
      (is (not (ui:widget-enabled (find-widget application :dialog-up))) "there is nothing above the root"))))

(test a-click-on-a-step-of-the-trail-or-on-a-place-changes-folder
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (click-widget application '(:place 0))
      (is (equal (truename (user-homedir-pathname))
                 (truename (app:file-dialog-directory (dialog-of application)))))
      (is (ui:widget-active (find-widget application '(:place 0))) "the current place is marked"))))

(test the-typed-path-is-relative-to-the-folder-shown-unless-it-is-absolute
  (with-temporary-directory (directory)
    (let* ((application (dialog-app directory))
           (dialog (progn (app:operation-open application) (dialog-of application))))
      (is (equal (merge-pathnames "plan.bcad" directory) (app:typed-path dialog "plan.bcad")))
      (is (equal (merge-pathnames "sub/plan.bcad" directory) (app:typed-path dialog "sub/plan.bcad")))
      (is (equal #p"/etc/hosts" (app:typed-path dialog "/etc/hosts")))
      (is (equal #p"/etc/hosts" (app:typed-path dialog "\"/etc/hosts\"")))
      (is (equal #p"/etc/hosts" (app:typed-path dialog "file:///etc/hosts")))
      (is (equal (merge-pathnames "x.bcad" (user-homedir-pathname)) (app:typed-path dialog "~/x.bcad")))
      (signals base:invalid-input (app:typed-path dialog "   ")))))

;;; Selection and scrolling

(test the-selection-moves-with-the-arrow-keys-and-the-list-follows-it
  (with-temporary-directory (directory)
    (dotimes (index 12)
      (touch-drawing (merge-pathnames (format nil "drawing-~2,'0d.bcad" index) directory)))
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (app:app-render application)                              ; the first frame learns how many rows fit
      (let* ((dialog (dialog-of application))
             (capacity (app:file-dialog-capacity dialog)))
        (is (< 3 capacity 12) "a 12-entry folder does not fit the dialog")
        (is (null (app:file-dialog-selected dialog)))
        (press application :down)
        (is (string= "drawing-00.bcad" (app:file-dialog-selected dialog)))
        (dotimes (index (1- capacity)) (press application :down))
        (is (= 0 (app:file-dialog-scroll dialog)) "still in view")
        (press application :down)
        (is (= 1 (app:file-dialog-scroll dialog)) "one more and the list scrolls by one row")
        (press application :page-down)
        (press application :page-down)
        (is (string= "drawing-11.bcad" (app:file-dialog-selected dialog)) "it stops at the last entry")
        (is (= (- 12 capacity) (app:file-dialog-scroll dialog)))
        (press application :page-up)
        (press application :home)                              ; Home goes to the text field, not the list
        (press application :up)
        (is (string/= "drawing-11.bcad" (app:file-dialog-selected dialog)))))))

(test the-wheel-scrolls-the-list-and-an-indicator-shows-where-it-is
  (with-temporary-directory (directory)
    (dotimes (index 30)
      (touch-drawing (merge-pathnames (format nil "d~2,'0d.bcad" index) directory)))
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (app:app-render application)
      (is (find-widget application :dialog-scrollbar))
      (let ((first-row (ui:widget-id (find-if (lambda (widget)
                                                (and (consp (ui:widget-id widget))
                                                     (eq :entry (first (ui:widget-id widget)))))
                                              (app:build-widgets application)))))
        (send application (app:make-wheel-event -3d0 700d0 400d0))
        (is (= 3 (app:file-dialog-scroll (dialog-of application))))
        (is (not (equal first-row (ui:widget-id (find-if (lambda (widget)
                                                           (and (consp (ui:widget-id widget))
                                                                (eq :entry (first (ui:widget-id widget)))))
                                                         (app:build-widgets application))))))
        (send application (app:make-wheel-event 10d0 700d0 400d0))
        (is (= 0 (app:file-dialog-scroll (dialog-of application))) "it stops at the top"))
      (is (zerop (app:file-dialog-scroll (dialog-of application)))))))

(test choosing-a-file-puts-its-name-in-the-name-field
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-save-as application)
      (is (string= "current.bcad" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application)))))
      (click-widget application '(:entry "zeta.bcad"))
      (is (string= "zeta.bcad" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application)))))
      (click-widget application '(:entry "Alpha"))
      (is (string= "zeta.bcad" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application))))
          "selecting a folder leaves the name alone"))))

;;; Creating folders

(test a-new-folder-is-created-selected-and-listed
  (with-temporary-directory (directory)
    (make-tree directory)
    (let* ((application (dialog-app directory))
           (dialog (progn (app:operation-save-as application) (dialog-of application))))
      (is (app:create-folder dialog "Casa Silva"))
      (is (eq :directory (sbcl:file-kind (merge-pathnames "Casa Silva/" directory))))
      (is (string= "Casa Silva" (app:file-dialog-selected dialog)))
      (is (member "Casa Silva" (visible-names application) :test #'string=))
      (is (not (app:create-folder dialog "Casa Silva")) "it exists now")
      (is (search "already exists" (app:file-dialog-message dialog)))
      (is (not (app:create-folder dialog "  ")))
      (is (not (app:create-folder dialog "a/b")))
      (is (search "slash" (app:file-dialog-message dialog)))
      (is (eq :directory (sbcl:file-kind (merge-pathnames "Casa Silva/" directory))))
      (is (null (sbcl:file-kind (merge-pathnames "a/" directory)))))))

(test the-new-folder-button-asks-for-a-name-then-creates-the-folder
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory)))
      (app:operation-save-as application)
      (click-widget application :dialog-new-folder)
      (is (app:app-prompt application))
      (is (every #'ui:widget-blocked (remove-if-not (lambda (widget)
                                                      (and (consp (ui:widget-id widget))
                                                           (eq :entry (first (ui:widget-id widget)))))
                                                    (app:build-widgets application)))
          "the dialog is under the prompt")
      (type-text application "Obras")
      (press application :enter)
      (is (null (app:app-prompt application)))
      (is (eq :directory (sbcl:file-kind (merge-pathnames "Obras/" directory))))
      (is (app:app-dialog application) "the dialog is still open")
      (is (find-widget application :dialog-new-folder) "the button stays while saving"))))

(test opening-has-no-new-folder-button-and-the-shortcut-is-for-saving-only
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (is (null (find-widget application :dialog-new-folder)))
      (press application :n :control :shift)
      (is (null (app:app-prompt application))))))

;;; Places and recent files

(test the-places-are-real-distinct-folders-and-the-examples-of-the-project-join-them
  (with-temporary-directory (directory)
    (ensure-directories-exist (merge-pathnames "examples/" directory))
    (let* ((config (base:default-config :project-root directory))
           (places (app:default-places config)))
      (is (string= "Home" (first (first places))))
      (is (equal (user-homedir-pathname) (third (first places))))
      (is (equal (length places) (length (remove-duplicates (mapcar #'third places) :test #'equal)))
          "no folder is listed twice")
      (is (every (lambda (place) (eq :directory (sbcl:file-kind (third place)))) places)
          "every place exists")
      (is (equal (merge-pathnames "examples/" directory)
                 (third (find "Examples" places :key #'first :test #'string=))))
      (is (null (find "Examples" (app:default-places (base:default-config :project-root #p"/nonexistent/"))
                      :key #'first :test #'string=))))))

(test the-recent-list-keeps-the-latest-first-without-repeats-and-within-its-limit
  (with-temporary-directory (directory)
    (let ((state (merge-pathnames "state/" directory)))
      (is (null (persist:load-recent-files state)) "nothing yet")
      (is (null (persist:load-recent-files nil)))
      (is (not (persist:remember-recent-file nil #p"/tmp/a.bcad")) "no state folder, nothing is remembered")
      (dotimes (index 15)
        (is (persist:remember-recent-file state (merge-pathnames (format nil "d~d.bcad" index) directory))))
      (let ((recent (persist:load-recent-files state)))
        (is (= persist:+recent-file-limit+ (length recent)))
        (is (string= "d14.bcad" (file-namestring (first recent))))
        (is (string= "d3.bcad" (file-namestring (car (last recent))))))
      (persist:remember-recent-file state (merge-pathnames "d10.bcad" directory))
      (let ((recent (mapcar #'file-namestring (persist:load-recent-files state))))
        (is (string= "d10.bcad" (first recent)) "a file used again moves to the front")
        (is (= 1 (count "d10.bcad" recent :test #'string=)))
        (is (= persist:+recent-file-limit+ (length recent)))))))

(test a-recent-list-that-cannot-be-written-or-read-is-only-a-shorter-list
  (with-temporary-directory (directory)
    (let ((blocked (merge-pathnames "file-in-the-way/" directory)))
      (touch-text (string-right-trim "/" (uiop:native-namestring blocked)) "x")
      (is (not (persist:remember-recent-file blocked (merge-pathnames "a.bcad" directory))))
      (is (null (persist:load-recent-files blocked))))
    (is (not (persist:remember-recent-file (merge-pathnames "state/" directory)
                                           (make-pathname :name (format nil "line~%break")
                                                          :type "bcad"
                                                          :defaults directory)))
        "a name with a line break cannot be kept in a one-name-per-line file")))

(test saving-and-opening-feed-the-recent-list-and-the-dialog-offers-it
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory :state t))
          (state (merge-pathnames "state/" directory)))
      (is (app:save-app-drawing application (uiop:native-namestring (merge-pathnames "one.bcad" directory))))
      (is (equal '("one.bcad") (mapcar #'file-namestring (persist:load-recent-files state))))
      (touch-drawing (merge-pathnames "two.bcad" directory))
      (is (app:load-drawing-into-app application (uiop:native-namestring (merge-pathnames "two.bcad" directory))))
      (is (equal '("two.bcad" "one.bcad") (mapcar #'file-namestring (persist:load-recent-files state))))
      (delete-file (merge-pathnames "one.bcad" directory))
      (app:operation-open application)
      (is (equal '("two.bcad") (mapcar #'file-namestring (app:file-dialog-recent (dialog-of application))))
          "a recent file that is gone is not offered")
      (setf (app:app-file-path application) nil)
      (click-widget application '(:recent 0))
      (is (null (app:app-dialog application)))
      (is (string= "two.bcad" (file-namestring (app:app-file-path application)))))))

;;; Opening

(test enter-opens-the-first-match-of-the-search
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (type-in-dialog application "zet")
      (press application :enter)
      (is (null (app:app-dialog application)))
      (is (string= "zeta.bcad" (file-namestring (app:app-file-path application))))
      (is (search "Opened zeta.bcad" (message-text application))))))

(test a-double-click-opens-a-drawing-and-a-single-click-only-selects-it
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (click-widget application '(:entry "plan-b.bcad"))
      (is (app:app-dialog application))
      (is (string= "plan-b.bcad" (app:file-dialog-selected (dialog-of application))))
      (is (string= "current.bcad" (file-namestring (app:app-file-path application))) "nothing opened yet")
      (click-widget application '(:entry "plan-b.bcad"))
      (is (null (app:app-dialog application)))
      (is (string= "plan-b.bcad" (file-namestring (app:app-file-path application)))))))

(test the-name-field-and-the-open-button-open-what-is-typed-or-selected
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (is (not (ui:widget-enabled (accept-button application))) "nothing to open yet")
      (click-widget application '(:entry "zeta.bcad"))
      (is (ui:widget-enabled (accept-button application)))
      (click-widget application :dialog-accept)
      (is (string= "zeta.bcad" (file-namestring (app:app-file-path application))))
      ;; Typing a name without its extension finds the drawing.
      (app:operation-open application)
      (click-widget application :dialog-filename)
      (type-text application "plan-b")
      (press application :enter)
      (is (null (app:app-dialog application)))
      (is (string= "plan-b.bcad" (file-namestring (app:app-file-path application)))))))

(test a-path-typed-in-the-search-field-is-followed
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (type-in-dialog application (uiop:native-namestring (merge-pathnames "beta/" directory)))
      (press application :enter)
      (is (string= "beta" (folder-name (app:file-dialog-directory (dialog-of application)))))
      (is (null (app:file-dialog-message (dialog-of application))))
      (type-in-dialog application (uiop:native-namestring (merge-pathnames "plan-b.bcad" directory)))
      (press application :enter)
      (is (null (app:app-dialog application)))
      (is (string= "plan-b.bcad" (file-namestring (app:app-file-path application)))))))

(test the-location-field-takes-a-path-and-escape-gives-back-the-trail
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (is (null (find-widget application :dialog-location)))
      (press application :l :control)
      (is (find-widget application :dialog-location))
      (is (ui:widget-edit (find-widget application :dialog-location)) "it has the keyboard")
      (press application :escape)
      (is (null (find-widget application :dialog-location)))
      (is (app:app-dialog application) "the first Escape only leaves the field")
      (click-widget application :dialog-edit-location)
      (type-text application (uiop:native-namestring (merge-pathnames "beta/" directory)))
      (press application :enter)
      (is (string= "beta" (folder-name (app:file-dialog-directory (dialog-of application)))))
      (is (null (find-widget application :dialog-location)) "arriving brings the trail back")
      (press application :l :control)
      (type-text application "/no/such/place")
      (press application :enter)
      (is (search "does not exist" (dialog-message-text application)))
      (is (find-widget application :dialog-location) "the field stays so the path can be fixed"))))

(test a-drawing-that-cannot-be-opened-is-explained-in-the-dialog-and-the-drawing-stays
  (with-temporary-directory (directory)
    (make-tree directory)
    (touch-text (merge-pathnames "broken.bcad" directory) "(drawing (format-version 9))")
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (click-widget application '(:entry "broken.bcad"))
      (click-widget application '(:entry "broken.bcad"))
      (is (app:app-dialog application) "the dialog stays so another file can be chosen")
      (is (search "newer" (dialog-message-text application)))
      (is (eq :error (app:file-dialog-message-severity (dialog-of application))))
      (is (= 4 (dom:drawing-entity-count (app:app-drawing application))) "the open drawing is untouched")
      (app:app-render application)
      (is (some (lambda (text) (search "newer" text)) (all-text application)) "and the user sees why")
      (click-widget application '(:entry "zeta.bcad"))
      (is (null (app:file-dialog-message (dialog-of application))) "choosing something else clears it"))))

(test opening-asks-about-unsaved-changes-first
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (press application :l)
      (click application 0 0)
      (click application 1000 0)
      (is (app:app-dirty-p application))
      (app:operation-open application)
      (is (null (app:app-dialog application)) "the question comes before the dialog")
      (is (eq :choice (app:prompt-kind (app:app-prompt application))))
      (click-widget application '(:prompt-button 1))            ; Discard
      (is (eq :open (app:file-dialog-mode (dialog-of application)))))))

;;; Saving

(test a-name-without-an-extension-is-saved-in-the-folder-shown-and-not-in-the-working-folder
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-save-as application)
      (type-text application "obra final")
      (press application :enter)
      (is (null (app:app-dialog application)))
      (is (probe-file (merge-pathnames "obra final.bcad" directory)))
      (is (string= "obra final.bcad" (file-namestring (app:app-file-path application))))
      (is (not (app:app-dirty-p application))))))

(test saving-into-a-folder-opened-in-the-dialog-uses-that-folder
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-save-as application)
      (click-widget application '(:entry "beta"))
      (click-widget application '(:entry "beta"))
      (type-text application "inside")
      (press application :enter)
      (is (probe-file (merge-pathnames "beta/inside.bcad" directory)))
      (is (null (probe-file (merge-pathnames "inside.bcad" directory)))))))

(test an-existing-folder-typed-as-the-name-is-entered-not-overwritten
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-save-as application)
      (type-text application "beta")
      (press application :enter)
      (is (app:app-dialog application))
      (is (string= "beta" (folder-name (app:file-dialog-directory (dialog-of application)))))
      (is (null (app:app-prompt application))))))

(test the-save-button-needs-a-name-and-enter-without-one-explains
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory)))
      (app:operation-save-as application)
      (is (ui:widget-enabled (accept-button application)) "the suggested name is there")
      (press application :backspace)
      (is (string= "" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application)))))
      (is (not (ui:widget-enabled (accept-button application))))
      (press application :enter)
      (is (search "Type a name" (dialog-message-text application)))
      (is (app:app-dialog application))
      (type-text application "x")
      (is (null (dialog-message-text application)) "typing clears the complaint")
      (is (ui:widget-enabled (accept-button application))))))

(test an-existing-file-is-replaced-only-after-confirmation-and-cancel-keeps-the-dialog
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory))
          (existing (merge-pathnames "zeta.bcad" directory)))
      (let ((before (file-text existing)))
        (app:operation-save-as application)
        (click-widget application '(:entry "zeta.bcad"))
        (click-widget application :dialog-accept)
        (is (eq :choice (app:prompt-kind (app:app-prompt application))))
        (is (every #'ui:widget-blocked (buttons-behind-the-prompt application))
            "every button but the prompt's own is blocked")
        (click-widget application '(:prompt-button 1))          ; Cancel
        (is (app:app-dialog application))
        (is (string= before (file-text existing)))
        (click-widget application :dialog-accept)
        (click-widget application '(:prompt-button 0))          ; Replace
        (is (null (app:app-dialog application)))
        (is (string/= before (file-text existing)))
        (is (string= "zeta.bcad" (file-namestring (app:app-file-path application))))))))

(test saving-before-another-operation-continues-with-it-after-the-dialog-closes
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory))
          (ran nil))
      (app:operation-save-as application (lambda () (setf ran t)))
      (type-text application "kept")
      (press application :enter)
      (is-true ran)
      (let ((cancelled nil))
        (app:operation-save-as application (lambda () (setf cancelled t)))
        (press application :escape)
        (is (not cancelled) "cancelling the dialog does not continue")))))

(test a-failed-save-keeps-the-dialog-and-says-why
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory)))
      (app:operation-save-as application)
      (type-text application "/proc/nowhere/plan.bcad")
      (press application :enter)
      (is (app:app-dialog application))
      (is (eq :error (app:file-dialog-message-severity (dialog-of application))))
      (is (plusp (length (dialog-message-text application))))
      (is (null (probe-file "/proc/nowhere/plan.bcad")) "nothing was saved")
      (app:app-render application)
      (is (some (lambda (text) (search (dialog-message-text application) text)) (all-text application))))))

;;; The dialog is modal

(test nothing-behind-the-dialog-reacts
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (press application :l)                                   ; a key is not a tool shortcut now
      (is (eq :select (app:tool-kind (app:app-tool application))))
      (type-text application "l")                              ; and typed letters go to the search
      (is (string= "l" (ui:edit-state-text (app:file-dialog-filter-edit (dialog-of application)))))
      (click application 0 0)
      (is (= 4 (dom:drawing-entity-count (app:app-drawing application))) "a click on the canvas does nothing")
      (dolist (id '(:line :save :grid :layer-add :new))
        (is (ui:widget-blocked (find-widget application id)) "~s is blocked" id))
      (send application (app:make-wheel-event 3d0 300d0 300d0))
      (press application :escape)
      (is (null (app:app-dialog application)))
      (press application :l)
      (is (eq :line (app:tool-kind (app:app-tool application))) "shortcuts work again"))))

(test the-dialog-asks-for-text-input-so-typing-reaches-it
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory)))
      (is (not (app:app-wants-text-input-p application)))
      (app:operation-open application)
      (is (app:app-wants-text-input-p application))
      (app:close-file-dialog application)
      (is (not (app:app-wants-text-input-p application))))))

(test tab-moves-the-keyboard-between-the-name-and-the-search
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory)))
      (app:operation-save-as application)
      (is (eq :name (app:file-dialog-focus (dialog-of application))))
      (press application :tab)
      (is (eq :filter (app:file-dialog-focus (dialog-of application))))
      (type-text application "abc")
      (is (string= "abc" (ui:edit-state-text (app:file-dialog-filter-edit (dialog-of application)))))
      (press application :tab)
      (is (eq :name (app:file-dialog-focus (dialog-of application))))
      (is (string= "current.bcad" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application))))))))

;;; Layout

(test the-dialog-fits-the-smallest-window-and-nothing-in-it-overlaps
  (with-temporary-directory (directory)
    (dotimes (index 14)
      (touch-drawing (merge-pathnames (format nil "a-rather-long-drawing-name-number-~d.bcad" index) directory)))
    (ensure-directories-exist (merge-pathnames "folder/" directory))
    (dolist (size (list (list +minimum-window-width+ +minimum-window-height+) (list 1440 900) (list 1920 1080)))
      (destructuring-bind (width height) size
        (let ((application (dialog-app directory :width (float width 1d0) :height (float height 1d0))))
          (dolist (mode '(:open :save))
            (if (eq mode :open) (app:operation-open application) (app:operation-save-as application))
            (app:app-render application)
            (let ((widgets (app:build-widgets application)))
              (dolist (widget widgets)
                (is (not (outside-window-p widget width height))
                    "~s is outside a ~dx~d window" (ui:widget-id widget) width height))
              (let ((controls (remove-if-not (lambda (widget)
                                               (and (member (ui:widget-kind widget) '(:button :field))
                                                    (not (ui:widget-blocked widget))))
                                             widgets)))
                (loop for (first . others) on controls
                      do (dolist (second others)
                           (is (not (overlapping-p first second))
                               "~s overlaps ~s at ~dx~d" (ui:widget-id first) (ui:widget-id second)
                               width height)))))
            (app:close-file-dialog application)))))))

(test every-row-sits-inside-the-list-and-the-entries-in-view-match-the-capacity
  (with-temporary-directory (directory)
    (dotimes (index 40)
      (touch-drawing (merge-pathnames (format nil "d~2,'0d.bcad" index) directory)))
    (let ((application (dialog-app directory :width (float +minimum-window-width+ 1d0)
                                             :height (float +minimum-window-height+ 1d0))))
      (app:operation-open application)
      (app:app-render application)
      (let* ((layout (app::browser-layout-for application))
             (rows (remove-if-not (lambda (widget)
                                    (and (consp (ui:widget-id widget)) (eq :entry (first (ui:widget-id widget)))))
                                  (app:build-widgets application))))
        (is (= (app:file-dialog-capacity (dialog-of application)) (length rows)))
        (dolist (row rows)
          (is (>= (ui:widget-y row) (app::browser-layout-list-top layout)))
          (is (<= (+ (ui:widget-y row) (ui:widget-height row))
                  (+ (app::browser-layout-list-top layout) (app::browser-layout-list-height layout)))))))))

(test long-names-are-cut-to-their-column
  (with-temporary-directory (directory)
    (touch-drawing (merge-pathnames (format nil "~a.bcad" (make-string 150 :initial-element #\w)) directory))
    (let ((application (dialog-app directory)))
      (app:operation-open application)
      (app:app-render application)
      (let ((row (find-if (lambda (widget)
                            (and (consp (ui:widget-id widget)) (eq :entry (first (ui:widget-id widget)))))
                          (app:build-widgets application))))
        (is (plusp (ui:widget-trailing row)) "the name keeps clear of the size and date columns")
        (is (every (lambda (run)
                     (<= (+ (scene:text-run-x run) (ui:text-width-pixels (scene:text-run-string run)
                                                                           (scene:text-run-size run)))
                         1441f0))
                   (scene:draw-list-text-runs (app:app-draw-list application))))))))

(test the-icon-buttons-of-the-dialog-have-tooltips
  (with-temporary-directory (directory)
    (let ((application (dialog-app directory)))
      (app:operation-save-as application)
      (dolist (id '(:dialog-close :dialog-up :dialog-edit-location))
        (hover application id)
        (let ((tooltip (find-widget application :tooltip)))
          (is-true tooltip)
          (is (string= (ui:widget-hint (find-widget application id)) (ui:widget-label tooltip))))))))

(test every-icon-the-dialog-uses-exists
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory :state t))
          (known (ui:icon-names)))
      (persist:remember-recent-file (merge-pathnames "state/" directory) (merge-pathnames "zeta.bcad" directory))
      (dolist (mode '(:open :save))
        (if (eq mode :open) (app:operation-open application) (app:operation-save-as application))
        (type-text application "zzz")
        (dolist (widget (app:build-widgets application))
          (when (ui:widget-icon widget)
            (is (member (ui:widget-icon widget) known) "~s uses the unknown icon ~s"
                (ui:widget-id widget) (ui:widget-icon widget))))
        (app:close-file-dialog application)))))

;;; Dropping a file on the window

(test a-dropped-drawing-opens-and-a-dropped-broken-file-explains
  (with-temporary-directory (directory)
    (make-tree directory)
    (touch-text (merge-pathnames "broken.bcad" directory) "(drawing (format-version 9))")
    (let ((application (dialog-app directory)))
      (send application (app:make-file-drop-event (uiop:native-namestring (merge-pathnames "zeta.bcad" directory))))
      (is (string= "zeta.bcad" (file-namestring (app:app-file-path application))))
      (is (= 3 (dom:drawing-entity-count (app:app-drawing application))))
      (send application (app:make-file-drop-event (uiop:native-namestring (merge-pathnames "broken.bcad" directory))))
      (is (string= "zeta.bcad" (file-namestring (app:app-file-path application))))
      (is (search "newer" (message-text application)))
      (is (eq :error (cdr (app:app-message application)))))))

(test a-dropped-drawing-asks-about-unsaved-changes-first
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory))
          (zeta (uiop:native-namestring (merge-pathnames "zeta.bcad" directory))))
      (press application :l)
      (click application 0 0)
      (click application 1000 0)
      (send application (app:make-file-drop-event zeta))
      (is (eq :choice (app:prompt-kind (app:app-prompt application))))
      (is (string= "current.bcad" (file-namestring (app:app-file-path application))) "nothing opened yet")
      (click-widget application '(:prompt-button 2))            ; Cancel
      (is (string= "current.bcad" (file-namestring (app:app-file-path application))))
      (send application (app:make-file-drop-event zeta))
      (click-widget application '(:prompt-button 1))            ; Discard
      (is (string= "zeta.bcad" (file-namestring (app:app-file-path application)))))))

(test a-drop-closes-the-file-dialog-but-is-ignored-under-a-question
  (with-temporary-directory (directory)
    (make-tree directory)
    (let ((application (dialog-app directory))
          (zeta (uiop:native-namestring (merge-pathnames "zeta.bcad" directory))))
      (app:operation-open application)
      (send application (app:make-file-drop-event zeta))
      (is (null (app:app-dialog application)))
      (is (string= "zeta.bcad" (file-namestring (app:app-file-path application))))
      (setf (app:app-file-path application) (merge-pathnames "current.bcad" directory))
      (app:open-choice-prompt application "Question" "Sure?" (list (cons "Yes" nil) (cons "No" nil)))
      (send application (app:make-file-drop-event zeta))
      (is (string= "current.bcad" (file-namestring (app:app-file-path application))) "ignored")
      (is (app:app-prompt application) "the question is still there"))))

