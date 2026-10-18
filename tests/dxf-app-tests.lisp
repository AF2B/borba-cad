(in-package #:borba-cad/tests)

(def-suite dxf-app
  :description "DXF in the application: the dialogs, the questions, the report, and the files they make."
  :in all)

(in-suite dxf-app)

;;; Files

(defun copy-fixture (name directory &optional (new-name name))
  "Copy the DXF fixture NAME into DIRECTORY under NEW-NAME; return the path of the copy."
  (let ((path (merge-pathnames new-name directory)))
    (touch-text path (fixture-text name))
    path))

(defun write-bytes-file (path octets)
  "Write OCTETS to PATH."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence octets out)))

(defun dxf-app (directory)
  "Return an application, with a drawing that has a file in DIRECTORY, so that dialogs start there."
  (dialog-app directory))

(defun prompt-title-of (application)
  "Return the heading of the open prompt, or NIL."
  (let ((prompt (app:app-prompt application)))
    (and prompt (app:prompt-title prompt))))

(defun prompt-labels (application)
  "Return the labels of the buttons of the open prompt."
  (mapcar #'car (app:prompt-choices (app:app-prompt application))))

(defun choose-in-prompt (application index)
  "Click the INDEX-th button of the open prompt."
  (click-widget application (list :prompt-button index)))

(defun dialog-title-text (application)
  "Return the heading of the open file dialog."
  (ui:widget-label (find-widget application :dialog-title)))

;;; Reading and writing files

(test a-dxf-file-is-read-from-disk-and-a-drawing-is-written-to-one
  (with-temporary-directory (directory)
    (let ((path (copy-fixture "plan-r12.dxf" directory)))
      (multiple-value-bind (drawing report) (dxf:load-dxf path)
        (is (= 5 (dom:drawing-entity-count drawing)))
        (is (= 5 (dxf:report-imported-total report))))
      (multiple-value-bind (saved properties report)
          (dxf:save-dxf (export-sample-drawing) (merge-pathnames "out.dxf" directory))
        (is (probe-file saved))
        (is (plusp (getf properties :bytes)))
        (is (= 9 (dxf:report-imported-total report)))
        (is (= (getf properties :bytes) (with-open-file (stream saved :element-type '(unsigned-byte 8))
                                          (file-length stream))))
        (is (= 14 (dom:drawing-entity-count (dxf:load-dxf saved :units :millimeter)))
            "and it reads back, the dimension as its three lines, two arrowheads and text")))))

(test saving-a-dxf-file-replaces-the-old-one-whole-or-not-at-all
  (with-temporary-directory (directory)
    (let ((path (merge-pathnames "out.dxf" directory)))
      (touch-text path "old contents")
      (dxf:save-dxf (export-sample-drawing) path)
      (is (search "SECTION" (fixture-like-text path)))
      (is (null (directory (merge-pathnames "*.tmp-*" directory))) "no temporary file is left behind"))))

(defun fixture-like-text (path)
  "Return the text of the file at PATH."
  (with-open-file (stream path :external-format :latin-1)
    (let ((text (make-string (file-length stream))))
      (subseq text 0 (read-sequence text stream)))))

(test a-dxf-file-in-the-old-western-code-page-is-read-with-its-accents
  (with-temporary-directory (directory)
    (let ((path (merge-pathnames "latin.dxf" directory))
          (text (entities-file `((0 "TEXT") (8 "A") (10 "0") (20 "0") (40 "100")
                                 (1 ,(format nil "Caf~a" (code-char #xE9)))))))
      ;; Written as one byte for each character, the way a file from an old program is.
      (write-bytes-file path (map '(vector (unsigned-byte 8)) #'char-code text))
      (is (string= (format nil "Caf~a" (code-char #xE9))
                   (dom:text-string (entity-of-kind (dxf:load-dxf path) :text)))))))

(test files-that-cannot-be-imported-say-why
  (with-temporary-directory (directory)
    (flet ((reason (path)
             (handler-case (progn (dxf:load-dxf path) nil)
               (base:drawing-file-error (condition) (princ-to-string condition)))))
      (is (search "does not exist" (reason (merge-pathnames "missing.dxf" directory))))
      (is (search "is a folder" (reason directory)))
      (touch-text (merge-pathnames "plain.dxf" directory) "this is not a drawing")
      (is (search "line 1" (reason (merge-pathnames "plain.dxf" directory))))
      (write-bytes-file (merge-pathnames "binary.dxf" directory)
                    (map '(vector (unsigned-byte 8)) #'char-code "AutoCAD Binary DXF"))
      (is (search "binary DXF" (reason (merge-pathnames "binary.dxf" directory))))
      (touch-text (merge-pathnames "empty.dxf" directory) "")
      (is (reason (merge-pathnames "empty.dxf" directory))))))

;;; The import dialog

(test the-import-dialog-lists-dxf-files-and-folders-and-says-what-it-is-for
  (with-temporary-directory (directory)
    (copy-fixture "plan-r12.dxf" directory)
    (copy-fixture "plan-r12.dxf" directory "Other.DXF")
    (touch-drawing (merge-pathnames "drawing.bcad" directory))
    (touch-text (merge-pathnames "notes.txt" directory) "x")
    (ensure-directories-exist (merge-pathnames "folder/" directory))
    (let ((application (dxf-app directory)))
      (app:operation-import-dxf application)
      (is (string= "Import a DXF file" (dialog-title-text application)))
      (is (equal '("folder" "Other.DXF" "plan-r12.dxf") (visible-names application))
          "drawings of the program and other files are not in the way, the extension may be in capitals")
      (is (string= "Import" (ui:widget-label (accept-button application))))
      (click-widget application :dialog-all-files)
      (is (member "drawing.bcad" (visible-names application) :test #'string=) "unless all files are asked for"))))

(test choosing-a-dxf-file-makes-a-new-unsaved-drawing-of-it
  (with-temporary-directory (directory)
    (copy-fixture "plan-r12.dxf" directory)
    (let ((application (dxf-app directory)))
      (app:operation-import-dxf application)
      (is (equal '("plan-r12.dxf") (visible-names application)))
      (type-text application "plan-r12")
      (press application :enter)
      (is (null (app:app-dialog application)) "the dialog closed")
      (is (null (app:app-prompt application)) "the file said its unit, so nothing was asked")
      (is (= 5 (dom:drawing-entity-count (app:app-drawing application))))
      (is (null (app:app-file-path application)) "the new drawing has no file yet")
      (is (app:app-dirty-p application) "and it is not saved")
      (is (string= "Untitled * - Borba CAD" (app:app-title application)))
      (is (search "Imported plan-r12.dxf" (message-text application)))
      (is (search "5 entities imported" (message-text application)))
      (is (not (ui:widget-enabled (find-widget application :undo))) "a fresh history"))))

(test a-file-that-does-not-say-its-unit-asks-for-it
  (with-temporary-directory (directory)
    (touch-text (merge-pathnames "unitless.dxf" directory)
                (entities-file '((0 "LINE") (8 "A") (10 "0") (20 "0") (11 "5") (21 "0"))))
    (let ((application (dxf-app directory)))
      (app:operation-import-dxf application)
      (let ((original (app:app-drawing application)))
        (type-text application "unitless")
        (press application :enter)
        (is (string= "Units of the DXF file" (prompt-title-of application)))
        (is (equal '("Millimeters" "Centimeters" "Meters" "Cancel") (prompt-labels application)))
        (is (eq original (app:app-drawing application)) "nothing changes before it is answered")
        (choose-in-prompt application 2)
        (is (= 5000d0 (dom:line-x2 (first (entities-of application)))) "meters")
        (is (eq :meter (dom:drawing-display-unit (app:app-drawing application))))))))

(test closing-the-units-question-keeps-the-drawing-that-was-open
  (with-temporary-directory (directory)
    (touch-text (merge-pathnames "unitless.dxf" directory)
                (entities-file '((0 "LINE") (8 "A") (10 "0") (20 "0") (11 "5") (21 "0"))))
    (let* ((application (dxf-app directory))
           (original (app:app-drawing application)))
      (app:operation-import-dxf application)
      (type-text application "unitless")
      (press application :enter)
      (press application :escape)
      (is (null (app:app-prompt application)))
      (is (eq original (app:app-drawing application)))
      (is (equal (merge-pathnames "current.bcad" directory) (app:app-file-path application))))))

(test what-was-left-out-is-told-in-a-report-that-stays-until-it-is-read
  (with-temporary-directory (directory)
    (touch-text (merge-pathnames "mixed.dxf" directory)
                (dxf-text '(0 "SECTION") '(2 "HEADER") '(9 "$INSUNITS") '(70 "4") '(0 "ENDSEC")
                          '(0 "SECTION") '(2 "ENTITIES")
                          '(0 "LINE") '(8 "A") '(10 "0") '(20 "0") '(11 "1") '(21 "1")
                          '(0 "SPLINE") '(8 "A") '(0 "HATCH") '(8 "A") '(0 "HATCH") '(8 "A")
                          '(0 "ENDSEC") '(0 "EOF")))
    (let ((application (dxf-app directory)))
      (app:operation-import-dxf application)
      (type-text application "mixed")
      (press application :enter)
      (is (string= "Imported mixed.dxf" (prompt-title-of application)))
      (is (search "Left out: 2 HATCH, 1 SPLINE." (app:prompt-message (app:app-prompt application))))
      (is (equal '("OK") (prompt-labels application)))
      (is (= 1 (dom:drawing-entity-count (app:app-drawing application))) "the drawing is there behind the report")
      (choose-in-prompt application 0)
      (is (null (app:app-prompt application))))))

(test a-file-that-is-not-a-dxf-leaves-the-dialog-open-with-the-reason-and-the-drawing-alone
  (with-temporary-directory (directory)
    (touch-text (merge-pathnames "broken.dxf" directory) "just words")
    (let* ((application (dxf-app directory))
           (original (app:app-drawing application)))
      (app:operation-import-dxf application)
      (type-text application "broken")
      (press application :enter)
      (is-true (app:app-dialog application) "it stays open")
      (is (search "line 1" (dialog-message-text application)))
      (is (eq original (app:app-drawing application)))
      (is (eq :error (app:file-dialog-message-severity (dialog-of application)))))))

(test a-drawing-with-unsaved-changes-is-offered-for-saving-before-an-import
  (with-temporary-directory (directory)
    (let ((application (dxf-app directory)))
      (press application :l)
      (click application 0 0)
      (click application 500 0)
      (is (app:app-dirty-p application))
      (app:operation-import-dxf application)
      (is (string= "Unsaved changes" (prompt-title-of application)))
      (is (null (app:app-dialog application)) "the dialog waits for the answer")
      (choose-in-prompt application 2)
      (is (null (app:app-dialog application)) "cancelled, no dialog")
      (app:operation-import-dxf application)
      (choose-in-prompt application 1)
      (is-true (app:app-dialog application) "discarded, the dialog opens")
      (is (string= "Import a DXF file" (dialog-title-text application))))))

;;; The export dialog

(test the-export-dialog-offers-the-name-of-the-drawing-with-the-dxf-extension
  (with-temporary-directory (directory)
    (let ((application (dxf-app directory)))
      (app:operation-export-dxf application)
      (is (string= "Export as a DXF file" (dialog-title-text application)))
      (is (string= "current.dxf" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application)))))
      (is (string= "Export" (ui:widget-label (accept-button application))))
      (setf (app:app-file-path application) nil)
      (app:close-file-dialog application)
      (app:operation-export-dxf application)
      (is (string= "untitled.dxf" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application))))))))

(test exporting-writes-the-file-says-what-changed-and-leaves-the-drawing-as-it-is
  (with-temporary-directory (directory)
    (let ((application (dxf-app directory))
          (path (merge-pathnames "current.bcad" directory)))
      (app:commit-changes application
                          (dom:create-entities-changes
                           (list (dom:make-ellipse :id 99 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius-x 500d0
                                                   :radius-y 200d0)))
                          "entity.create")
      (let ((revision (dom:drawing-revision (app:app-drawing application)))
            (dirty (app:app-dirty-p application)))
        (app:operation-export-dxf application)
        (press application :enter)
        (is (null (app:app-dialog application)))
        (is (probe-file (merge-pathnames "current.dxf" directory)))
        (is (string= "DXF exported" (prompt-title-of application)) "a modal says it worked")
        (is (search "current.dxf was saved" (app:prompt-message (app:app-prompt application))))
        (is (search "5 entities exported" (app:prompt-message (app:app-prompt application))))
        (is (search "Ellipses were written as polylines" (app:prompt-message (app:app-prompt application))))
        (is (equal '("OK") (prompt-labels application)))
        (press application :enter)
        (is (null (app:app-prompt application)) "OK closes it")
        (is (= revision (dom:drawing-revision (app:app-drawing application))))
        (is (eq dirty (app:app-dirty-p application)) "an export does not save the drawing")
        (is (equal path (app:app-file-path application)) "and does not rename it")
        (is (= 5 (dom:drawing-entity-count (dxf:load-dxf (merge-pathnames "current.dxf" directory)
                                                         :units :millimeter))))))))

(test a-name-typed-for-an-export-gets-the-extension-and-an-existing-file-is-asked-about
  (with-temporary-directory (directory)
    (touch-text (merge-pathnames "plan.dxf" directory) "old")
    (let ((application (dxf-app directory)))
      (app:operation-export-dxf application)
      (type-text application "plan")
      (press application :enter)
      (is (string= "Replace file?" (prompt-title-of application)) "plan became plan.dxf, which is there")
      (choose-in-prompt application 1)
      (is (equal "old" (fixture-like-text (merge-pathnames "plan.dxf" directory))) "cancelled, untouched")
      (app:operation-export-dxf application)
      (type-text application "fresh")
      (press application :enter)
      (is (probe-file (merge-pathnames "fresh.dxf" directory)))
      (press application :enter)
      (app:operation-export-dxf application)
      (type-text application "v1.2 final")
      (press application :enter)
      (is (probe-file (merge-pathnames "v1.2 final.dxf" directory)) "a name with a dot that is not an extension"))))

(test a-failed-export-is-told-in-a-modal-and-the-dialog-stays
  (with-temporary-directory (directory)
    (let ((application (dxf-app directory)))
      (app:operation-export-dxf application)
      (is (null (app:export-dxf-from-app application
                                         (uiop:native-namestring (merge-pathnames "no-such-folder/out.dxf"
                                                                                  directory)))))
      (is (string= "Could not export" (prompt-title-of application)))
      (is (search "out.dxf" (app:prompt-message (app:app-prompt application))))
      (is (equal '("OK") (prompt-labels application)))
      (press application :enter)
      (is (null (app:app-prompt application)))
      (is-true (app:app-dialog application) "the dialog is still there, to try another name"))))

(test an-export-into-a-folder-that-cannot-be-written-says-why-and-keeps-the-dialog
  (with-temporary-directory (directory)
    (let ((application (dxf-app directory)))
      (app:operation-export-dxf application)
      (type-text application (uiop:native-namestring (merge-pathnames "missing/folder/out.dxf" directory)))
      (press application :enter)
      (is-true (app:app-dialog application))
      (is (search "does not exist" (dialog-message-text application))))))

;;; The menu and the keys

(test the-more-button-opens-a-menu-with-the-dxf-and-script-operations
  (let ((application (make-test-app)))
    (click-widget application :more)
    (is (equal '((:menu-item :import-dxf)
                 (:menu-item :export-dxf)
                 (:menu-item :export-image)
                 (:menu-item :export-pdf)
                 (:menu-item :export-svg)
                 (:menu-item :console)
                 (:menu-item :run-script)
                 (:menu-item :export-script))
               (mapcar #'ui:widget-id (menu-rows application))))
    (is (equal '("Import DXF..." "Export DXF..." "Export as JPEG..." "Export as PDF..." "Export as SVG..." "Console"
                 "Run script..." "Export as script...")
               (mapcar #'ui:widget-label (menu-rows application))))
    (is (equal '("Ctrl+Shift+I" "Ctrl+Shift+E" nil nil nil "F2" nil nil)
               (mapcar #'ui:widget-shortcut (menu-rows application))))
    (click-widget application '(:menu-item :export-dxf))
    (is (null (menu-rows application)))
    (is (string= "Export as a DXF file" (dialog-title-text application)))))

(test the-import-row-and-the-keys-open-the-dialogs
  (let ((application (make-test-app)))
    (click-widget application :more)
    (click-widget application '(:menu-item :import-dxf))
    (is (string= "Import a DXF file" (dialog-title-text application)))
    (app:close-file-dialog application)
    (press application :e :control :shift)
    (is (string= "Export as a DXF file" (dialog-title-text application)))
    (app:close-file-dialog application)
    (press application :i :control :shift)
    (is (string= "Import a DXF file" (dialog-title-text application)))
    (app:close-file-dialog application)
    (press application :e :control)
    (is (null (app:app-dialog application)) "without Shift the key does nothing here")))

(test the-more-button-sits-between-the-document-name-and-the-language-button
  (let* ((application (make-test-app))
         (more (find-widget application :more))
         (language (find-widget application :language))
         (name (find-widget application :document-name)))
    (is (< (+ (ui:widget-x more) (ui:widget-width more)) (ui:widget-x language)))
    (is (< (+ (ui:widget-x name) (ui:widget-width name)) (ui:widget-x more)))
    (is (eq :more (ui:widget-icon more)))))

;;; In Portuguese

(test the-dxf-dialogs-questions-and-reports-are-in-portuguese-in-portuguese
  (i18n:with-language (:pt-br)
    (with-temporary-directory (directory)
      (touch-text (merge-pathnames "unitless.dxf" directory)
                  (entities-file '((0 "LINE") (8 "A") (10 "0") (20 "0") (11 "5") (21 "0"))
                                 '((0 "SPLINE") (8 "A"))))
      (let ((application (dxf-app directory))
            (leftovers '()))
        (flet ((check (label)
                 (let ((found (untranslated-texts (render-texts application))))
                   (when found (push (list label found) leftovers)))))
          (click-widget application :more)
          (check "the menu")
          (click-widget application '(:menu-item :import-dxf))
          (check "the import dialog")
          (type-text application "unitless")
          (press application :enter)
          (check "the units question")
          (choose-in-prompt application 0)
          (check "the report")
          (choose-in-prompt application 0)
          (app:operation-export-dxf application)
          (check "the export dialog"))
        (is (null leftovers) "English text on the screen: ~s" leftovers)))))
