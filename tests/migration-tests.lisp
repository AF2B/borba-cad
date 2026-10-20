(in-package #:borba-cad/tests)

(def-suite migration
  :description "Reading a file of an older format version: it is migrated as it is read."
  :in all)

(in-suite migration)

(defun fixture-path (name)
  "Return the path of the test fixture called NAME."
  (asdf:system-relative-pathname "borba-cad" (format nil "tests/fixtures/~a" name)))

(defun fixture-text (name)
  "Return the text of the fixture called NAME."
  (with-open-file (stream (fixture-path name) :external-format :utf-8)
    (let ((text (make-string (file-length stream))))
      (subseq text 0 (read-sequence text stream)))))

(defun entity-values (drawing)
  "Return, for every entity of DRAWING in stacking order, its id, layer id, kind and property values."
  (mapcar (lambda (entity)
            (list (dom:entity-id entity)
                  (dom:entity-layer-id entity)
                  (dom:entity-kind entity)
                  (mapcar #'dom:property-value (dom:entity-properties entity))))
          (dom:drawing-entities drawing)))

(test the-fixture-is-a-version-1-file
  (is (search "(format-version 1)" (fixture-text "v1-floor-plan.bcad")))
  (is (null (search "(angle" (fixture-text "v1-floor-plan.bcad"))) "and has no text angle"))

(test a-version-1-file-is-read-and-its-texts-get-the-default-angle
  (let ((drawing (persist:load-drawing (fixture-path "v1-floor-plan.bcad"))))
    (is (= 10 (dom:drawing-entity-count drawing)))
    (let ((texts (remove :text (dom:drawing-entities drawing) :key #'dom:entity-kind :test-not #'eq)))
      (is (plusp (length texts)))
      (dolist (text texts)
        (is (= 0d0 (dom:text-angle text)) "text ~d is horizontal" (dom:entity-id text))))))

(test a-migrated-file-is-saved-in-the-newest-version-and-reads-back-the-same
  (let* ((old (persist:parse-drawing-text (fixture-text "v1-floor-plan.bcad")))
         (written (persist:drawing-to-string old)))
    (is (search (format nil "(format-version ~d)" persist:+supported-format-version+) written))
    (is (search "(angle 0.0)" written) "the migrated texts say their angle now")
    (is (equal (entity-values old) (entity-values (persist:parse-drawing-text written))))
    (is (string= written (persist:drawing-to-string (persist:parse-drawing-text written))) "and it is stable")))

(test migrating-keeps-everything-else-as-it-was
  (let* ((fixture (fixture-text "v1-floor-plan.bcad"))
         (upgraded (replace-once-text fixture "(format-version 1)" "(format-version 2)"))
         (with-angles (add-angle-to-texts upgraded)))
    (is (equal (entity-values (persist:parse-drawing-text fixture))
               (entity-values (persist:parse-drawing-text with-angles)))
        "a version 1 file reads as the version 2 file that says angle 0 in every text")))

(defun replace-once-text (text old new)
  "Return TEXT with the first OLD replaced by NEW."
  (let ((start (search old text)))
    (concatenate 'string (subseq text 0 start) new (subseq text (+ start (length old))))))

(defun add-angle-to-texts (text)
  "Return TEXT, a file, with (angle 0.0) added to every text entity."
  (with-output-to-string (stream)
    (with-input-from-string (input text)
      (loop for line = (read-line input nil nil)
            while line
            do (if (search "(text (id" line)
                   (write-line (concatenate 'string (subseq line 0 (1- (length line))) " (angle 0.0))") stream)
                   (write-line line stream))))))

(test a-version-1-file-cannot-have-what-version-2-added
  (let ((with-angle (add-angle-to-texts (fixture-text "v1-floor-plan.bcad"))))
    (handler-case (progn (persist:parse-drawing-text with-angle)
                         (fail "a text angle in a version 1 file was accepted"))
      (base:drawing-file-content-error (condition)
        (is (search "unknown field \"angle\"" (princ-to-string condition)))))))

(test a-version-2-file-must-say-the-angle
  (let* ((golden (persist:drawing-to-string (golden-drawing)))
         (without (replace-once-text golden " (angle 0.5)" "")))
    (handler-case (progn (persist:parse-drawing-text without)
                         (fail "a version 2 text without its angle was accepted"))
      (base:drawing-file-content-error (condition)
        (is (search "missing field \"angle\"" (princ-to-string condition)))))))

(test the-versions-between-the-oldest-and-the-newest-are-all-read
  (is (= 1 persist:+oldest-supported-format-version+))
  (loop for version from persist:+oldest-supported-format-version+ to persist:+supported-format-version+
        do (is (typep version '(integer 1)))))

(defparameter *empty-hatch-holes* " (hole-vertices ) (hole-sizes )"
  "What the writer says of the holes of a hatch that has none, which version 6 added.")

(defun hatch-lines-with-holes (text)
  "Return TEXT, a file, with the empty holes that version 6 says added to every hatch.

TEXT: the text of a file without them."
  (with-output-to-string (stream)
    (with-input-from-string (input text)
      (loop for line = (read-line input nil nil)
            while line
            do (if (search "(hatch (id" line)
                   (write-line (concatenate 'string
                                            (subseq line 0 (1- (length line)))
                                            *empty-hatch-holes*
                                            ")")
                               stream)
                   (write-line line stream))))))

(defun hatch-lines-without-holes (text)
  "Return TEXT, a file, without the empty holes that version 6 says in every hatch.

TEXT: the text of a file with them."
  (let ((result text))
    (loop for found = (search *empty-hatch-holes* result)
          while found
          do (setf result (concatenate 'string
                                       (subseq result 0 found)
                                       (subseq result (+ found (length *empty-hatch-holes*))))))
    result))

(defun as-newest-version (text version)
  "Return the text of a file of an older VERSION as the writer writes it in the newest version: the version line,
the section of blocks that version 5 added, which is empty, the holes of the hatches that version 6 added, the
section of sheets that version 7 added, which is empty, and the section of underlays that version 9 added, which is
empty.

TEXT: the text of the old file.
VERSION: its format version."
  (let* ((renumbered (replace-once-text text
                                        (format nil "(format-version ~d)" version)
                                        (format nil "(format-version ~d)" persist:+supported-format-version+)))
         (with-blocks (if (< version 5)
                          (replace-once-text renumbered
                                             (format nil " )~% (entities")
                                             (format nil " )~% (blocks~% )~% (entities"))
                          renumbered)))
    (let* ((with-holes (if (< version 6)
                           (hatch-lines-with-holes with-blocks)
                           with-blocks))
           (with-sheets (if (< version 7)
                            (replace-once-text with-holes
                                               (format nil " )~% (entities")
                                               (format nil " )~% (sheets~% )~% (entities"))
                            with-holes)))
      (if (< version 9)
          (replace-once-text with-sheets
                             (format nil " )~% (entities")
                             (format nil " )~% (underlays~% )~% (entities"))
          with-sheets))))

(defun as-older-version (text version)
  "Return the text of a file of the newest version as the file of an older VERSION would read: with that version
line, without the section of blocks when the version predates it, without the holes of the hatches when it
predates them, and without the sections of sheets and of underlays when it predates those.

TEXT: the text of a file of the newest version, with empty sections of blocks, sheets and underlays and no holes in its
hatches.
VERSION: the format version to say."
  (let* ((without-underlays (if (< version 9)
                                (replace-once-text text (format nil " (underlays~% )~%") "")
                                text))
         (renumbered (replace-once-text (if (< version 7)
                                            (replace-once-text without-underlays
                                                               (format nil " (sheets~% )~%")
                                                               "")
                                            without-underlays)
                                        (format nil "(format-version ~d)" persist:+supported-format-version+)
                                        (format nil "(format-version ~d)" version)))
         (without-blocks (if (< version 5)
                             (replace-once-text renumbered
                                                (format nil " (blocks~% )~%")
                                                "")
                             renumbered)))
    (if (< version 6)
        (hatch-lines-without-holes without-blocks)
        without-blocks)))
