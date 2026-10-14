(in-package #:borba-cad/tests)

(def-suite i18n
  :description "Languages: the catalogs, the choice of language, and the interface speaking it."
  :in all)

(in-suite i18n)

(defun catalog-file-form ()
  "Return the form read from the Portuguese catalog file."
  (with-open-file (stream (asdf:system-relative-pathname "borba-cad" "src/i18n/pt-br.catalog")
                          :external-format :utf-8)
    (let ((*read-eval* nil)
          (*package* (find-package :keyword)))
      (read stream))))

(defun portuguese-entries ()
  "Return the (english translation) lists of the Portuguese catalog."
  (i18n:catalog-translations :pt-br))

(defun untranslated-texts (strings)
  "Return the members of STRINGS that are an English text the Portuguese catalog translates into
something else, which is to say text that reached the screen without going through the catalog."
  (remove-if-not (lambda (string)
                   (let ((entry (find string (portuguese-entries) :key #'first :test #'string=)))
                     (and entry (string/= (first entry) (second entry)))))
                 strings))

;;; Translating

(test english-is-the-default-and-its-texts-pass-through-unchanged
  (is (eq :en-us i18n:+default-language+))
  (is (eq :en-us i18n:*language*))
  (is (string= "Save" (i18n:tr "Save")))
  (is (string= "Saved plan.bcad (1 KB)." (i18n:trf "Saved ~a (~a)." "plan.bcad" "1 KB"))))

(test a-text-with-a-translation-is-translated-and-one-without-stays-in-english
  (i18n:with-language (:pt-br)
    (is (string= "Salvar" (i18n:tr "Save")))
    (is (string= "plan.bcad salvo (1 KB)." (i18n:trf "Saved ~a (~a)." "plan.bcad" "1 KB")))
    (is (string= "A text nobody translated" (i18n:tr "A text nobody translated")))
    (is (string= (format nil "Entidade inv~alida (linha): x" (code-char 225))
                 (i18n:trf "Invalid ~a: ~a" (base:entity-kind-label :line) "x")))))

(test with-language-restores-the-language-afterwards-even-after-an-error
  (is (eq :en-us i18n:*language*))
  (handler-case (i18n:with-language (:pt-br)
                  (is (eq :pt-br i18n:*language*))
                  (error "leave"))
    (error () nil))
  (is (eq :en-us i18n:*language*)))

(test a-plural-in-a-translation-follows-the-count
  (i18n:with-language (:pt-br)
    (let ((control "Deleted ~d ~:*~[entities~;entity~:;entities~].~a"))
      (is (string= (format nil "1 entidade exclu~ada." (code-char 237)) (i18n:trf control 1 "")))
      (is (string= (format nil "3 entidades exclu~adas." (code-char 237)) (i18n:trf control 3 "")))
      (is (string= (format nil "0 entidades exclu~adas." (code-char 237)) (i18n:trf control 0 ""))))))

;;; Names of languages

(test the-program-knows-which-languages-it-speaks-and-names-each-in-its-own
  (is (equal '(:en-us :pt-br) (i18n:supported-languages)))
  (is (string= "en-US" (i18n:language-tag :en-us)))
  (is (string= "pt-BR" (i18n:language-tag :pt-br)))
  (is (string= "English (US)" (i18n:language-name :en-us)))
  (is (string= (format nil "Portugu~as (Brasil)" (code-char 234)) (i18n:language-name :pt-br))))

(test language-tags-are-understood-in-the-usual-spellings
  (dolist (text '("pt-BR" "pt-br" "pt_BR" "PT_br" " pt-br " "pt"))
    (is (eq :pt-br (i18n:parse-language text)) "~s" text))
  (dolist (text '("en-US" "en-us" "en_US" "en"))
    (is (eq :en-us (i18n:parse-language text)) "~s" text))
  (dolist (text '("" "fr" "pt-PT" "klingon"))
    (is (null (i18n:parse-language text)) "~s" text)))

;;; The catalog

(test the-catalog-file-names-its-language-and-has-no-duplicate-keys
  (let ((form (catalog-file-form)))
    (is (eq :pt-br (getf form :language)))
    (is (plusp (length (getf form :entries))))
    (is (= (length (getf form :entries)) (length (portuguese-entries))) "no English text is translated twice")))

(test every-translation-asks-for-the-same-values-as-its-english-text
  (dolist (entry (portuguese-entries))
    (destructuring-bind (english translation) entry
      (is (equal (i18n:format-directives english) (i18n:format-directives translation))
          "~s: ~s asks for other values" english translation)
      (is (plusp (length translation)) "~s has an empty translation" english)
      (is (every (lambda (character) (or (graphic-char-p character) (char= #\Space character))) translation)
          "~s has a control character" english))))

(test every-translation-fills-in-with-the-values-its-english-text-takes
  (dolist (entry (portuguese-entries))
    (let* ((english (first entry))
           (arguments (loop for directive in (i18n:format-directives english)
                            when (member (char directive (1- (length directive))) '(#\a #\s #\d #\f #\[ #\{ #\*))
                              collect (case (char directive (1- (length directive)))
                                        (#\d 1)
                                        (#\f 1.5d0)
                                        (#\[ 1)
                                        (#\{ '(:mm))
                                        (t "text")))))
      (is (stringp (handler-case (i18n:with-language (:pt-br) (apply #'i18n:trf english arguments))
                     (error (condition) (format nil "~a" condition))))
          "~s" english))))

(test directives-are-read-with-their-parameters-and-modifiers
  (is (equal '("~a" "~d") (i18n:format-directives "x ~a y ~d")))
  (is (equal '("~:*" "~[" "~;" "~:;" "~]") (i18n:format-directives "~:*~[a~;b~:;c~]")))
  (is (equal '("~4,'0d" "~2,'0d") (i18n:format-directives "~4,'0d-~2,'0d")))
  (is (equal '("~@[" "~(" "~a" "~)" "~]") (i18n:format-directives "~@[ | ~(~a~)~]")))
  (is (equal '("~a") (i18n:format-directives "100~~ ~a")) "a doubled tilde is not a directive")
  (is (null (i18n:format-directives "no directives"))))

;;; The configuration

(test the-language-can-be-chosen-by-option-and-by-environment
  (let ((config (base:make-config)))
    (is (null (base:config-language config)) "nothing chosen is nothing, not English")
    (is (eq :pt-br (base:config-language (base:config-with-arguments config '("--language" "pt-BR")))))
    (is (eq :en-us (base:config-language (base:config-with-arguments config '("--language" "en")))))
    (is (eq :pt-br (base:config-language (base:config-with-environment
                                          config
                                          (lambda (name) (and (string= name "BORBA_CAD_LANGUAGE") "pt_br"))))))
    (signals base:configuration-error (base:config-with-arguments config '("--language" "fr")))
    (signals base:configuration-error (base:config-with-arguments config '("--language")))
    (signals base:configuration-error (base:config-with-environment
                                       config
                                       (lambda (name) (and (string= name "BORBA_CAD_LANGUAGE") "klingon"))))))

;;; The choice is kept

(defun language-app (directory &key language)
  "Create an application that remembers its choices in DIRECTORY and, when LANGUAGE is given, was
asked to speak it."
  (let ((config (quiet-config)))
    (setf (base:config-state-directory config) directory
          (base:config-language config) language)
    (app:make-app config)))

(test choosing-a-language-changes-the-interface-and-is-remembered-for-the-next-run
  (with-temporary-directory (directory)
    (let ((application (language-app directory)))
      (unwind-protect
           (progn
             (is (eq :en-us i18n:*language*))
             (app:operation-set-language application :pt-br)
             (is (eq :pt-br i18n:*language*))
             (is (eql 0 (search "Idioma: Portugu" (message-text application))))
             (is (equal "pt-BR" (persist:load-setting directory "language")))
             (setf i18n:*language* :en-us)
             (app:make-app (let ((config (quiet-config)))
                             (setf (base:config-state-directory config) directory)
                             config))
             (is (eq :pt-br i18n:*language*) "the next run starts in the saved language"))
        (setf i18n:*language* :en-us)))))

(test an-option-beats-the-saved-language-and-nothing-saved-means-english
  (with-temporary-directory (directory)
    (unwind-protect
         (progn
           (language-app directory)
           (is (eq :en-us i18n:*language*) "nothing saved, nothing asked")
           (persist:save-setting directory "language" "pt-BR")
           (language-app directory :language :en-us)
           (is (eq :en-us i18n:*language*) "the option wins")
           (language-app directory)
           (is (eq :pt-br i18n:*language*) "without the option the saved one is used")
           (persist:save-setting directory "language" "klingon")
           (setf i18n:*language* :en-us)
           (language-app directory)
           (is (eq :en-us i18n:*language*) "a saved language the program does not speak is ignored"))
      (setf i18n:*language* :en-us))))

(test settings-keep-each-name-once-and-survive-what-cannot-be-written
  (with-temporary-directory (directory)
    (is (null (persist:load-setting directory "language")))
    (is (persist:save-setting directory "language" "pt-BR"))
    (is (persist:save-setting directory "other" "1"))
    (is (persist:save-setting directory "language" "en-US"))
    (is (equal '(("other" . "1") ("language" . "en-US")) (persist:load-settings directory)))
    (is (null (persist:save-setting directory "a=b" "x")) "a name with an equals sign is refused")
    (is (null (persist:save-setting directory "a" (format nil "x~%y"))) "a value with a line break is refused")
    (is (null (persist:save-setting nil "language" "pt-BR")) "no state directory, nothing to save")
    (is (null (persist:load-setting nil "language")))))

;;; The interface in Portuguese

(defun language-menu-rows (application)
  "Return the ids of the rows of the open menu."
  (mapcar #'ui:widget-id (menu-rows application)))

(test the-language-button-opens-a-menu-of-languages-and-choosing-one-switches
  (let ((application (make-test-app)))
    (unwind-protect
         (progn
           (click-widget application :language)
           (is (equal '((:menu-item :en-us) (:menu-item :pt-br)) (language-menu-rows application)))
           (is (ui:widget-active (find-widget application '(:menu-item :en-us))) "the one in use is highlighted")
           (click-widget application '(:menu-item :pt-br))
           (is (eq :pt-br i18n:*language*))
           (is (null (language-menu-rows application)) "the menu closed")
           (is (string= "Salvar" (ui:widget-hint (find-widget application :save))))
           (click-widget application :language)
           (click-widget application '(:menu-item :en-us))
           (is (eq :en-us i18n:*language*))
           (is (string= "Save" (ui:widget-hint (find-widget application :save)))))
      (setf i18n:*language* :en-us))))

(defun render-texts (application)
  "Render a frame and return every string drawn."
  (app:app-render application)
  (all-text application))

(test no-english-text-reaches-the-screen-in-portuguese
  (i18n:with-language (:pt-br)
    (let* ((application (make-test-app :drawing (rng:generate-drawing :seed 5 :entity-count 40)))
           (leftovers '()))
      (flet ((check (label)
               (let ((found (untranslated-texts (render-texts application))))
                 (when found
                   (push (list label found) leftovers)))))
        (check "the main window")
        (dolist (kind '(:line :rectangle :circle :polyline :text :dimension))
          (let ((entity (find kind (entities-of application) :key #'dom:entity-kind)))
            (when entity
              (select-only application entity)
              (check kind))))
        (app:operation-select-all application)
        (check "a selection")
        (click-widget application :draw)
        (check "the draw menu")
        (press application :escape)
        (click-widget application :language)
        (check "the language menu")
        (press application :escape)
        (click-widget application :layer-rename)
        (check "a prompt")
        (app:close-prompt application)
        (app:operation-open application)
        (check "the open dialog")
        (app:close-file-dialog application)
        (app:operation-save-as application)
        (check "the save dialog")
        (app:close-file-dialog application)
        (app:operation-new application)
        (check "an empty drawing"))
      (is (null leftovers) "English text on the screen in Portuguese: ~s" leftovers))))

(test the-messages-of-the-operations-come-out-in-portuguese
  (i18n:with-language (:pt-br)
    (let ((application (make-test-app)))
      (app:operation-undo application)
      (is (string= "Nada para desfazer." (message-text application)))
      (app:operation-delete-selection application)
      (is (eql 0 (search "Nada est" (message-text application))))
      (is (search "selecionado." (message-text application)))
      (app:operation-add-layer application "paredes")
      (is (search "Camada" (message-text application)))
      (press application :l)
      (is (search "Linha:" (app:tool-prompt (app:app-tool application) application)))
      (click application 0 0)
      (click application 0 0)
      (is (search "(linha)" (message-text application)) "a line of no length, as the error says it"))))
