(add-to-list 'load-path "../")
(require 'ert)
(require 'orgify-compiler)

;;; Tokenizer

(ert-deftest test-orgify--tokenize-substitutions ()
  (should (equal '((sub "{{ contents }}"))
                 (with-temp-buffer
                   (insert "{{ contents }}")
                   (goto-char 1)
                   (orgify--tokenize))))
  (should (equal '((sub "{{contents}}"))
                 (with-temp-buffer
                   (insert "{{contents}}")
                   (goto-char 1)
                   (orgify--tokenize)))))

(ert-deftest test-does-not-tokenize-single-braces ()
  (should (equal '((text "{ contents }"))
                 (with-temp-buffer
                   (insert "{ contents }\n")
                   (goto-char 1)
                   (orgify--tokenize)))))

(ert-deftest test-substitution-missing-ending-brace ()
  (should (equal '((text "{{ contents"))
                 (with-temp-buffer
                   (insert "{{ contents\n")
                   (goto-char 1)
                   (orgify--tokenize)))))

(ert-deftest test-orgify--tokenize-loops ()
  (should (equal '((each-begin "#each foo in foobar")
                   (text "\n")
                   (sub "{{ foo }}")
                   (text "\n")
                   (each-end "/each"))
                 (with-temp-buffer
                   (insert "#each foo in foobar\n{{ foo }}\n/each")
                   (goto-char 1)
                   (orgify--tokenize)))))

;; TODO: Need error handling
(ert-deftest test-does-not-tokenize-loops-on-same-line ()
  (should (equal '((each-begin "#each foo in foobar {{ foo }} /each"))
                 (with-temp-buffer
                   (insert "#each foo in foobar {{ foo }} /each")
                   (goto-char 1)
                   (orgify--tokenize)))))

(ert-deftest test-orgify--tokenize-text ()
  (should (equal '((text "foobar"))
                 (with-temp-buffer
                   (insert "foobar\n")
                   (goto-char 1)
                   (orgify--tokenize)))))

;; TODO: Fix (missing ending char)
;; (ert-deftest test-tokenize-truncates-ending-newlines-but-not-characters ()
;;   (should (equal '((text "foobar"))
;;                  (with-temp-buffer
;;                    (insert "foobar")
;;                    (goto-char 1)
;;                    (orgify--tokenize)))))

;;; Parser

(ert-deftest test-parsing-text ()
  (should (equal '((text "foobar"))
                 (orgify--parse '((text "foobar"))))))

(ert-deftest test-parsing-substitutions ()
  (should (equal '((sub "foobar"))
                 (orgify--parse '((sub "{{ foobar }}"))))))

;; TODO: fix white-space significance
;; (ert-deftest test-parsing-substitutions ()
;;   (should (equal '((sub "foobar"))
;;                  (orgify--parse '((sub "{{foobar}}"))))))

(ert-deftest test-parsing-loops ()
  (should (equal '((loop "foo" "foobar" ((text "\n")
                                         (sub "foo")
                                         (text "\n"))))
                 (orgify--parse '((each-begin "#each foo in foobar")
                                  (text "\n")
                                  (sub "{{ foo }}")
                                  (text "\n")
                                  (each-end "\end"))))))

;; TODO: Need error handling
;; (ert-deftest test-parsing-loops-missing-end ()
;;   (should (equal '((loop "foo" "foobar" ((text "\n")
;;                                          (sub "foo")
;;                                          (text "\n"))))
;;                  (orgify--parse '((each-begin "#each foo in foobar")
;;                                   (text "\n")
;;                                   (sub "{{ foo }}")
;;                                   (text "\n"))))))

;; TODO: Need error handling
;; (ert-deftest test-parsing-loops-on-same-line ()
;;   (should (equal '((loop "foo" "foobar" ((text "\n")
;;                                          (sub "foo")
;;                                          (text "\n"))))
;;                  (orgify--parse '((each-begin "#each foo in foobar {{ foo }} /each"))))))

;;; Codgen

(ert-deftest test-codegen-text ()
  (let ((keywords (make-hash-table :test 'equal)))
    (should (equal '((insert "foobar"))
                   (orgify--generate-code '((text "foobar")) keywords)))))

(ert-deftest test-codegen-sub ()
  (let ((keywords (make-hash-table :test 'equal)))
    (puthash "content" "<p>Hello world!</p>" keywords)
    (should (equal '((insert "<p>Hello world!</p>"))
                   (orgify--generate-code '((sub "content")) keywords)))))

;; TODO: Need error handling
(ert-deftest test-codegen-sub-keyword-missing ()
  (let ((keywords (make-hash-table :test 'equal)))
    (should (equal '((insert nil))
                   (orgify--generate-code '((sub "nocontent")) keywords)))))

(ert-deftest test-codegen-loop ()
  (let ((keywords (make-hash-table :test 'equal)))
    (puthash "foobar" '("one" "two" "three") keywords)
    (should (equal '((insert "one")
                     (insert "two")
                     (insert "three"))
                   (orgify--generate-code '((loop "foo" "foobar" ((sub "foo")))) keywords)))))

;; TODO: Need error handling
(ert-deftest test-codegen-loop-keyword-missing ()
  (let ((keywords (make-hash-table :test 'equal)))
    (should (equal '()
                   (orgify--generate-code '((loop "foo" "foobar" ((sub "foo")))) keywords)))))

;;; Fixture tests

(defmacro with-simple-template (&rest body)
  `(with-temp-buffer
    (insert-file-contents "fixtures/simple-template.html")
    (goto-char (point-min))
    ,@body))

(ert-deftest fixture-test-orgify--tokenize ()
  (should (equal '((text "<p>Hello ")
	           (sub "{{ name }}")
                   (text "!</p>\n\n<ul>\n  ")
                   (each-begin "#each page in pages")
                   (text "\n  <li>\n    ")
                   (sub "{{ page }}")
	           (text "\n  </li>\n  ")
	           (each-end "/each")
	           (text "\n</ul>"))
          (with-simple-template (orgify--tokenize)))))

(ert-deftest fixture-test-orgify--parse ()
  (should (equal '((text "<p>Hello ")
	           (sub "name")
	           (text "!</p>\n\n<ul>\n  ")
	           (loop "page" "pages" ((text "\n  <li>\n    ")
                                         (sub "page")
                                         (text "\n  </li>\n  ")))
	           (text "\n</ul>"))
                 (orgify--parse (with-simple-template (orgify--tokenize))))))

(ert-deftest fixture-test-orgify--generate-code ()
  (let ((keywords (make-hash-table :test 'equal)))
    (puthash "name" "world" keywords)
    (puthash "pages" '("page-one" "page-two") keywords)

    (setq got (orgify--generate-code
                    (orgify--parse (with-simple-template (orgify--tokenize)))
                    keywords))

    (cl-loop for v in '((insert "<p>Hello ")
                        (insert "world")
                        (insert "!</p>\n\n<ul>\n  ")
                        (insert "\n  <li>\n    ")
                        (insert "page-one")
                        (insert "\n  </li>\n  ")
                        (insert "\n  <li>\n    ")
                        (insert "page-two")
                        (insert "\n  </li>\n  ")
                        (insert "\n</ul>"))
             for i from 0
             do
             (should (equal v (nth i got))))))

