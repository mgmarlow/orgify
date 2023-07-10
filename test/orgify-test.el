(add-to-list 'load-path "../")
(require 'ert)
(require 'orgify)

;;;; Template engine tests

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
    (insert-file-contents "test/simple-template.html")
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

;; This test is pretty ugly, should clean this up.
(ert-deftest fixture-test-orgify--templatize-page ()
  (let ((keywords (make-hash-table :test 'equal)))
    (puthash "name" "world" keywords)
    (puthash "pages" '("page-one" "page-two") keywords)
    (should (string=
             (with-temp-buffer
               (insert-file-contents "test/expected-simple-template.html")
               ;; Strip trailing newline from hand-edited file
               (string-trim (buffer-string)))
             (with-temp-buffer
               (dolist (expr (orgify--templatize-page (make-orgify-page
                                         :slug "foobar"
                                         :html "<p>great content here</p>"
                                         :layout "test/simple-template.html"
                                         :keywords keywords)))
                 (eval expr))
               (buffer-string))))))

;;;; Orgify tests

(ert-deftest test-orgify--build-page ()
  (let ((page (orgify--build-page "test/simple-page.org")))
    (should (string= "simple-page" (orgify-page-slug page)))
    (should (string= "<p>\nI'm a simple page.\n</p>\n" (orgify-page-html page)))
    (should (equal nil (orgify-page-layout page)))
    (should (equal "Hello world" (gethash "title" (orgify-page-keywords page))))))

(ert-deftest test-orgify--build-page-different-base-dir ()
  (let ((page (orgify--build-page "test/simple-page.org" (project-root (project-current)))))
    (should (string= "test/simple-page" (orgify-page-slug page)))
    (should (string= "<p>\nI'm a simple page.\n</p>\n" (orgify-page-html page)))
    (should (equal nil (orgify-page-layout page)))
    (should (equal "Hello world" (gethash "title" (orgify-page-keywords page))))))

(ert-deftest test-orgify--build-page-with-layout ()
  (let ((page (orgify--build-page "test/page-with-layout.org")))
    (should (string= "page-with-layout" (orgify-page-slug page)))
    (should (string= "<p>\nI'm a page with a layout.\n</p>\n" (orgify-page-html page)))
    (should (equal (expand-file-name "test/main.html" (project-root (project-current)))
                   (orgify-page-layout page)))
    (should (equal "Layout page!" (gethash "title" (orgify-page-keywords page))))))
