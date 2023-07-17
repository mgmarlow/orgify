(add-to-list 'load-path "../")
(require 'ert)
(require 'orgify-compiler)

;;; Tokenizer

(ert-deftest test-tokenize-subs ()
  (should (equal '((sub "contents")) (orgify--tokenize "{{ contents }}")))
  (should (equal '((sub "contents")) (orgify--tokenize "{{contents}}")))
  (should (equal '((sub "contents")) (orgify--tokenize "{{ contents}}")))
  (should (equal '((sub "contents")) (orgify--tokenize "{{contents }}"))))

(ert-deftest test-tokenize-two-subs ()
  (should (equal '((sub "foo") (sub "bar")) (orgify--tokenize "{{ foo }}{{ bar }}"))))

(ert-deftest test-tokenize-subs-with-surrounding-text ()
  (should (equal '((text "foo") (sub "contents") (text "bar")) (orgify--tokenize "foo{{ contents }}bar")))
  (should (equal '((text "foo") (sub "contents")) (orgify--tokenize "foo{{ contents }}")))
  (should (equal '((sub "contents") (text "bar")) (orgify--tokenize "{{ contents }}bar"))))

(ert-deftest test-tokenize-subs-with-newlines ()
  (should (equal '((text "{{\ncontents\n}}")) (orgify--tokenize "{{\ncontents\n}}"))))

(ert-deftest test-tokenize-subs-with-single-brace ()
  (should (equal '((text "{ contents }")) (orgify--tokenize "{ contents }"))))

(ert-deftest test-tokenize-subs-with-missing-end-brace ()
  (should (equal '((text "{{ contents")) (orgify--tokenize "{{ contents"))))

(ert-deftest test-tokenize-subs-with-missing-beginning-brace ()
  (should (equal '((text "contents }}")) (orgify--tokenize "contents }}"))))

(ert-deftest test-tokenize-loops ()
  (should (equal '((each-begin "#each foo in foobar")
                   (text "\n")
                   (sub "foo")
                   (text "\n")
                   (each-end "/each"))
                 (orgify--tokenize "#each foo in foobar\n{{ foo }}\n/each"))))

(ert-deftest test-tokenize-loop-same-line-error ()
  (should-error (orgify--tokenize "#each foo in foobar {{ foo }} /each")))

(ert-deftest test-tokenize-loop-missing-end-error ()
  (should-error (orgify--tokenize "#each foo in foobar\n{{ foo }}")))

(ert-deftest test-tokenize-multiline-text ()
  (should (equal '((text "this\nis\a\test\n")) (orgify--tokenize "this\nis\a\test\n"))))

;;; Parser

(ert-deftest test-parsing-text ()
  (should (equal '(lambda (env) (insert "foobar")) (orgify--parse '((text "foobar"))))))

(ert-deftest test-parsing-expr ()
  (should (equal '(lambda (env) (insert (orgify--eval-string "(+ 1 3)" env))) (orgify--parse '((sub "(+ 1 3)"))))))

(ert-deftest test-parsing-substitutions ()
  (should (equal '(lambda (env) (insert (orgify--eval-string "foobar" env))) (orgify--parse '((sub "foobar"))))))

(ert-deftest test-parsing-loops ()
  (should (equal '(lambda (env) (cl-loop for iter in (alist-get 'foobar env)
                                         do (progn
                                              (push (cons 'foo iter) env)
                                              ((lambda (env)
                                                 (insert "\n")
                                                 (insert (orgify--eval-string "foo" env))
                                                 (insert "\n"))
                                               env))))
                 (orgify--parse '((each-begin "#each foo in foobar")
                                  (text "\n")
                                  (sub "foo")
                                  (text "\n")
                                  (each-end "\end"))))))

;;; Fixture tests

(defun simple-template ()
  (with-temp-buffer
    (insert-file-contents "test/fixtures/simple-template.html")
    (buffer-string)))

(ert-deftest test-simple-template-fixture ()
  (let ((env-alist '((name . "world") (pages . ("page-one" "page-two"))))
        (input (with-temp-buffer
                 (insert-file-contents "test/fixtures/simple-template.html")
                 (buffer-string))))
    (should
     (equal
      "<p>Hello world!</p>\n\n<ul>\n  \n  <li>\n    page-one\n  </li>\n  \n  <li>\n    page-two\n  </li>\n  \n</ul>\n"
      (with-temp-buffer
        (orgify--compile-and-exec input env-alist)
        (buffer-string))))))

(ert-deftest test-compile-expr ()
  (let ((env-alist '())
        (input "<p>{{ (+ 1 2) }}</p>"))
    (should
     (equal
      "<p>3</p>"
      (with-temp-buffer
        (orgify--compile-and-exec input env-alist)
        (buffer-string))))))

(ert-deftest test-compile-exprs ()
  (let ((env-alist '())
        (input "<p>{{ \"hello\" }} {{ (+ 1 2) }}</p>"))
    (should
     (equal
      "<p>hello 3</p>"
      (with-temp-buffer
        (orgify--compile-and-exec input env-alist)
        (buffer-string))))))

