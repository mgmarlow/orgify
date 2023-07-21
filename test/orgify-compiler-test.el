(add-to-list 'load-path "../")
(require 'ert)
(require 'cl-lib)
(require 'orgify-compiler)

;;; Tokenizer

(ert-deftest test-tokenize-exprs ()
  (should (equal '((expr "contents")) (orgify--tokenize "{{ contents }}")))
  (should (equal '((expr "contents")) (orgify--tokenize "{{contents}}")))
  (should (equal '((expr "contents")) (orgify--tokenize "{{ contents}}")))
  (should (equal '((expr "contents")) (orgify--tokenize "{{contents }}"))))

(ert-deftest test-tokenize-two-exprs ()
  (should (equal '((expr "foo") (expr "bar")) (orgify--tokenize "{{ foo }}{{ bar }}"))))

(ert-deftest test-tokenize-exprs-with-surrounding-text ()
  (should (equal '((text "foo") (expr "contents") (text "bar")) (orgify--tokenize "foo{{ contents }}bar")))
  (should (equal '((text "foo") (expr "contents")) (orgify--tokenize "foo{{ contents }}")))
  (should (equal '((expr "contents") (text "bar")) (orgify--tokenize "{{ contents }}bar"))))

(ert-deftest test-tokenize-exprs-with-newlines ()
  (should (equal '((text "{{\ncontents\n}}")) (orgify--tokenize "{{\ncontents\n}}"))))

(ert-deftest test-tokenize-exprs-with-single-brace ()
  (should (equal '((text "{ contents }")) (orgify--tokenize "{ contents }"))))

(ert-deftest test-tokenize-exprs-with-missing-end-brace ()
  (should (equal '((text "{{ contents")) (orgify--tokenize "{{ contents"))))

(ert-deftest test-tokenize-exprs-with-missing-beginning-brace ()
  (should (equal '((text "contents }}")) (orgify--tokenize "contents }}"))))

(ert-deftest test-tokenize-loops ()
  (should (equal '((each-begin "#each foo in foobar")
                   (text "\n")
                   (expr "foo")
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
  (should (equal '(lambda (env) (insert (orgify--eval-string "(+ 1 3)" env))) (orgify--parse '((expr "(+ 1 3)"))))))

(ert-deftest test-parsing-substitutions ()
  (should (equal '(lambda (env) (insert (orgify--eval-string "foobar" env))) (orgify--parse '((expr "foobar"))))))

(ert-deftest test-parsing-loops ()
  (should (equal '(lambda (env) (cl-loop for iter in (orgify--eval-string "foobar" env)
                                         do (progn
                                              (push (cons 'foo iter) env)
                                              ((lambda (env)
                                                 (insert "\n")
                                                 (insert (orgify--eval-string "foo" env))
                                                 (insert "\n"))
                                               env))))
                 (orgify--parse '((each-begin "#each foo in foobar")
                                  (text "\n")
                                  (expr "foo")
                                  (text "\n")
                                  (each-end "\end"))))))

;;; Fixture tests

(cl-defun assert-compiled (&key expect input env)
  (should (equal
           expect
           (with-temp-buffer
             (orgify--compile-and-exec input (or env '()))
             (buffer-string)))))

(ert-deftest test-compile-simple-exprs ()
  (assert-compiled :expect "42"
                   :input "{{ (+ 1 41) }}")
  (assert-compiled :expect "<p>hello 42</p>"
                   :input "<p>{{ \"hello\" }} {{ (* 2 21) }}</p>"))

(ert-deftest test-compile-with-env ()
  (assert-compiled :expect "Hello, world!"
                   :input "Hello, {{ name }}!"
                   :env '((name . "world")))

  (assert-compiled :expect "<ul>\n<li>page-one</li>\n\n<li>page-two</li>\n"
                   :input "<ul>#each page in pages\n<li>{{ page }}</li>\n/each</ul>"
                   :env '((pages . ("page-one" "page-two"))))

  (assert-compiled :expect "<ul>\n<li>page-one 2</li>\n\n<li>page-two 2</li>\n"
                   :input "<ul>#each page in pages\n<li>{{ page }} {{ (length pages) }}</li>\n/each</ul>"
                   :env '((pages . ("page-one" "page-two")))))
