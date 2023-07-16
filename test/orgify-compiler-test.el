(add-to-list 'load-path "../")
(require 'ert)
(require 'orgify-compiler)

;;; Tokenizer

(ert-deftest test-tokenize-subs()
  (should (equal '((sub "contents")) (orgify--tokenize "{{ contents }}")))
  (should (equal '((sub "contents")) (orgify--tokenize "{{contents}}")))
  (should (equal '((sub "contents")) (orgify--tokenize "{{ contents}}")))
  (should (equal '((sub "contents")) (orgify--tokenize "{{contents }}"))))

(ert-deftest test-tokenize-subs-with-surrounding-text()
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
  (should (equal '((text "foobar")) (orgify--parse '((text "foobar"))))))

(ert-deftest test-parsing-substitutions ()
  (should (equal '((sub "foobar")) (orgify--parse '((sub "foobar"))))))

(ert-deftest test-parsing-loops ()
  (should (equal '((loop "foo" "foobar" ((text "\n")
                                         (sub "foo")
                                         (text "\n"))))
                 (orgify--parse '((each-begin "#each foo in foobar")
                                  (text "\n")
                                  (sub "foo")
                                  (text "\n")
                                  (each-end "\end"))))))

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
  :expected-result :failed
  (let ((keywords (make-hash-table :test 'equal)))
    (should-error (orgify--generate-code '((sub "nocontent")) keywords))))

(ert-deftest test-codegen-loop ()
  (let ((keywords (make-hash-table :test 'equal)))
    (puthash "foobar" '("one" "two" "three") keywords)
    (should (equal '((insert "one")
                     (insert "two")
                     (insert "three"))
                   (orgify--generate-code '((loop "foo" "foobar" ((sub "foo")))) keywords)))))

;; TODO: Need error handling
(ert-deftest test-codegen-loop-keyword-missing ()
  :expected-result :failed
  (let ((keywords (make-hash-table :test 'equal)))
    (should-error (orgify--generate-code '((loop "foo" "foobar" ((sub "foo")))) keywords))))

;;; Fixture tests

(defmacro simple-template ()
  (with-temp-buffer
    (insert-file-contents "test/fixtures/simple-template.html")
    (buffer-string)))

(ert-deftest test-simple-template-fixture ()
  (let ((env (make-hash-table :test 'equal))
        (input (with-temp-buffer
                 (insert-file-contents "fixtures/simple-template.html")
                 (buffer-string))))
    (puthash "name" "world" env)
    (puthash "pages" '("page-one" "page-two") env)

    (should
     (equal
      "<p>Hello world!</p>\n\n<ul>\n  \n  <li>\n    page-one\n  </li>\n  \n  <li>\n    page-two\n  </li>\n  \n</ul>\n"
      (with-temp-buffer
        (orgify--compile-and-exec input env)
        (buffer-string))))))

