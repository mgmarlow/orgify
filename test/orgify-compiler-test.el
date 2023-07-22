(add-to-list 'load-path "../")
(require 'ert)
(require 'cl-lib)
(require 'orgify-compiler)

(ert-deftest test-lastcar ()
  (should (equal 'bar (orgify-lastcar '(foo bar)))))

(ert-deftest test-eval-string ()
  (should (equal "42" (orgify--eval-string "(* 2 21)" '())))
  (should (equal '("world") (orgify--eval-string "name" '((name "world")))))
  (should-error (orgify--eval-string "name" '())))

(defun compile-to-string (input &optional env)
  (with-temp-buffer
    (orgify--compile-and-exec input (or env '()))
    (buffer-string)))

(cl-defun assert-compiled (&key expect input env)
  (should (equal expect (compile-to-string input env))))

(ert-deftest test-missing-closing-brace ()
  (should-error (compile-to-string "{{ (+ 1 41)")))

(ert-deftest test-missing-opening-brace ()
  (should-error (compile-to-string "(+ 1 41) }}")))

(ert-deftest test-missing-closing-each ()
  (should-error (compile-to-string "#each page in pages\n{{ page }}")))

(ert-deftest test-missing-opening-each ()
  (should-error (compile-to-string "\n{{ page }}/each")))

(ert-deftest test-compile-simple-exprs ()
  (assert-compiled :expect "42"
                   :input "{{ (+ 1 41) }}")
  (assert-compiled :expect "42"
                   :input "{{(+ 1 41)}}")
  (assert-compiled :expect "<p>hello 42</p>"
                   :input "<p>{{ \"hello\" }} {{ (* 2 21) }}</p>"))

(ert-deftest test-compile-multiline-expr ()
  (assert-compiled :expect "42"
                   :input "{{\n(+ 1 41)\n}}")
  (assert-compiled :expect "42"
                   :input "{{\n(*\n (+ 1 20)\n 2)\n}}"))

(ert-deftest test-compile-with-env ()
  (assert-compiled :expect "Hello, world!"
                   :input "Hello, {{ name }}!"
                   :env '((name . "world")))

  (assert-compiled :expect "<ul>\n<li>page-one</li>\n\n<li>page-two</li>\n</ul>"
                   :input "<ul>#each page in pages\n<li>{{ page }}</li>\n/each</ul>"
                   :env '((pages . ("page-one" "page-two"))))

  (assert-compiled :expect "<ul>\n<li>page-one 2</li>\n\n<li>page-two 2</li>\n</ul>"
                   :input "<ul>#each page in pages\n<li>{{ page }} {{ (length pages) }}</li>\n/each</ul>"
                   :env '((pages . ("page-one" "page-two")))))
