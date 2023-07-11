(add-to-list 'load-path "../")
(require 'ert)
(require 'orgify)

;; This test is pretty ugly, should clean this up.
(ert-deftest fixture-test-orgify--templatize-page ()
  (let ((keywords (make-hash-table :test 'equal)))
    (puthash "name" "world" keywords)
    (puthash "pages" '("page-one" "page-two") keywords)
    (should (string=
             (with-temp-buffer
               (insert-file-contents "test/fixtures/expected-simple-template.html")
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

(ert-deftest test-orgify--build-page ()
  (let ((page (orgify--build-page "test/fixtures/simple-page.org")))
    (should (string= "simple-page" (orgify-page-slug page)))
    (should (string= "<p>\nI'm a simple page.\n</p>\n" (orgify-page-html page)))
    (should (equal nil (orgify-page-layout page)))
    (should (equal "Hello world" (gethash "title" (orgify-page-keywords page))))))

(ert-deftest test-orgify--build-page-different-base-dir ()
  (let ((page (orgify--build-page "test/fixtures/simple-page.org" (project-root (project-current)))))
    (should (string= "test/simple-page" (orgify-page-slug page)))
    (should (string= "<p>\nI'm a simple page.\n</p>\n" (orgify-page-html page)))
    (should (equal nil (orgify-page-layout page)))
    (should (equal "Hello world" (gethash "title" (orgify-page-keywords page))))))

(ert-deftest test-orgify--build-page-with-layout ()
  (let ((page (orgify--build-page "test/fixtures/page-with-layout.org")))
    (should (string= "page-with-layout" (orgify-page-slug page)))
    (should (string= "<p>\nI'm a page with a layout.\n</p>\n" (orgify-page-html page)))
    (should (equal (expand-file-name "test/main.html" (project-root (project-current)))
                   (orgify-page-layout page)))
    (should (equal "Layout page!" (gethash "title" (orgify-page-keywords page))))))
