(add-to-list 'load-path "../")
(require 'ert)
(require 'orgify)

(ert-deftest test-build-page ()
  (let ((page (orgify--build-page "test/fixtures/simple-page.org")))
    (should (string= "simple-page" (orgify-page-slug page)))
    (should (string= "<p>\nI'm a simple page.\n</p>\n" (orgify-page-html page)))
    (should (equal nil (orgify-page-layout page)))
    (should (equal "Hello world" (alist-get 'title (orgify-page-keywords page))))))

(ert-deftest test-build-page-different-base-dir ()
  (let ((page (orgify--build-page "test/fixtures/simple-page.org" (project-root (project-current)))))
    (should (string= "test/fixtures/simple-page" (orgify-page-slug page)))
    (should (string= "<p>\nI'm a simple page.\n</p>\n" (orgify-page-html page)))
    (should (equal nil (orgify-page-layout page)))
    (should (equal "Hello world" (alist-get 'title (orgify-page-keywords page))))))

(ert-deftest test-build-page-with-layout ()
  (let ((page (orgify--build-page "test/fixtures/page-with-layout.org")))
    (should (string= "page-with-layout" (orgify-page-slug page)))
    (should (string= "<p>\nI'm a page with a layout.\n</p>\n" (orgify-page-html page)))
    (should (equal (expand-file-name "test/fixtures/main.html" (project-root (project-current)))
                   (orgify-page-layout page)))
    (should (equal "Layout page!" (alist-get 'title (orgify-page-keywords page))))))
