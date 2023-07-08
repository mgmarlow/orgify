(add-to-list 'load-path "../")
(require 'ert)
(require 'orgify)

(ert-deftest test-orgify--parse-handlebars ()
  (should (string= "content" (orgify--parse-handlebars "{{ content }}")))
  (should (string= "content" (orgify--parse-handlebars "{{content }}")))
  (should (string= "content" (orgify--parse-handlebars "{{ content}}")))
  (should (string= "content" (orgify--parse-handlebars "{{content}}"))))

(ert-deftest test-orgify--parse-loop ()
  (should (equal (cons "page" "pages") (orgify--parse-loop "#each page in pages"))))

(defun templatize (template content &optional keywords)
  (with-temp-buffer
    (insert template)
    (goto-char (point-min))
    (orgify--search-and-replace-handlebars
     content
     (or keywords (make-hash-table)))
    (buffer-string)))

(ert-deftest test-orgify--search-and-replace-content ()
  (should (string=
           "foobar"
           (templatize "{{ content }}" "foobar")))
  (should (string=
           "<main>foobar</main>"
           (templatize "<main>{{ content }}</main>" "foobar"))))

(ert-deftest test-orgify--search-and-replace-keywords ()
  (should (string=
           "hello world"
           (templatize
            "{{ content }} {{ text }}"
            "hello"
            (let ((keywords (make-hash-table :test 'equal)))
              (puthash "text" "world" keywords)
              keywords)))))

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
