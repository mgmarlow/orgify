(add-to-list 'load-path "../")
(require 'ert)
(require 'orgify)

;; Template engine tests

;; TODO: Introduce non-fixture tests

;; Fixture tests:
(defmacro with-simple-template (&rest body)
  `(with-temp-buffer
    (insert-file-contents "test/simple-template.html")
    (goto-char (point-min))
    ,@body))

(ert-deftest test-orgify--tokenize ()
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

(ert-deftest test-orgify--parse ()
  (should (equal '((text "<p>Hello ")
	           (sub "name")
	           (text "!</p>\n\n<ul>\n  ")
	           (loop "page" "pages" ((text "\n  <li>\n    ")
                                         (sub "page")
                                         (text "\n  </li>\n  ")))
	           (text "\n</ul>"))
                 (orgify--parse (with-simple-template (orgify--tokenize))))))

(ert-deftest test-orgify--generate-code ()
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
(ert-deftest test-orgify--templatize-page ()
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
               (print (buffer-string))
               (buffer-string))))))

;; Page struct
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
