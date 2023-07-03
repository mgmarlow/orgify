(require 'ert)
(require 'orgify)

(ert-deftest test-orgify--parse-handlebars ()
  (should (string= "content" (orgify--parse-handlebars "{{ content }}")))
  (should (string= "content" (orgify--parse-handlebars "{{content }}")))
  (should (string= "content" (orgify--parse-handlebars "{{ content}}")))
  (should (string= "content" (orgify--parse-handlebars "{{content}}"))))

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
