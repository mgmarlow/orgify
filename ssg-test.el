(require 'ert)
(require 'ssg)

(ert-deftest test-ssg--parse-handlebars ()
  (should (string= "content" (ssg--parse-handlebars "{{ content }}")))
  (should (string= "content" (ssg--parse-handlebars "{{content }}")))
  (should (string= "content" (ssg--parse-handlebars "{{ content}}")))
  (should (string= "content" (ssg--parse-handlebars "{{content}}"))))
