(require 'ert)
(require 'orgify)

(ert-deftest test-orgify--parse-handlebars ()
  (should (string= "content" (orgify--parse-handlebars "{{ content }}")))
  (should (string= "content" (orgify--parse-handlebars "{{content }}")))
  (should (string= "content" (orgify--parse-handlebars "{{ content}}")))
  (should (string= "content" (orgify--parse-handlebars "{{content}}"))))
