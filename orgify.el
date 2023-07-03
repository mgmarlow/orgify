;;; orgify.el --- A static site generator that understands org -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: tools
;; URL: https://git.sr.ht/~mgmarlow/orgify
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple static site generator that understands org. Orgify
;; provides a little extra structure to ox-html, adding support for
;; HTML layouts with handlebars-like expressions as well as other
;; tools for building a static website. Use `orgify-build' to generate
;; your static site via a script, making it easy to integrate with a
;; build pipeline.

;;; Code:

(require 'cl-lib)
(require 'ox-html)

(defvar-local orgify--default-template
    "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <meta http-equiv=\"X-UA-Compatible\" content=\"ie=edge\">
  <title>{{ title }}</title>
  <link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\">
  <link rel=\"icon\" type=\"image/x-icon\" href=\"/favicon.ico\">
</head>
<body>
  {{ content }}
</body>
</html>"
  "Layout template with handlebar expressions used by `orgify-build'.")

(defun orgify--parse-org (input-data)
  "Given org INPUT-DATA as a string, produce HTML."
  (let (html
        (keywords (make-hash-table :test 'equal)))
    ;; Override ox HTML exports to produce bare-minimum HTML contents.
    (advice-add
     #'org-html-template :override
     (lambda (contents _i) (setq html contents)))

    (advice-add
     #'org-html-keyword :before
     (lambda (keyword _c _i)
       (puthash (downcase (org-element-property :key keyword))
                (org-element-property :value keyword)
                keywords)))

    (with-temp-buffer
      (insert input-data)
      (org-html-export-as-html))

    (cl-values html keywords)))

(defun orgify--parse-handlebars (handlebars)
  "Return inner expression from HANDLEBARS as a string."
  (string-trim (substring handlebars 2 (- (length handlebars) 2))))

(cl-defun orgify-build (&key base-dir out-dir static-dir)
  "Build org files into a static website.

Orgify supports a number of optional keyword arguments:

* BASE-DIR is the location of your org files. Defaults to the root
directory.

* STATIC-DIR is a directory of static files whose contents are
copied into OUT-DIR. Defaults to public/.

* OUT-DIR is the build destination of your site. Defaults to
  output/."
  (let* ((base-dir (or base-dir "."))
         (out-dir (or out-dir "output/"))
         (static-dir (or static-dir "public/"))
         (org-files (directory-files-recursively base-dir ".*\.org")))
    ;; Make output directory.
    (unless (file-exists-p out-dir)
      (make-directory out-dir))
    ;; Copy over static assets.
    (when (file-exists-p static-dir)
      (copy-directory static-dir out-dir nil nil 'copy-contents))
    ;; Convert org->HTML
    (dolist (file org-files)
      ;; Relative file name so files are in the same directory format
      ;; as the base dir (avoid repeating the base-dir directory under
      ;; out-dir).
      (let* ((rel (file-relative-name file base-dir))
             (destination-file (expand-file-name
                                (concat (file-name-sans-extension rel) ".html")
                                out-dir))
             (input-data (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
        (save-excursion
          (make-empty-file destination-file)
          (with-temp-file destination-file
            (cl-multiple-value-bind (content keywords)
                (orgify--parse-org input-data)

              ;; Insert layout first.
              (if (gethash "layout" keywords)
                  (insert-file-contents
                   (expand-file-name (gethash "layout" keywords) base-dir))
                (insert orgify--default-template))
              (goto-char (point-min))

              ;; Replace handlebar expressions.
              (while (re-search-forward "{{[ ]*[a-z]*[ ]*}}" nil t)
                ;; It's important to preserve match data since we're
                ;; calling substring to parse out the template
                ;; content (which will mutate).
                (let ((expr (save-match-data
                              (and (match-string 0)
                                   (orgify--parse-handlebars (match-string 0))))))
                  (cond ((string= expr "content") (replace-match content))
                        (t
                         (unless (gethash expr keywords)
                           (error (concat "Unrecognized expression: " (match-string 0))))
                         (replace-match (gethash expr keywords)))))))))))))

(provide 'orgify)
;;; orgify.el ends here
