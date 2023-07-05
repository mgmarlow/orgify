;;; orgify.el --- A static site generator that understands org -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: tools
;; URL: https://git.sr.ht/~mgmarlow/orgify
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;; A simple static site generator that understands org.  Orgify
;; provides a little extra structure to ox-html, adding support for
;; HTML layouts with handlebars-like expressions as well as other
;; tools for building a static website.  Use `orgify-build' to
;; generate your static site via a script, or `orgify-build-project'
;; to generate one interactively.

;;; Code:

(require 'cl-lib)
(require 'ox-html)
(require 'project)

(defvar-local orgify--default-template
    "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <meta http-equiv=\"X-UA-Compatible\" content=\"ie=edge\">
  <title>my cool Orgify site</title>
  <link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\">
  <link rel=\"icon\" type=\"image/x-icon\" href=\"/favicon.ico\">
</head>
<body>
  {{ content }}
</body>
</html>"
  "Layout template with handlebar expressions used by `orgify-build'.")

(cl-defstruct orgify-page slug html layout keywords)

;; This function stores the converted HTML and HTML layout as strings
;; on the page struct. This may prove to be inefficient, so it's
;; likely the actual HTML data will be moved elsewhere in the
;; future.
(defun orgify--build-page (org-file-name &optional base-dir)
  "Create an `orgify-page' from ORG-FILE-NAME.

Parses org content from ORG-FILE-NAME to HTML via ox-html.  Org
file keywords are accesible via `orgify-page-keywords'.

BASE-DIR is an optional argument that resolves filepaths relative
to BASE-DIR rather than their default."
  (let* (html
         ;; Relative file name so files are in the same directory
         ;; format as the base dir (avoid repeating the base-dir
         ;; directory under out-dir).
         (base-dir (or base-dir (file-name-directory org-file-name)))
         (slug (file-name-sans-extension
                (file-relative-name org-file-name base-dir)))
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
      (insert-file-contents org-file-name)
      (org-html-export-as-html))

    (make-orgify-page
     :slug slug
     :html html
     :layout (and (gethash "layout" keywords)
                  (expand-file-name (gethash "layout" keywords) base-dir))
     :keywords keywords)))

(defun orgify--parse-handlebars (handlebars)
  "Return inner expression from HANDLEBARS as a string."
  (string-trim (substring handlebars 2 (- (length handlebars) 2))))

(defun orgify--search-and-replace-handlebars (content keywords)
  "Replace handlebars expressions in current buffer.

CONTENT is HTML content that is substituted for the {{ content }}
expression.

KEYWORDS is a hash-table of arbitrary key-value pairs.  An
expression {{ key }} is substituted with value, if that key
exists in KEYWORDS.  Otherwise, an error is thrown."
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
             (replace-match (gethash expr keywords)))))))

;; Note that the layout parsing is repeated for every page, regardless
;; of whether or not that layout has already been read from the file
;; system. It's likely layouts will be moved to their own hash-table
;; in the future.
(defun orgify--render-page (page out-dir)
  "Write PAGE to OUT-DIR.

Replaces handlebar expressions in the page layout with the page
content and keywords."
  (let ((destination (expand-file-name
                      (concat (orgify-page-slug page) ".html")
                      out-dir)))
    (make-empty-file destination)
    (with-temp-file destination
      ;; Initial layout
      (if (orgify-page-layout page)
          (insert-file-contents (orgify-page-layout page))
        (insert orgify--default-template))
      (goto-char (point-min))
      ;; Template expressions
      (orgify--search-and-replace-handlebars
       (orgify-page-html page)
       (orgify-page-keywords page)))))

(cl-defun orgify-build (&key base-dir out-dir static-dir)
  "Build org files into a static website.

BASE-DIR is the location of your org files.  Defaults to the root
directory.

STATIC-DIR is a directory of static files whose contents are
copied into OUT-DIR.  Defaults to public/.

OUT-DIR is the build destination of your site.  Defaults to
  output/."
  (let* ((base-dir (or base-dir "."))
         (out-dir (or out-dir "output/"))
         (static-dir (or static-dir "public/")))
    ;; Make output directory.
    (when (file-exists-p out-dir)
      (delete-directory out-dir t))
    (make-directory out-dir)
    ;; Copy over static assets.
    (when (file-exists-p static-dir)
      (copy-directory static-dir out-dir nil nil 'copy-contents))
    ;; Build pages ahead-of-time to collect information from page
    ;; keywords, then render them to the output directory.
    (dolist (page (cl-loop
                   for file in (directory-files-recursively base-dir ".*\.org")
                   collect (orgify--build-page file base-dir)))
      (orgify--render-page page out-dir))))

;;;###autoload
(defun orgify-build-project ()
  "Builds current project with `orgify-build'.

Assumes the default Orgify project structure, using
`project-root' as the base directory."
  (interactive)
  (save-window-excursion
    (unless (project-current)
      (error "File %s is not part of any known project"
             (buffer-file-name (buffer-base-buffer))))
    (let ((base-dir (project-root (project-current))))
      (orgify-build :base-dir base-dir
                    :static-dir (expand-file-name "public/" base-dir)
                    :out-dir (expand-file-name "output/" base-dir)))))

(provide 'orgify)
;;; orgify.el ends here
