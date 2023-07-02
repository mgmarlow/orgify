;;; ssg.el --- A static site generator that understands org -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <mgmarlow@Grahams-Mac-mini.local>
;; Keywords: tools

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

;; 

;;; Code:

(require 'cl-lib)
(require 'ox-html)

;; TODO: Actual HTML templating
(defun ssg--wrap-in-layout (body)
  (concat
"<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <meta http-equiv=\"X-UA-Compatible\" content=\"ie=edge\">
    <title>My cool ssg.el site</title>
    <link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\">
    <link rel=\"icon\" type=\"image/x-icon\" href=\"/favicon.ico\">
    <link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"/favicon-32x32.png\">
    <link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"/favicon-16x16.png\">
</head>
<body>"
body
"</body>
</html>"))

(defun ssg--parse-org (input-data)
  "Given org INPUT-DATA as a string, produce HTML."
  (let (html)
    ;; Override ox HTML exports to produce bare-minimum HTML contents.
    (advice-add
     #'org-html-template :override
     (lambda (contents _i) (setq html contents)))

    ;; (advice-add
    ;;  #'org-html-keyword :before
    ;;  (lambda (keyword _c _i) 'todo))

    (with-temp-buffer
      (insert input-data)
      (org-html-export-as-html))

    html))

(cl-defun ssg-build (&key base-dir out-dir rel-static-dir)
  "TODO

REL-STATIC-DIR is a directory of static files that are copied
into OUT-DIR, relative to BASE-DIR."
  (let* ((base-dir (or base-dir "."))
         (out-dir (or out-dir "output/"))
         (rel-static-dir (or rel-static-dir "public/"))
         (source-static-dir (expand-file-name rel-static-dir base-dir))
         (org-files (directory-files-recursively base-dir ".*\.org")))
    ;; Make output directory.
    (unless (file-exists-p out-dir)
      (make-directory out-dir))
    ;; Copy over static assets.
    (when (file-exists-p source-static-dir)
      (copy-directory source-static-dir out-dir nil nil 'copy-contents))
    ;; org->HTML
    (dolist (file org-files)
      ;; Relative file name so files are in the same directory format as the
      ;; base dir (avoid repeating the base-dir directory under out-dir).
      (let* ((rel (file-relative-name file base-dir))
             (destination-file (expand-file-name
                                (concat (file-name-sans-extension rel) ".html")
                                out-dir))
             (input-data (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
        (save-excursion
          ;; Convert individual files to HTML and write them to output.
          (make-empty-file destination-file)
          (with-temp-file destination-file
            (insert (ssg--wrap-in-layout
                     (ssg--parse-org input-data)))))))))

(provide 'ssg)
;;; ssg.el ends here
