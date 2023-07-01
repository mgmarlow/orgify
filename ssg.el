;;; ssg.el --- A static site generator built in Emacs Lisp  -*- lexical-binding: t; -*-

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
</head>
<body>"
body
"</body>
</html>"))

(defun ssg--parse-org (input-data)
  "TODO"
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

(cl-defun ssg-config (&key base-dir out-dir)
  "TODO"
  (let ((org-files (directory-files-recursively base-dir ".*\.org"))
        (out-dir (expand-file-name (or out-dir "output/") base-dir)))
    (cl-loop
     for file in org-files
     do
     (let ((destination-file (expand-file-name
                              (replace-regexp-in-string ".org" ".html" file)
                              out-dir))
           (input-data (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))))
       (save-excursion
         (unless (file-exists-p out-dir)
           (make-directory out-dir))
         (make-empty-file destination-file)
         (with-temp-file destination-file
           (insert (ssg--wrap-in-layout
                    (ssg--parse-org input-data)))))))))

(provide 'ssg)
;;; ssg.el ends here
