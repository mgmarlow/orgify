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

;; Might swap out org-html-style-default for (org-html-head-include-default-style . nil)

(cl-defun ssg-config (&key base-dir out-dir)
  "todo"
  (let ((org-files (directory-files-recursively base-dir ".*\.org"))
        (out-dir (expand-file-name (or out-dir "output/") base-dir))
        (org-html-style-default "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\">"))
    (cl-loop
     for file in org-files
     do
     (let ((destination-file (expand-file-name
                              (replace-regexp-in-string ".org" ".html" file)
                              out-dir)))
       (save-excursion
         (unless (file-exists-p out-dir)
           (make-directory out-dir))
         (with-current-buffer (find-file file)
             (save-restriction
               (widen)
               (make-empty-file destination-file)
               (org-export-to-file 'html destination-file))))))))

(provide 'ssg)
;;; ssg.el ends here
