;;; orgify-compiler.el --- Compiler for Orgify template language  -*- lexical-binding: t; -*-

;;; Commentary:

;; Compiler for the Orgify template language.

;;; Code:

(require 'cl-lib)

(defvar-local orgify--substitution-regexp
    (rx "{{"
        (zero-or-more blank)
        (zero-or-more nonl)
        (zero-or-more blank)
        "}}"))

(defvar-local orgify--each-begin-regexp
    (rx "#each" (zero-or-more nonl)))

(defvar-local orgify--each-end-regexp
    (rx "/each" (zero-or-more nonl)))

(defun orgify-safe-trim (str)
  "Trim STR while preserving match data."
  (save-match-data (string-trim str)))

(defun orgify-lastcar (l)
  "Get last element of L."
  (car (cdr l)))

(defun orgify--tokenize (&optional buffer)
  "Break current buffer contents into a list of tokens.

If BUFFER is not nil, use that buffer instead."
  (let ((tokens '()) (cur-text ""))
    (cl-flet ((purge-text ()
                (when (> (length cur-text) 0)
                  (push (list 'text cur-text) tokens)
                  (setq cur-text ""))))
      (while (< (point) (buffer-size buffer))
        (cond ((looking-at orgify--substitution-regexp)
               (purge-text)
               (push (list 'sub (orgify-safe-trim (match-string 0))) tokens)
               (goto-char (1- (match-end 0))))
              ((looking-at orgify--each-begin-regexp)
               (purge-text)
               (push (list 'each-begin (match-string 0)) tokens)
               (goto-char (1- (match-end 0))))
              ((looking-at orgify--each-end-regexp)
               (purge-text)
               (push (list 'each-end (match-string 0)) tokens)
               (goto-char (1- (match-end 0))))
              (t (setq cur-text (concat cur-text (char-to-string (char-after))))))
        (forward-char))
      (purge-text))
    (reverse tokens)))

(defun orgify--parse (tokens &optional cur)
  "Parse TOKENS into an AST representing the template evaluation.

Searches tokens beginning at index 0. If CUR is not nil, start at
CUR instead."
  (cl-block parser
    (let (root (cur (or cur 0)))
      (while (< cur (length tokens))
        (let ((token (nth cur tokens)))
          (cond ((eq 'text (car token))
                 (push (list 'text (orgify-lastcar token)) root))
                ((eq 'sub (car token))
                 (push (list 'sub (nth 1 (split-string (orgify-lastcar token) " ")))
                       root))
                ((eq 'each-begin (car token))
                 (let ((subtree (orgify--parse tokens (1+ cur))))
                   (push (list 'loop
                               (nth 1 (split-string (orgify-lastcar token) " "))
                               (nth 3 (split-string (orgify-lastcar token) " "))
                               (car subtree))
                         root)
                   (setq cur (cdr subtree))))
                ((eq 'each-end (car token))
                 (cl-return-from parser (cons (reverse root) cur)))))
        (setq cur (1+ cur)))
      (reverse root))))

(defun orgify--generate-code (ast keywords)
  "Generate code from AST, prepared for `eval'.

KEYWORDS is a hash-table of org file keywords.

COLLECTIONS is a hash-table of collection names to org file
keywords."
  (let ((expressions '()))
    (dolist (val ast)
      (cond ((eq 'text (car val))
             (push `(insert ,(orgify-lastcar val)) expressions))
            ((eq 'sub (car val))
             (push `(insert ,(gethash (orgify-lastcar val) keywords)) expressions))
            ((eq 'loop (car val))
             (let ((subtree '()))
               (dolist (iter (gethash (nth 2 val) keywords))
                 (puthash (nth 1 val) iter keywords)
                 (push (orgify--generate-code (nth 3 val) keywords) subtree))
               ;; Unwind subtree for proper order and a flattened list
               ;; of expressions. There's probably an easier way.
               (dolist (l (reverse subtree))
                 (dolist (v l)
                   (push v expressions)))))))
    (reverse expressions)))

;; TODO: Need a better interface that actually executes the code.
(defun orgify--compile (state &optional buffer)
  (orgify--generate-code
   (orgify--parse (orgify--tokenize buffer))
   state))

(provide 'orgify-compiler)
