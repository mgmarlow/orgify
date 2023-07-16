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

(defun orgify-lastcar (l)
  "Get last element of L."
  (car (cdr l)))

(defun orgify--extract-sub (source)
  "Extract substitution variable from SOURCE."
  (save-match-data
    (string-trim (substring source 2 (- (length source) 2)))))

(defun orgify--tokenize (input)
  "Collects INPUT into tokens for the Orgify template language."
  (let ((tokens '()) (cur-text "") (idx 0) expecting-each-end)
    (cl-flet ((purge-text ()
                (when (> (length cur-text) 0)
                  (push (list 'text cur-text) tokens)
                  (setq cur-text ""))))
      (while (< idx (length input))
        ;; Need to use (eq ... idx) to ensure regex is matching from
        ;; idx onwards. `string-start' in the regexp itself doesn't
        ;; work as I would expect, seemingly always matching the
        ;; entirety of input.
        (cond ((eq (string-match orgify--substitution-regexp input idx) idx)
               (purge-text)
               (push (list 'sub (orgify--extract-sub (match-string 0 input))) tokens)
               (setq idx (1- (match-end 0))))
              ((eq (string-match orgify--each-begin-regexp input idx) idx)
               ;; Expecting "#each foo in bar" only
               (when (save-match-data
                       (> (length (split-string (match-string 0 input))) 4))
                 (error "Invalid loop in template: %s" (match-string 0 input)))
               (purge-text)
               (push (list 'each-begin (match-string 0 input)) tokens)
               (setq expecting-each-end (match-string 0 input))
               (setq idx (1- (match-end 0))))
              ((eq (string-match orgify--each-end-regexp input idx) idx)
               (purge-text)
               (push (list 'each-end (match-string 0 input)) tokens)
               (setq expecting-each-end nil)
               (setq idx (1- (match-end 0))))
              (t
               (setq cur-text (concat cur-text (char-to-string (aref input idx))))))
        (cl-incf idx))

      ;; Failed to reach /each
      (when expecting-each-end
        (error "Expected end of #each: %s" expecting-each-end))

      (purge-text))
    (reverse tokens)))

;; TODO: Just codegen here
(defun orgify--parse (tokens &optional cur)
  "Parse TOKENS into an AST representing the template evaluation.

Searches tokens beginning at index 0. If CUR is not nil, start at
CUR instead."
  (let (root (cur (or cur 0)))
    (while (< cur (length tokens))
      (let ((token (nth cur tokens)))
        (cond ((or (eq 'text (car token))
                   (eq 'sub (car token)))
               (push token root))
              ((eq 'each-begin (car token))
               (let ((subexpr '()) (s-idx (1+ cur)))
                 (while (not (eq 'each-end (car (nth s-idx tokens))))
                   (unless (< s-idx (length tokens))
                     (error "Missing end-each token"))

                   (push (nth s-idx tokens) subexpr)
                   (cl-incf s-idx))
                 (push (list 'loop
                             (nth 1 (split-string (orgify-lastcar token) " "))
                             (nth 3 (split-string (orgify-lastcar token) " "))
                             (reverse subexpr))
                       root)
                 (setq cur s-idx)))))
      (setq cur (1+ cur)))
    (reverse root)))

(defun orgify--generate-code (ast env)
  "Generate code from AST, prepared for `eval'.

ENV is an alist of the org file environment."
  (let ((expressions '()))
    (dolist (val ast)
      (cond ((eq 'text (car val))
             (push `(insert ,(orgify-lastcar val)) expressions))
            ((eq 'sub (car val))
             (push `(insert ,(gethash (orgify-lastcar val) env)) expressions))
            ((eq 'loop (car val))
             (let ((subtree '()))
               (dolist (iter (gethash (nth 2 val) env))
                 (puthash (nth 1 val) iter env)
                 (push (orgify--generate-code (nth 3 val) env) subtree))
               ;; Unwind subtree for proper order and a flattened list
               ;; of expressions. There's probably an easier way.
               (dolist (l (reverse subtree))
                 (dolist (v l)
                   (push v expressions)))))))
    (reverse expressions)))

(defun orgify--execute (parsed)
  (dolist (expr parsed)
    (eval expr)))

(defun orgify--compile-and-exec (input env)
  (orgify--execute
   (orgify--generate-code
    (orgify--parse (orgify--tokenize input))
    env)))

(provide 'orgify-compiler)
