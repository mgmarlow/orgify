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

(defun orgify--parse (tokens &optional start end)
  "Parse TOKENS into elisp expressions.

START and END optionally restrict parsing to particular indices
in TOKENS.  This is primarily used for recursively descending
through sub-expressions (like in #each).

The final expression tree is built into a lambda function that
takes a single env argument.  Right now, env is expected to be a
hash-table containing the page data/keywords.  Both the
substitution code and the looping code are coupled to hash-table
methods, which makes the final tree inflexible to arbitrary
expressions.  Instead, substitutions should call `eval', allowing
for any valid Emacs Lisp code for leaf nodes."
  (let (root (cur (or start 0)))
    (while (< cur (or end (length tokens)))
      (let ((token (nth cur tokens)))
        (cond ((eq 'text (car token))
               (push `(insert ,(orgify-lastcar token)) root))
              ((eq 'sub (car token))
               ;; TODO: allow for arbitrary expressions w/ `eval'.
               (push `(insert (gethash ,(orgify-lastcar token) env)) root))
              ((eq 'each-begin (car token))
               (let ((istart (1+ cur)) (iend (1+ cur)))
                 (while (not (eq 'each-end (car (nth iend tokens))))
                   (when (> iend (length tokens))
                     (error "Missing end-each token"))
                   (cl-incf iend))
                 (let ((item (nth 1 (split-string (orgify-lastcar token) " ")))
                       (collection (nth 3 (split-string (orgify-lastcar token) " "))))
                   (push `(cl-loop
                           for iter in (gethash ,collection env)
                           do (progn
                                ;; This puthash is ugly! Instead, leaf
                                ;; nodes should be eval expressions so
                                ;; iter can just be passed through.
                                (puthash ,item iter env)
                                (,(orgify--parse tokens istart iend) env)))
                         root))
                 (setq cur iend)))))
      (setq cur (1+ cur)))
    `(lambda (env) ,@(reverse root))))

(defun orgify--compile-and-exec (input env)
  (funcall
   (orgify--parse (orgify--tokenize input))
   env))

(provide 'orgify-compiler)
