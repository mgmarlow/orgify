;;; orgify-compiler.el --- Compiler for Orgify template language  -*- lexical-binding: t; -*-

;;; Commentary:

;; Compiler for the Orgify template language.

;;; Code:

(require 'cl-lib)

(defvar-local orgify--obrace-regexp "{{")

(defvar-local orgify--cbrace-regexp "}}")

(defvar-local orgify--oeach-regexp (rx "#each" (zero-or-more nonl)))

;; Perhaps change up the syntax here to avoid issue with "every/each"
;; or "when/if".
(defvar-local orgify--ceach-regexp (rx "/each" (zero-or-more nonl)))

(defun orgify-lastcar (l)
  "Get last element of L."
  (car (cdr l)))

(defun orgify--extract-sub (source)
  "Extract substitution variable from SOURCE."
  (save-match-data
    (string-trim (substring source 2 (- (length source) 2)))))

(defun orgify--tokenize (input)
  "Collects INPUT into tokens for the Orgify template language."
  (let ((tokens '()) (cur-text "") (idx 0))
    (cl-flet ((purge-text ()
                (when (> (length cur-text) 0)
                  (push (list 'text cur-text) tokens)
                  (setq cur-text ""))))
      (while (< idx (length input))
        ;; Need to use (eq ... idx) to ensure regex is matching from
        ;; idx onwards. `string-start' in the regexp itself doesn't
        ;; work as I would expect, seemingly always matching the
        ;; entirety of input.
        ;; TODO: DRY
        (cond ((eq (string-match orgify--obrace-regexp input idx) idx)
               (purge-text)
               (push `(obrace ,(match-string 0 input)) tokens)
               (setq idx (1- (match-end 0))))
              ((eq (string-match orgify--cbrace-regexp input idx) idx)
               (purge-text)
               (push `(cbrace ,(match-string 0 input)) tokens)
               (setq idx (1- (match-end 0))))
              ((eq (string-match orgify--oeach-regexp input idx) idx)
               ;; Expecting "#each foo in bar" only
               (when (save-match-data
                       (> (length (split-string (match-string 0 input))) 4))
                 (error "Invalid loop in template: %s" (match-string 0 input)))
               (purge-text)
               (push `(oeach ,(match-string 0 input)) tokens)
               (setq idx (1- (match-end 0))))
              ((eq (string-match orgify--ceach-regexp input idx) idx)
               (purge-text)
               (push `(ceach ,(match-string 0 input)) tokens)
               (setq idx (1- (match-end 0))))
              (t
               (setq cur-text (concat cur-text (char-to-string (aref input idx))))))
        (cl-incf idx))
      (purge-text))
    (reverse tokens)))

(defun orgify--eval-string (string env)
  "Evaluate elisp code stored in STRING.

ENV brings the outer lexical environment into the scope of the
`eval', allowing variables from org files to be exposed to
expressions."
  (let ((result (eval (car (read-from-string string)) env)))
    (cond ((numberp result) (number-to-string result))
          (t result))))

(defun orgify--parse (tokens &optional start end)
  "Parse TOKENS into elisp expressions.

START and END optionally restrict parsing to particular indices
in TOKENS.  This is primarily used for recursively descending
through sub-expressions (like in #each)."
  (let (root (cur (or start 0)))
    (while (< cur (or end (length tokens)))
      (let ((token (nth cur tokens)))
        (cond ((eq 'text (car token))
               (push `(insert ,(orgify-lastcar token)) root))
              ((eq 'obrace (car token))
               ;; Can safely assume that the next token is a single
               ;; text node, and the one following that is a closing
               ;; brace.
               (when (not (eq 'cbrace (car (nth (+ cur 2) tokens))))
                 (error "Missing closing handlebars"))
               (push `(insert (orgify--eval-string
                               ,(orgify-lastcar (nth (1+ cur) tokens))
                               env))
                     root)
               (setq cur (+ cur 2)))
              ((eq 'cbrace (car token))
               (error "Unexpected closing handlebars"))
              ((eq 'oeach (car token))
               (let ((istart (1+ cur)) (iend (1+ cur)))
                 (while (not (eq 'ceach (car (nth iend tokens))))
                   (when (> iend (length tokens))
                     (error "Missing end-each token"))
                   (cl-incf iend))
                 (let ((item (nth 1 (split-string (orgify-lastcar token) " ")))
                       (collection (nth 3 (split-string (orgify-lastcar token) " "))))
                   (push `(cl-loop
                           for iter in (orgify--eval-string ,collection env)
                           do (progn
                                (push (cons ',(intern-soft item) iter) env)
                                (,(orgify--parse tokens istart iend) env)))
                         root))
                 (setq cur iend)))
              ((eq 'ceach (car token))
               (error "Unexpected closing each"))))
      (setq cur (1+ cur)))
    `(lambda (env) ,@(reverse root))))

(defun orgify--compile-and-exec (input env)
  "Compile INPUT and execute it with ENV.

ENV is an alist of the containing lexical environment.  This
alist should contain any variables required in the HTML template.
See `eval' for more info."
  (funcall
   (orgify--parse (orgify--tokenize input))
   env))

(provide 'orgify-compiler)
