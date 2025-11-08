;;; world-peace-mode.el --- Major mode for World Peace -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Lukáš Hozda
;;
;; Author: Lukáš Hozda <luk.hozda@gmail.com>
;; Maintainer: Lukáš Hozda <luk.hozda@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: languages
;; Homepage: https://github.com/luciusmagn/world-peace
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Major mode for editing World Peace source files.
;;
;;; Code:

(defvar org-src-lang-modes)

(defgroup world-peace nil
  "Editing support for the World Peace programming language."
  :group 'languages)

(defcustom world-peace-indent-offset 3
  "Indentation width for World Peace continuation lines."
  :type 'integer
  :safe #'integerp
  :group 'world-peace)

(defconst world-peace--identifier-regexp
  "[[:alpha:]_][[:alnum:]_]*")

(defvar world-peace-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for `world-peace-mode'.")

(defvar world-peace-font-lock-keywords
  `((,(regexp-opt '("dec" "end" "ret" "do" "by" "case" "num" "load"
                    "if" "else")
                  'words)
     . font-lock-keyword-face)
    (,(regexp-opt '("len" "push" "pop" "print" "read" "syscall"
                    "errno" "argv")
                  'words)
     . font-lock-builtin-face)
    (,(concat "\\_<dec\\_>\\s-+\\(" world-peace--identifier-regexp "\\)")
     1 font-lock-function-name-face)
    (,(concat "\\_<num\\_>\\s-*"
              "\\(?:\\[[^]\n]*\\]\\s-*\\)?"
              "\\(" world-peace--identifier-regexp "\\)")
     1 font-lock-variable-name-face)
    ("\\_<load\\_>\\s-+\\([^;\n]+\\)"
     1 font-lock-constant-face)
    ("\\_<0[xX][0-9A-Fa-f][0-9A-Fa-f_]*\\_>"
     . font-lock-constant-face)
    ("\\_<0[bB][01][01_]*\\_>"
     . font-lock-constant-face)
    ("\\_<0[oO][0-7][0-7_]*\\_>"
     . font-lock-constant-face)
    ("\\_<[0-9][0-9_]*\\_>"
     . font-lock-constant-face)
    ("\\_<_\\_>"
     . font-lock-constant-face)
    (,(regexp-opt '(">>=" "<<=" "-->" "---" "<-" "==" "!=" "<=" ">="
                    "<<" ">>" "&&" "||" "+=" "-=" "*=" "/=" "%="
                    "&=" "|=" "^=" ".." "=" "+" "-" "*" "/" "%"
                    "!" "^" "<" ">" "&" "|" "." "," ":" ";"
                    "(" ")" "[" "]" "{" "}")
                  t)
     . font-lock-builtin-face)
    ("^\\s-*---"
     . font-lock-preprocessor-face))
  "Font-lock rules for `world-peace-mode'.")

(defvar world-peace-imenu-generic-expression
  `((nil ,(concat "^\\s-*dec\\s-+\\(" world-peace--identifier-regexp "\\)")
         1))
  "Imenu rules for `world-peace-mode'.")

(defun world-peace--previous-code-line-indentation ()
  "Return the indentation of the previous nonblank line."
  (save-excursion
    (let ((indentation 0)
          (done nil))
      (while (and (not done)
                  (zerop (forward-line -1)))
        (unless (looking-at-p "\\s-*$")
          (setq indentation (current-indentation)
                done t)))
      indentation)))

(defun world-peace--line-closes-block-p ()
  "Return non-nil when the current line closes a block."
  (save-excursion
    (back-to-indentation)
    (looking-at-p "\\(?:}\\|end\\_>\\)")))

(defun world-peace--previous-line-opens-block-p ()
  "Return non-nil when the previous code line opens a block."
  (save-excursion
    (let ((opens nil)
          (done nil))
      (while (and (not done)
                  (zerop (forward-line -1)))
        (unless (looking-at-p "\\s-*$")
          (end-of-line)
          (setq opens (save-excursion
                        (skip-chars-backward " \t")
                        (or (eq (char-before) ?{)
                            (save-excursion
                              (beginning-of-line)
                              (looking-at-p "\\s-*dec\\_>"))))
                done t)))
      opens)))

(defun world-peace-indent-line ()
  "Indent current line as World Peace code."
  (interactive)
  (let ((offset (- (current-column) (current-indentation)))
        (indentation (world-peace--previous-code-line-indentation)))
    (when (world-peace--previous-line-opens-block-p)
      (setq indentation (+ indentation world-peace-indent-offset)))
    (when (world-peace--line-closes-block-p)
      (setq indentation (max 0 (- indentation world-peace-indent-offset))))
    (when (save-excursion
            (back-to-indentation)
            (looking-at-p "---"))
      (setq indentation 0))
    (indent-line-to indentation)
    (when (> offset 0)
      (move-to-column (+ indentation offset)))))

(defun world-peace-insert-body-line ()
  "Insert a new World Peace function body line marker."
  (interactive)
  (end-of-line)
  (newline)
  (insert "--- ")
  (world-peace-indent-line)
  (end-of-line))

(defun world-peace--continue-body-marker-p ()
  "Return non-nil when the next line should start with a body marker."
  (save-excursion
    (back-to-indentation)
    (or (looking-at-p "---")
        (looking-at-p "dec\\_>.*:\\s-*$"))))

(defun world-peace-newline-and-indent ()
  "Insert a newline and continue body markers where appropriate."
  (interactive)
  (let ((continue-body-marker-p (world-peace--continue-body-marker-p)))
    (newline)
    (when continue-body-marker-p
      (insert "--- "))
    (world-peace-indent-line)))

;;;###autoload
(define-derived-mode world-peace-mode prog-mode "World Peace"
  "Major mode for editing World Peace programming language code."
  :syntax-table world-peace-mode-syntax-table
  (setq-local font-lock-defaults '(world-peace-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local indent-line-function #'world-peace-indent-line)
  (setq-local imenu-generic-expression world-peace-imenu-generic-expression)
  (define-key world-peace-mode-map (kbd "RET")
              #'world-peace-newline-and-indent)
  (define-key world-peace-mode-map (kbd "C-c C-j")
              #'world-peace-insert-body-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wp\\'" . world-peace-mode))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("world-peace" . world-peace)))

(provide 'world-peace-mode)
;;; world-peace-mode.el ends here
