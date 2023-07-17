;; -*- lexical-binding: t -*-

;; github.com/flycheck/flycheck
(require 'flycheck)

(setq-default
 flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(add-hook 'prog-mode-hook 'global-flycheck-mode)
