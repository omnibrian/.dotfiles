;; -*- lexical-binding: t -*-

;; Might be worth bringing in some vertico.el functions to have better TAB
;; support in completion.

;; It would be nice to port the more interesting consult.el functions in.
;;   (consult-git-grep, consult-line, etc)

(fido-vertical-mode +1)

;; github.com/justbur/emacs-which-key
(require 'which-key)

;; always have which-key-mode enabled
(which-key-mode)
