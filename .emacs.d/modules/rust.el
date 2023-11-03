;; -*- lexical-binding: t -*-

;; github.com/rust-lang/rust-mode

(setq
 rust-format-on-save t)

;; add rust-mode folder to load path
(add-to-list 'load-path (concat dots--packages-dir "rust-mode"))

(autoload 'rust-mode "rust-mode"
  "rust language mode"
  t)

(require 'rust-mode)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
