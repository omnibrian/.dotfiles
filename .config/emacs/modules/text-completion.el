;; -*- lexical-binding: t -*-

;; github.com/company-mode/company-mode

(setq
 company-tooltip-align-annotations t
 company-minimum-prefix-length 1)

;; add company-mode folder to load path
(add-to-list 'load-path (concat dots--modules-dir "company-mode"))

;; --- autoload ---
;; modes
(autoload 'company-mode "company"
  "\"complete anything\""
  t
  nil)

(put 'global-company-mode 'globalized-minor-mode t)

(defvar global-company-mode nil
  "Non-nil if Global Company mode is enabled.")

(custom-autoload 'global-company-mode "company" nil)

(autoload 'global-company-mode "company"
  "Toggle Company mode in all buffers."
  t
  nil)

(autoload 'company-manual-begin "company"
  nil
  t
  nil)

(autoload 'company-complete "company"
  "Insert the common part of all candidates or the current selection."
  t
  nil)

(register-definition-prefixes "company" '("company-"))

;; abbrev
(autoload 'company-abbrev "company-abbrev"
  "\"company-mode\" completion backend for abbrev."
  t
  nil)

(register-definition-prefixes "company-abbrev" '("company-abbrev-insert"))

;; bbdb
(autoload 'company-bbdb "company-bbdb"
  "\"company-mode\" completion backend for BBDB."
  t
  nil)

(register-definition-prefixes "company-bbdb" '("company-bbdb-"))

;; capf
(register-definition-prefixes "company-capf" '("company-"))

;; clang
(register-definition-prefixes "company-clang" '("company-clang"))

;; cmake
(register-definition-prefixes "company-cmake" '("company-cmake"))

;; css
(autoload 'company-css "company-css"
  "\"company-mode\" completion backend for \"css-mode\"."
  t
  nil)

(register-definition-prefixes "company-css" '("company-css-"))

;; dabbrev
(autoload 'company-dabbrev "company-dabbrev"
  "dabbrev-like \"company-mode\" completion backend."
  t
  nil)

(register-definition-prefixes "company-dabbrev" '("company-dabbrev-"))

;; dabbrev-code
(autoload 'company-dabbrev-code "company-dabbrev-code"
  "dabbrev-like \"company-mode\" backend for code."
  t
  nil)

(register-definition-prefixes "company-dabbrev-code" '("company-dabbrev-code-"))

;; eclim
(register-definition-prefixes "company-eclim" '("company-eclim"))

;; elisp
(autoload 'company-elisp "company-elisp"
  "\"company-mode\" completion backend for Emacs Lisp."
  t
  nil)

(register-definition-prefixes "company-elisp" '("company-elisp-"))

;; etags
(autoload 'company-etags "company-etags"
  "\"company-mode\" completion backend for etags."
  t
  nil)

(register-definition-prefixes "company-etags" '("company-etags-"))

;; files
(autoload 'company-files "company-files"
  "\"company-mode\" completion backend for existing file names."
  t
  nil)

(register-definition-prefixes "company-files" '("company-file"))

;; gtags
(autoload 'company-gtags "company-gtags"
  "\"company-mode\" completion backend for GNU Global."
  t
  nil)

(register-definition-prefixes "company-gtags" '("company-gtags-"))

;; ispell
(autoload 'company-ispell "company-ispell"
  "\"company-mode\" completion backend using Ispell."
  t
  nil)

(register-definition-prefixes "company-ispell" '("company-ispell-"))

;; keywords
(autoload 'company-keywords "company-keywords"
  "\"company-mode\" backend for programming language keywords."
  t
  nil)

(register-definition-prefixes "company-keywords" '("company-keywords-"))

;; nxml
(autoload 'company-nxml "company-nxml"
  "\"company-mode\" completion backend for \"nxml-mode\"."
  t
  nil)

(register-definition-prefixes "company-nxml" '("company-nxml-"))

;; oddmuse
(autoload 'company-oddmuse "company-oddmuse"
  "\"company-mode\" completion backend for \"oddmuse-mode\"."
  t
  nil)

(register-definition-prefixes "company-oddmuse" '("company-oddmuse-"))

;; semantic
(autoload 'company-semantic "company-semantic"
  "\"company-mode\" completion backend using CEDET Semantic."
  t
  nil)

(register-definition-prefixes "company-semantic" '("company-semantic-"))

;; tempo
(autoload 'company-tempo "company-tempo"
  "\"company-mode\" completion backend for tempo."
  t
  nil)

(register-definition-prefixes "company-tempo" '("company-tempo-"))

;; tng
(autoload 'company-tng-frontend "company-tng"
  "Auto populate current selection into buffer."
  nil
  nil)

(autoload 'company-tng-configure-default "company-tng"
  "Applies the default configuration to enable company-tng."
  nil
  nil)

(register-definition-prefixes "company-tng" '("company-tng--"))

;; xcode
(autoload 'company-xcode "company-xcode"
  "\"company-mode\" completion backend for Xcode projects."
  t
  nil)

(register-definition-prefixes "company-xcode" '("company-xcode-"))

(require 'company)

;; mode for company-tng frontend
(defvar company-tng-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap company-active-map)
    (define-key keymap [return] nil)
    (define-key keymap (kbd "RET") nil)
    (define-key keymap [tab] 'company-select-next)
    (define-key keymap (kbd "TAB") 'company-select-next)
    (define-key keymap [backtab] 'company-select-previous)
    (define-key keymap (kbd "S-TAB") 'company-select-previous)
    keymap))

(define-minor-mode company-tng-mode
  "This minor mode enables `company-tng-frontend'."
  :init-value nil
  :global t
  (cond
   (company-tng-mode
    (setq company-frontends
          (add-to-list 'company-frontends 'company-tng-frontend))
    (setq company-frontends '(company-tng-frontend
                              company-pseudo-tooltip-frontend
                              company-echo-metadata-frontend))
    (setq company-require-match nil
          company-clang-insert-arguments nil
          company-semantic-insert-arguments nil
          company-rtags-insert-arguments nil
          lsp-enable-snippet nil)
    (setq company-active-map company-tng-map)
    (setq company-selection-default nil))
   (t
    (setq company-frontends
          '(company-pseudo-tooltip-unless-just-one-frontend
            company-preview-if-just-one-frontend
            company-echo-metadata-frontend))
    (setq company-require-match 'company-explicit-action-p
          company-clang-insert-arguments t
          company-semantic-insert-arguments t
          company-rtags-insert-arguments t
          lsp-enable-snippet t)
    (setq company-active-map (keymap-parent company-tng-map))
    (setq company-selection-default 0))))

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'company-tng-mode)
