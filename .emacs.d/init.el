;;; init.el --- main Emacs init script -*- lexical-binding: t -*-

;;; Commentary:
;;; Entrypoint for Emacs configuration.

;;; Code:
;; ================ colors =============================================
(load-theme 'brian t)

;; highlight current line
(global-hl-line-mode)
;; ================ colors =============================================


;; ================ decorations ========================================
(global-linum-mode 1)

;; highlight current line with different color
;; TODO include this in theme
(defface linum-current-line-face
  `((t :inherit linum
       :foreground "goldenrod"
       :weight bold))
  "Face for displaying the current line number."
  :group 'linum)

;; add vars for relative line numbers and border-width++
(defadvice linum-update (before advice-linum-update activate)
  "Get the last position of linum and set border width."
  (setq linum-last-pos (line-number-at-pos)
        linum-border-width (number-to-string
                            (+ 1 (length
                                  (number-to-string
                                   (count-lines
                                    (point-min)
                                    (point-max))))))))

(defun linum-relative (line-number)
  "Helper for relative line numbers.  LINE-NUMBER is current line number."
  (let* ((diff (abs (- line-number linum-last-pos)))
	       (line-number (if (= diff 0) line-number diff))
	       (face (if (= diff 0) 'linum-current-line-face 'linum)))
    (propertize (format (concat "%" linum-border-width "d") line-number)
                'face face)))

(setq linum-format 'linum-relative)

;; window decorations
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
;; ================ decorations ========================================


;; ================ whitespace =========================================
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

(setq-default tab-width 2)  ;; M-x set-variable RET tab-width RET 2
(setq-default sh-basic-offset 2)
(setq-default js-indent-level 2)

;; electric-indent-mode doesn't work with python-mode
(add-hook 'electric-indent-functions
          '(lambda (char)
             (if (equal major-mode 'python-mode)
                 'no-indent
               nil)))

;; enter key executes newline-and-indent
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'write-file-functions 'delete-trailing-whitespace)
;; ================ whitespace =========================================


;; ================ packages ===========================================
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; make straight-use-package defacto
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; projectile -- github.com/bbatsov/projectile
(use-package projectile
  :straight t
  :defer 0.1
  :bind-keymap
  (("C-c p" . projectile-command-map))
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '(("~/git/" . 2) ("~/.dotfiles" . 0)))
  :config
  (projectile-mode +1)
  (with-eval-after-load "neotree"
    (with-eval-after-load "auto-virtualenv"
      (setq projectile-switch-project-action
            '(lambda ()
               (neotree-projectile-action)
               (auto-virtualenv-set-virtualenv))))))

;; ivy -- github.com/abo-abo/swiper
(use-package ivy
  :straight t
  :defer 0.1
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-selectable-prompt t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

;; counsel -- github.com/abo-abo/swiper
(use-package counsel
  :straight t
  :after ivy
  :defer 0.1
  :config
  (counsel-mode))

;; neotree -- github.com/jaypei/emacs-neotree
(defun neotree-project-toggle ()
  "Open NeoTree in Projectile root."
  (interactive)
  (let ((project-dir (projectile-project-root)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find (buffer-file-name)))
          (message "Could not find git project root.")))))

(use-package neotree
  :straight t
  :after projectile
  :defer 0.2
  :bind
  (("C-x /"   . neotree-project-toggle)
   ("C-x C-/" . neotree-project-toggle))
  :init
  (setq neo-smart-open t)
  (setq neo-show-hidden-files t))

;; which-key -- github.com/justbur/emacs-which-key
(use-package which-key
  :straight t
  :defer 0.1
  :config
  (which-key-mode))

;; company-mode -- github.com/company-mode/company-mode
(use-package company
  :straight t
  :defer 0.2
  :hook
  ((prog-mode . company-mode)
   (prog-mode . company-tng-mode))
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1))

;; magit -- github.com/magit/magit
(use-package magit
  :straight t
  :defer 0.2
  :bind
  (("C-c g" . magit-status)
   ("C-x g" . magit-status)))

;; language server protocol -- github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :straight t
  :defer 0.2
  :commands lsp
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   ;; (python-mode . lsp)
   ;; (js-mode . lsp)
   (yaml-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-snippet nil)
  :config
  ;; python language server -- github.com/python-lsp/python-lsp-server
  ;; requires: pip install python-lsp-server[all] pylint
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-enabled-clients 'pylsp)
  (setq lsp-pylsp-plugins-pylint-enabled t)
  (setq lsp-pylsp-plugins-pydocstyle-enabled nil))

;; language server hovers / actions -- github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :straight t
  :defer 0.2
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-doc-enable nil))

;; language server symbols in ivy -- github.com/emacs-lsp/lsp-ivy
(use-package lsp-ivy
  :straight t
  :defer 0.2
  :commands lsp-ivy-workspace-symbol
  :bind
  (:map lsp-mode-map
        ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)))

;; flycheck -- github.com/flycheck/flycheck
(use-package flycheck
  :straight t
  :defer 0.2
  :hook
  ((prog-mode . flycheck-mode)))


;;;; user modes

;; elpy -- github.com/jorgenschaefer/elpy
(use-package elpy
  :straight t
  :defer 0.2
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; auto-virtualenv -- github.com/marcwebbie/auto-virtualenv
(use-package auto-virtualenv
  ;;  :straight (auto-virtualenv :type git :host github :repo "marcwebbie/auto-virtualenv")
  :straight t
  :defer 0.2
  :hook
  ((python-mode . auto-virtualenv-set-virtualenv)))

;; pyvenv -- github.com/jorgenschaefer/pyvenv
;;(use-package pyvenv
;;  :straight t
;;  :defer 0.2
;;  :config
;;  (pyvenv-mode t)
;;  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))

;; yaml-mode -- github.com/yoshiki/yaml-mode
;; requires npm install -g yaml-language-server
(use-package yaml-mode
  :straight t
  :defer 0.2
  :init
  (setq lsp-yaml-format-enable t)
  (setq lsp-yaml-single-quote t)
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'yamlls)))

;; terraform-mode -- github.com/emacsorphanage/terraform-mode
(use-package terraform-mode
  :straight t
  :defer 0.2)

;; js2-mode -- github.com/mooz/js2-mode
(use-package js2-mode
  :straight t
  :defer 0.2
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode)))

;; rjsx-mode -- github.com/felipeochoa/rjsx-mode
(use-package rjsx-mode
  :straight t
  :defer 0.2)

;; tide -- github.com/ananthakumaran/tide
(use-package tide
  :straight t
  :defer 0.2
  :after
  (typescript-mode company flycheck)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  :hook
  ((js2-mode-hook . setup-tide-mode)
   (typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode)
   (before-save . tide-format-before-save)))

;; TODO: go-mode -- github.com/dominikh/go-mode.el
;; ================ packages ===========================================


;; ================ misc ===============================================
;; don't need to here any bell sounds, so flash mode line instead
;; https://www.emacswiki.org/emacs/AlarmBell
(defun flash-mode-line ()
  "Invert mode line face for 0.1 seconds."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

;; always follow symlinks to version controlled files
(setq vc-follow-symlinks t)

;; matching parens
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; trailing whitespace
(setq-default show-trailing-whitespace t)

;; aint nobody got time for 'yes', we use 'y' instead here
(defalias 'yes-or-no-p 'y-or-n-p)

;; update PATH to match what's in shell rcfiles
(defun set-exec-path-from-shell ()
  "Run shell to steal $PATH from it and set 'exec-path' accordingly."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$"
          ""
          (shell-command-to-string "$SHELL --login -c 'echo \$PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(setenv "PATH" (concat (getenv "HOME") "/.local/bin" ":" (getenv "PATH")))
(add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
;; ================ misc ===============================================


(provide 'init)
;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(elpy-syntax-check-command "pylint"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
