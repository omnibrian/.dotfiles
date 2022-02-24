;; main emacs init script

;; ================ colors =============================================
(load-theme 'brian t)

;; highlight current line
(global-hl-line-mode)
(set-face-background hl-line-face "#272931")  ; dark-dark-gray
;; ================ colors =============================================


;; ================ line-numbers =======================================
(global-linum-mode 1)

;; highlight current line with different color
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
  "Set relative line numbers."
  (let* ((diff (abs (- line-number linum-last-pos)))
	       (line-number (if (= diff 0) line-number diff))
	       (face (if (= diff 0) 'linum-current-line-face 'linum)))
    (propertize (format (concat "%" linum-border-width "d") line-number)
                'face face)))

(setq linum-format 'linum-relative)
;; ================ line-numbers =======================================


;; ================ decorations ========================================
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
;; ================ decorations ========================================


;; ================ tabs ===============================================
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

(setq-default tab-width 2)  ;; M-x set-variable RET tab-width RET 2
(setq-default sh-basic-offset 2)

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
;; ================ tabs ===============================================


;; ================ whitespace =========================================
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
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
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(setq projectile-completion-system 'ivy)
(setq projectile-project-search-path '(("~/git/" . 2) ("~/.dotfiles" . 0)))

;; ivy / counsel -- github.com/abo-abo/swiper
(use-package ivy
  :straight t
  :defer 0.1
  :config
  (ivy-mode 1))

(setq ivy-use-virtual-buffers t)
(setq ivy-display-style 'fancy)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)

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
  :config
  (global-set-key (kbd "C-x /")   'neotree-project-toggle)
  (global-set-key (kbd "C-x C-/") 'neotree-project-toggle)
  (setq projectile-switch-project-action 'neotree-projectile-action))

(setq neo-smart-open t)
(setq neo-show-hidden-files t)

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
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'prog-mode-hook 'company-tng-mode))

(setq company-tooltip-align-annotations t)
(setq company-minimum-prefix-length 1)

;; magit -- github.com/magit/magit
(use-package magit
  :straight t
  :defer 0.2
  :config
  (global-set-key (kbd "C-c g") 'magit-status))

;; language server protocol -- github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :straight t
  :defer 0.2
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (python-mode . lsp)
  (yaml-mode . lsp))

(setq lsp-enable-snippet nil)

;; language server hovers / actions -- github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :straight t
  :defer 0.2
  :commands lsp-ui-mode)

(setq lsp-ui-peek-always-show t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-doc-enable nil)

;; language server symbols in ivy -- github.com/emacs-lsp/lsp-ivy
(use-package lsp-ivy
  :straight t
  :defer 0.2
  :commands lsp-ivy-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] 'lsp-ivy-workspace-symbol))

;; flycheck -- github.com/flycheck/flycheck
(use-package flycheck
  :straight t
  :defer 0.2
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))


;;;; user modes

;; python language server -- github.com/fredcamps/lsp-jedi
;; requires pip install jedi-language-server
(use-package lsp-jedi
  :straight t
  :defer 0.2
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

;; auto-virtualenv -- github.com/marcwebbie/auto-virtualenv
(use-package auto-virtualenv
  :straight t
  :defer 0.2
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

;; yaml-mode -- github.com/yoshiki/yaml-mode
;; requires npm install -g yaml-language-server
(use-package yaml-mode
  :straight t
  :defer 0.2
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'yamlls)))

(setq lsp-yaml-format-enable t)
(setq lsp-yaml-single-quote t)

;; TODO: go-mode -- github.com/dominikh/go-mode.el
;; ================ packages ===========================================


;; ================ misc ===============================================
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
  "Run shell to steal $PATH from it and set exec-path accordingly."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -c 'echo \$PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;;(set-exec-path-from-shell)

(setenv "PATH" (concat (getenv "HOME") "/.local/bin" ":" (getenv "PATH")))
(add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
;; ================ misc ===============================================
