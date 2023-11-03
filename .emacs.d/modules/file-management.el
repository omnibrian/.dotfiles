;; -*- lexical-binding: t -*-

;; github.com/bbatsov/projectile
(require 'projectile)

(setq
 projectile-project-search-path `((,dots--repos-dir . 2) (,dots--dotfiles-dir . 0))
 projectile-sort-order 'recently-active
 projectile-switch-project-action 'neotree-projectile-action)

;; always have projectile-mode enabled
(projectile-mode +1)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(add-to-list 'projectile-globally-ignored-directories "^node_modules$")


;; github.com/jaypei/emacs-neotree
(require 'neotree)

(setq
 neo-smart-open t
 neo-theme 'arrow
 neo-show-hidden-files t)

(defun neotree-project-toggle ()
  "Open NeoTree view in projectile root."
  (interactive)
  (let ((project-dir (projectile-project-root)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find (buffer-file-name)))
          (message "Could not find git project root.")))))

(global-set-key (kbd "C-x /") 'neotree-project-toggle)
(global-set-key (kbd "C-x C-/") 'neotree-project-toggle)
