;; -*- lexical-binding: t -*-

;; github.com/emacs-typescript/typescript.el
(require 'typescript-mode)

;; github.com/ananthakumaran/tide
(require 'tide)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)

  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-select-checker 'javascript-eslint) ; this might cause issues

  (eldoc-mode +1)

  (tide-hl-identifier-mode +1)

  (company-mode +1))

;; aligns annotation to the right hand side in company
(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; github.com/fxbois/web-mode
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(setq
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(flycheck-add-mode 'javascript-eslint 'web-mode)

;; enable highlight-indentation
(add-hook 'web-mode-hook 'highlight-indentation-mode)
(add-hook 'tide-mode-hook 'highlight-indentation-mode)
