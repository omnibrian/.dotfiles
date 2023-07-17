;; -*- lexical-binding: t -*-

(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

(setq-default tab-width 2)  ;; M-x set-variable RET tab-width RET 2
(setq-default sh-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)
(setq-default css-indent-offset 2)

;; trailing whitespace
(setq-default show-trailing-whitespace t)

;; electric-indent-mode doesn't work with python-mode
(add-hook 'electric-indent-functions
          #'(lambda (char)
             (if (equal major-mode 'python-mode)
                 'no-indent
               nil)))

;; enter key executes newline-and-indent
(add-hook 'python-mode-hook
          #'(lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'write-file-functions 'delete-trailing-whitespace)
