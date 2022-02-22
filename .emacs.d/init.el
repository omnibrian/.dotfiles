;; main emacs init script

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


;; ================ colors =============================================
(load-theme 'brian t)

;; highlight current line
(global-hl-line-mode)
(set-face-background hl-line-face "#272931")  ; dark-dark-gray
;; ================ colors =============================================


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
(defvar bootstrap-version)

(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; helm -- github.com/emacs-helm/helm
(straight-use-package 'helm)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
;; ================ packages ===========================================


;; ================ misc ===============================================
;; always follow symlinks to version controlled files
(setq vc-follow-symlinks t)
;; ================ misc ===============================================
