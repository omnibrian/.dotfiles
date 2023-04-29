;; -*- lexical-binding: t -*-

(defgroup term-toggle nil
  "Quickly toggle a terminal in current working directory."
  :prefix "term-toggle--")

(defcustom term-toggle--kill-buffer-on-process-exit t
  "Kill buffer when shell process has exited."
  :type 'boolean
  :group 'term-toggle)

(defcustom term-toggle--minimum-split-height 10
  "Minimum height of a split window."
  :type 'fixnum
  :group 'term-toggle)

(defcustom term-toggle--default-height 15
  "Default height for split window."
  :type 'fixnum
  :group 'term-toggle)

;; functions
(defun term-toggle--start-shell (shell name)
  "Create shell process."
  (cond ((or (eq shell 'term) (eq shell 'ansi-term))
         (funcall shell (getenv "SHELL")))
        (t (funcall shell)))
  (let ((proc (get-buffer-process (get-buffer name))))
    (when proc
      (set-process-query-on-exit-flag proc nil)
      (if term-toggle--kill-buffer-on-process-exit
          (set-process-sentinel
           proc (lambda (__ evt)
                  (when (string-match-p "\\(?:exited\\|finished\\)" evt)
                    (kill-buffer))))))))

(defun term-toggle--toggle (term-buffer)
  "Create or delete term window and buffer."
  (let ((term-window (get-buffer-window term-buffer)))
    (if term-window
        (progn
          (bury-buffer term-buffer)
          (delete-window term-window))
      (progn
        (split-window-vertically)
        (other-window 1)
        (pop-to-buffer-same-window term-buffer t)
        (set-window-dedicated-p term-window t)
        (when (>= (window-total-height (selected-window))
                  term-toggle--minimum-split-height)
          (let ((delta (- (window-height (selected-window)) term-toggle--default-height)))
            (if (> delta 0)
                (shrink-window delta))))))))

(defun term-toggle (shell)
  "Wrapper for term toggle enabling jump to term buffer."
  (let ((name (format "*%s*" (if (eq shell 'term) "terminal" shell)))
        (original-buffer (current-buffer)))
    (unless (get-buffer name)
      (term-toggle--start-shell shell name)
      (pop-to-buffer-same-window original-buffer))
    (term-toggle--toggle (get-buffer name))))

;; commands
(defun term-toggle-term ()
  "Toggle 'term'."
  (interactive)
  (term-toggle 'term))

(defun term-toggle-shell ()
  "Toggle 'shell'."
  (interactive)
  (term-toggle 'shell))

(defun term-toggle-ansi ()
  "Toggle 'ansi'."
  (interactive)
  (term-toggle 'ansi-term))

(defun term-toggle-eshell ()
  "Toggle 'eshell'."
  (interactive)
  (term-toggle 'eshell))

;; keybinds
(global-set-key (kbd "C-c t") 'term-toggle-term)
(global-set-key (kbd "C-c C-t") 'term-toggle-term)
(global-set-key (kbd "C-`") 'term-toggle-term)

(provide 'term-toggle)
