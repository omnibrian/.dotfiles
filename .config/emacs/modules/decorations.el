;; -*- lexical-binding: t -*-

(load-theme 'brian t)

;; highlight current line
(global-hl-line-mode)

;; ===== paren =====
;; highlight matching parenthesis
(show-paren-mode 1)

(setq show-paren-style 'parenthesis)

;; ===== linum =====
;; always show line numbers
(global-linum-mode 1)

(defface linum-current-line-face
  `((t
     :inherit linum
     :foreground "goldenrod"
     :weight bold))
  "Face for highlighting the current line number."
  :group 'linum)

(defvar linum-current-line 1 "Current line number.")
(defvar linum-border-width 1 "Border width for linum display.")

(defadvice linum-update (before advice-linum-update activate)
  "Get the last position of linum and set border width."
  (let* ((max-line-number  (count-lines (point-min) (point-max)))
         (base-linum-width (length (number-to-string max-line-number))))
    (setq linum-border-width (number-to-string (+ 1 base-linum-width))
          linum-current-line (line-number-at-pos))))

(defun linum-relative (line-number)
  "Helper for relative line numbers.  LINE-NUMBER is current line number."
  (let* ((diff        (abs (- line-number linum-current-line)))
	       (line-number (if (= diff 0) line-number diff))
	       (face        (if (= diff 0) 'linum-current-line-face 'linum)))
    (propertize (format (concat "%" linum-border-width "d ") line-number)
		            'face face)))

(setq linum-format 'linum-relative)

;; ===== visual bell =====
;; flash mode line instead of ringing bell
;; https://www.emacswiki.org/emacs/AlarmBell
(defun flash-mode-line ()
  "Invert mode line face for 0.1 seconds."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
