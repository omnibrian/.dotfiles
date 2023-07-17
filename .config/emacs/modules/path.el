;; -*- lexical-binding: t -*-

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
