;; -*- lexical-binding: t -*-

(defconst dots--emacs-dir   (file-name-as-directory (expand-file-name user-emacs-directory)))
(defconst dots--modules-dir (file-name-as-directory (concat dots--emacs-dir "modules")))
(defconst dots--saves-dir   (file-name-as-directory (concat dots--emacs-dir "autosaves")))
(defconst dots--repos-dir   (expand-file-name "~/git/"))

(setf
 vc-follow-symlinks     t
 custom-file            (concat dots--emacs-dir "custom.el")
 custom-theme-directory (concat dots--emacs-dir "themes")
 load-prefer-newer      noninteractive
 backup-directory-alist `(("." . ,dots--saves-dir)))

(defalias 'yes-or-no-p 'y-or-n-p)

(eval-when-compile (require 'cl-lib))

(defgroup dots nil
  "dotfiles group."
  :group  'dots
  :prefix "dots--")

(cl-defmacro dots--load (file &key if)
  "Load custom module from dotfiles."
  (when (or (null if) (eval if))
    `(load (concat dots--modules-dir ,file) nil :no-messages)))
