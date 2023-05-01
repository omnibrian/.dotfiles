;; -*- lexical-binding: t -*-

(defconst dots--emacs-dir   (file-name-as-directory (expand-file-name user-emacs-directory)))
(defconst dots--modules-dir (file-name-as-directory (concat dots--emacs-dir "modules")))
(defconst dots--saves-dir   (file-name-as-directory (concat dots--emacs-dir "autosaves")))
(defconst dots--repos-dir   (expand-file-name "~/git/"))

(setf
 ;; automatically follow symlinks when file is in version control
 vc-follow-symlinks     t
 ;; fix option key not getting recognized as meta on mac
 mac-option-modifier    'meta
 ;; store customizations in emacs dir
 custom-file            (concat dots--emacs-dir "custom.el")
 ;; point to dotfiles theme dir
 custom-theme-directory (concat dots--emacs-dir "themes")
 ;; don't prompt to load newer
 load-prefer-newer      noninteractive
 ;; dump backup files into saves dir instead
 backup-directory-alist `(("." . ,dots--saves-dir)))

;; aint nobody got time for 'yes' when a simple 'y' will do
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
