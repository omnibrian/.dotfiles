;; -*- lexical-binding: t -*-

(defconst start-time (float-time))

(load (concat user-emacs-directory "modules/bootstrap") nil :no-message)
(dots--load "decorations")
(dots--load "whitespace")
(dots--load "window-switching")
(dots--load "input-completion")
(dots--load "term-toggle")
(dots--load "text-completion")

(message "Config loaded in %ss" (- (float-time) start-time))
