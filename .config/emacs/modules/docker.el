;; -*- lexical-binding: t -*-

(require 'rx)
(require 'sh-script)

(defgroup dockerfile nil
  "Dockerfile editing commands."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "dockerfile-"
  :group 'languages)

(defcustom dockerfile-mode-command "docker"
  "Which binary to use to build images."
  :group 'dockerfile
  :type 'string)

(defcustom dockerfile-build-force-rm nil
  "Runs docker builder command with --force-rm switch."
  :type 'boolean
  :group 'dockerfile)

(defcustom dockerfile-build-pull nil
  "Runs docker builder command with --pull switch."
  :type 'boolean
  :group 'dockerfile)

(defcustom dockerfile-build-args nil
  "List of --build-arg to pass to docker build."
  :type '(repeat string)
  :group 'dockerfile)

(defcustom dockerfile-build-progress "auto"
  "Type of --progress output (auto, plain, tty) of docker build."
  :group 'dockerfile
  :type 'string)

(defcustom dockerfile-use-buildkit nil
  "Use Docker buildkit for building images."
  :type 'boolean)

(defcustom dockerfile-enable-auto-indent t
  "Toggles the auto indentation functionality."
  :type 'boolean)

(defcustom dockerfile-indent-offset (or standard-indent 2)
  "Dockerfile number of columns for marging-changing functions to indent."
  :type 'integer
  :safe #'integerp
  :group 'dockerfile)

(defface dockerfile-image-name
  '((t (:inherit (font-lock-type-face bold))))
  "Face to highlight the base image name after FROM instruction.")

(defface dockerfile-image-alias
  '((t (:inherit (font-lock-constant-face bold))))
  "Face to highlight the base image alias in FROM ... AS <alias> construct.")

(defconst dockerfile--from-regex
  (rx "from " (group (1+ not-newline)) (or " " eol) (? "as " (group (1+ not-newline)))))

(defvar dockerfile-font-lock-keywords
  `(,(cons (rx (or line-start "onbuild ")
               (group (or "from"
                          "maintainer"
                          "run"
                          "cmd"
                          "expose"
                          "env"
                          "arg"
                          "add"
                          "copy"
                          "entrypoint"
                          "volume"
                          "user"
                          "workdir"
                          "onbuild"
                          "label"
                          "stopsignal"
                          "shell"
                          "healthcheck"))
               word-boundary)
           font-lock-keyword-face)
    (,dockerfile--from-regex
     (1 'dockerfile-image-name)
     (2 'dockerfile-image-alias nil t))
    ,@(sh-font-lock-keywords)
    ,@(sh-font-lock-keywords-2)
    ,@(sh-font-lock-keywords-1))
  "Default `font-lock-keywords' for `dockerfile mode'.")

(defvar dockerfile-mode-map
  (let ((map      (make-sparse-keymap)))
    (define-key map "\C-c\C-b" #'dockerfile-build-buffer)
    (define-key map "\C-c\M-b" #'dockerfile-build-no-cache-buffer)
    (define-key map "\C-c\C-c" #'comment-region)
    map)
  "Keymap for `dockerfile-mode'.")

(defvar dockerfile-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?= "." table)
    table)
  "Syntax table for `dockerfile-mode'.")

(define-abbrev-table 'dockerfile-mode-abbrev-table nil
  "Abbrev table used while in `dockerfile-mode'.")

(unless dockerfile-mode-abbrev-table
  (define-abbrev-table 'dockerfile-mode-abbrev-table ()))

(defun dockerfile-indent-line-function ()
  "Indent lines in a Dockerfile.

Lines beginning with a keyword are ignored, and any others are
indented by on `dockerfile-indent-offset'. Functionality toggled
by `dockerfile-enable-auto-indent'."
  (when dockerfile-enable-auto-indent
    (unless (member (get-text-property (point-at-bol) 'face)
                    '(font-lock-comment-delimiter-face font-lock-keyword-face))
      (save-excursion
        (beginning-of-line)
        (skip-chars-forward "[ \t]" (point-at-eol))
        (unless (equal (point) (point-at-eol)) ; Ignore empty lines
          ;; Delete existing whitespace
          (delete-char (- (point-at-bol) (point)))
          (indent-to dockerfile-indent-offset))))))

(defun dockerfile-build-arg-string ()
  "Create a --build-arg string for each element in `dockerfile-build-args'."
  (mapconcat (lambda (arg) (concat "--build-arg " (shell-quote-argument arg)))
             dockerfile-build-args " "))

(defun dockerfile-tag-string (image-name)
  "Return a --tag shell-quoted IMAGE-NAME string.

Returns empty string if IMAGE-NAME is blank."
  (if (string= image-name "")
      ""
    (format "--tag %s "
            (shell-quote-argument image-name))))

(defvar dockerfile-image-name nil
  "Name of the dockerfile currently being used.
This can be set in file or directory-local variables.")

(defvar dockerfile-image-name-history nil
  "History of image names read by `dockerfile-read-image-name'.")

(defun dockerfile-read-image-name ()
  "Read a docker image name."
  (read-string "Image name: " dockerfile-image-name 'dockerfile-image-name-history))


;;;###autoload
(defun dockerfile-build-buffer (image-name &optional no-cache)
  "Build an image called IMAGE-NAME based upon the buffer.

If the prefix arg NO-CACHE is set, don't cache the image.

The shell command used to build the image is:

    docker build         \\
      --no-cache         \\
      --force-rm         \\
      --pull             \\
      --tag IMAGE-NAME   \\
      --build-args args  \\
      --progress type    \\
      -f filename        \\
      directory"
  (interactive (list (dockerfile-read-image-name) prefix-arg))
  (save-buffer)
  (compilation-start
   (format
    "%s%s build %s%s%s%s %s --progress %s -f %s %s"
    (if dockerfile-use-buildkit "DOCKER_BUILDKIT=1 " "")
    dockerfile-mode-command
    ;; build
    (if no-cache "--no-cache " "")
    (if dockerfile-build-force-rm "--force-rm " "")
    (if dockerfile-build-pull "--pull " "")
    (dockerfile-tag-string image-name)
    (dockerfile-build-arg-string)
    ;; --progress
    dockerfile-build-progress
    (shell-quote-argument (convert-standard-filename
                           (or (file-remote-p (buffer-file-name) 'localname)
                               (buffer-file-name))))
    (shell-quote-argument (convert-standard-filename
                           (or (file-remote-p default-directory 'localname)
                               default-directory)))))
  nil
  (lambda (_) (format "*dockerfile-build-output: %s *" image-name)))

;;;###autoload
(defun dockerfile-build-no-cache-buffer (image-name)
  "Build an image called IMAGE-NAME based upon the buffer without cache."
  (interactive (list (dockerfile-read-image-name)))
  (dockerfile-build-buffer image-name t))

(defun dockerfile--imenu-function ()
  "Find the previous headline from point.

Search for a FROM instruction. If an alias is used this is
returned, otherwise the base image name is used."
  (when (re-search-backward dockerfile--from-regex nil t)
    (let ((data (match-data)))
      (when (match-string 2)
        ;; drop the first match group because imenu-generic-expression
        ;; can only use one offset, so normalize to `1'.
        (set-match-data (list (nth 0 data)
                              (nth 1 data)
                              (nth 4 data)
                              (nth 5 data))))
      t)))

;;;###autoload
(define-derived-mode dockerfile-mode prog-mode "Dockerfile"
  "A major mode to edit Dockerfiles.
\\{dockerfile-mode-map}"
  (set-syntax-table dockerfile-mode-syntax-table)
  (set (make-local-variable 'imenu-generic-expression)
       `(("Stage" dockerfile--imenu-function 1)))
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'font-lock-defaults)
       '(dockerfile-font-lock-keywords nil t))
  (setq local-abbrev-table dockerfile-mode-abbrev-table)
  (set (make-local-variable 'indent-line-function) #'dockerfile-indent-line-function))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (concat "[/\\]"
                           "\\(?:Containerfile\\|Dockerfile\\)"
                           "\\(?:\\.[^/\\]*\\)?\\'")
                   'dockerfile-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode))

(provide 'dockerfile-mode)


(add-hook 'dockerfile-mode-hook 'highlight-indentation-mode)
