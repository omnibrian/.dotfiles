;; -*- lexical-binding: t -*-

;; based on company -- github.com/company-mode/company-mode

(require 'cl-lib)
(require 'subr-x)
(require 'pcase)

(defgroup tcomplete nil
  "Inline text completion."
  :group 'abbrev
  :group 'convenience
  :group 'matching)

(defgroup tcomplete-faces nil
  "Faces used by tcomplete."
  :group 'tcomplete
  :group 'faces)

(defcustom tcomplete-backends `(tcomplete-semantic
                                tcomplete-cmake
                                tcomplete-capf
                                tcomplete-clang
                                tcomplete-files
                                (tcomplete-dabbrev-code
                                 tcomplete-gtags
                                 tcomplete-etags
                                 tcomplete-keywords)
                                tcomplete-oddmuse
                                tcomplete-company-dabbrev)
  "List of completion engines to activate.")

(defvar tcomplate-async-wait 0.03
  "Pause between checks to see if value has been set.")

(defvar tcomplete-async-timeout 2
  "Maximum wait time for an async call.")

(defvar tcomplete-active-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-g" 'tcomplete-abort)
    (define-key keymap (kbd "C-n") 'tcomplete-select-next-or-abort)
    (define-key keymap (kbd "C-p") 'tcomplete-select-previous-or-abort)
    (define-key keymap (kbd "<down>") 'tcomplete-select-next-or-abort)
    (define-key keymap (kbd "<up>") 'tcomplete-select-previous-or-abort)
    (define-key keymap [remap scroll-up-command] 'tcomplete-next-page)
    (define-key keymap [remap scroll-down-command] 'tcomplete-previous-page)
    (define-key keymap [return] 'tcomplete-complete-selection)
    (define-key keymap (kbd "RET") 'tcomplete-complete-selection)
    (define-key keymap [tab] 'tcomplete-complete-common)
    (define-key keymap (kbd "TAB") 'tcomplete-complete-common)
    (define-key keymap (kbd "<f1>") 'tcomplete-show-doc-buffer)
    (define-key keymap (kbd "C-h") 'tcomplete-show-doc-buffer)
    (define-key keymap "\C-s" 'tcomplete-search-candidates)
    (define-key keymap "\C-\M-s" 'tcomplete-filter-candidates)
    keymap)
  "Keymap to enable during active completion.")

(defvar tcomplete--disabled-backends nil)

(defun tcomplete-init-backend (backend)
  (when (and (symbolp backend)
             (not (fboundp backend)))
    (ignore-errors (require backend nil t)))
  (cond
   ((symbolp backend)
    (condition-case err
        (progn
          (funcall backend 'init)
          (put backend 'tcomplete-init t))
      (error
       (put backend 'tcomplete-init 'failed)
       (unless (memq backend tcomplete--disabled-backends)
         (message "tcomplete backend '%s' could not be initialized:\n%s"
                  backend (error-message-string err)))
       (cl-pushnew backend tcomplete--disabled-backends)
       nil)))
   ;; don't initialize when it's a lambda
   ((functionp backend) t)
   ;; if it's not a symbol or function, assume list
   (t
    (cl-dolist (b backend)
      (unless (keywordp b)
        (tcomplete-init-backend b))))))

(defun tcomplete-backend-init-p (backend)
  (if (symbolp backend)
      (get backend 'tcomplete-init)
    nil))

(defun tcomplete-try-init-backend (backend)
  (let ((backend-init (tcomplete-backend-init-p backend)))
    (unless (or (eq t backend-init) backend-init)
      (tcomplete-init-backend backend))))


;; ===== modes =====

;;;###autoload
(define-minor-mode tcomplete-mode
  "Mode for completing text in the buffer."
  (if tcomplete-mode
      (progn
        (add-hook 'pre-command-hook 'tcomplete-pre-command nil t)
        (add-hook 'post-command-hook 'tcomplete-post-command nil t)
        (mapc 'tcomplete-init-backend tcomplete-backends))
    (remove-hook 'pre-command-hook 'tcomplete-pre-command t)
    (remove-hook 'post-command-hook 'tcomplete-post-command t)
    (tcomplete-cancel)
    (kill-local-variable 'tcomplete-point)))

;;;###autoload
(define-globalized-minor-mode global-tcomplete-mode tcomplete-mode tcomplete-mode-on)

(defun tcomplete-mode-on ()
  "Enable 'tcomplete-mode' for global mode."
  (unless (or noninteractive
              (eq (aref (buffer-name) 0) ?\s))
    (tcomplete-mode 1)))

;; ===== backends =====

(defvar-local tcomplete-backend nil)

(defun tcomplete-grab (regexp &optional expression limit)
  (when (looking-back regexp limit)
    (or (match-string-no-properties (or expression 0)) "")))

(defun tcomplete-grab-line (regexp &optional expression)
  "Return match string for 'regexp' if matched before point."
  (let ((inhibit-field-text-motion t))
    (tcomplete-grab regexp expression (line-beginning-position))))

(defun tcomplete-grab-symbol ()
  "Return symbol if point is at the end of it."
  (if (looking-at "\\_>")
      (buffer-substring
       (point)
       (save-excursion (skip-syntax-backward "w_") (point)))
    (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
      "")))

(defun tcomplete-grab-word ()
  "Return word if point is at the end of it."
  (if (looking-at "\\>")
      (buffer-substring
       (point)
       (save-excursion (skip-syntax-backward "w") (point)))
    (unless (and (char-after) (eq (char-syntax (char-after)) ?w))
      "")))

(defun tcomplete-grab-symbol-cons (idle-begin-after-re &optional max-len)
  "Return string SYMBOL or a cons (SYMBOL . t) if text before point matches IDLE-BEGIN-AFTER-RE."
  (let ((symbol (tcomplete-grab-symbol)))
    (when symbol
      (save-excursion
        (forward-char (- (length symbol)))
        (let ((start-point (if max-len
                               (- (point) max-len)
                             (line-beginning-position))))
          (if (looking-back idle-begin-after-re start-point)
              (cons symbol t)
            symbol))))))

(defun tcomplete-in-string-or-comment ()
  "Return non-nil if point is within a string or comment."
  (let ((ppss (syntax-ppss)))
    (or (car (setq ppss (nthcdr 3 ppss)))
        (car (setq ppss (cdr ppss)))
        (nth 3 ppss))))

(defun tcomplete-call-backend (&rest args)
  (tcomplete--force-sync #'tcomplete-call-backend-raw args tcomplete-backend))

(defun tcomplete--force-sync (fun args backend)
  (let ((value (apply fun args)))
    (if (not (eq (car-safe value) :async))
        value
      (let ((res 'trash)
            (start (time-to-seconds)))
        (funcall (cdr value)
                 (lambda (result) (setq res result)))
        (while (eq res 'trash)
          (if (> (- (time-to-seconds) start) tcomplete-async-timeout)
              (error "tcomplete: backend %s async timeout with args %s"
                     backend args)
            (sleep-for tcomplete-async-wait)))
        res))))

(defun tcomplete-call-backend-raw (&rest args)
  (condition-case-unless-debug err
      (if (functionp tcomplete-backend)
          (apply tcomplete-backend args)
        (apply #'tcomplete--multi-backend-adapter tcomplete-backend args))
    (user-error (user-error "tcomplete: backend %s user-error: %s"
                            tcomplete-backend (error-message-string err)))
    (error (error "tcomplete: backend %s error \"%s\" with args %s"
                  tcomplete-backend (error-message-string err) args))))

(defun tcomplete--multi-backend-adapter (backends command &rest args)
  (let ((backends (cl-remove-if-not
                   (lambda (b)
                     (or (keywordp b)
                         (tcomplete-try-init-backend b)))))
        (separate (memq :separate backends)))
    (cl-delete-if #'keywordp backends)
    (pcase command
      (`candidates
       (tcomplete--multi-backend-adapter-candidates backends (car args) separate))
      (`sorted separate)
      (`duplicates (not separate))
      ((or `prefix `ignore-case `no-cache `require-match)
       (let (value)
         (cl-dolist (backend backends)
           (when (and
                  (setq value
                        (tcomplete--force-sync backend (cons command args) backend))
                  (eq command 'ignore-case)
                  (eq value 'keep-prefix))
             (setq value t)))))
      (_
       (let ((arg (car args)))
         (when (> (length arg) 0)
           (let ((backend (or (get-text-property 0 'tcomplete-backend arg)
                              (car backends))))
             (apply backend command args))))))))

(defun tcomplete--multi-backend-adapter-candidates (backends prefix separate)
  (let ((pairs (mapcar
                (lambda (backend)
                  (cons (funcall backend 'candidates prefix)
                        (tcomplete--multi-candidates-mapper
                         backend
                         separate
                         (not (eq backend (car backends))))))
                (cl-remove-if
                 (lambda (backend)
                   (equal (tcomplete--prefix-str
                           (let ((tcomplete-backend backend))
                             (tcomplete-call-backend 'prefix)))
                          prefix))))))
    (tcomplete--merge-async pairs (lambda (values) (apply #'append values)))))

(defun tcomplete--multi-candidates-mapper (backend separate tag)
  (lambda (candidates)
    (when separate
      (let ((tcomplete-backend backend))
        (setq candidates (tcomplete--preprocess-candidates candidates))))
    (when tag
      (setq candidates
            (mapcar
             (lambda (str)
               (propertize str 'tcomplete-backend backend))
             candidates)))
    candidates))

(defun tcomplete--merge-async (pairs merger)
  (let ((async (cl-find-if
                (lambda (pair)
                  (eq :async (car-safe (car pair))))
                pairs)))
    (if (not async)
        (funcall merger (mapcar
                         (lambda (pair)
                           (funcall (cdr pair) (car pair)))
                         pairs))
      (cons :async
            (lambda (callback)
              (let (lst
                    (pending (mapcar #'car pairs))
                    (finisher (lambda ()
                                (unless pending
                                  (funcall callback
                                           (funcall merger
                                                    (nreverse lst)))))))
                (dolist (pair pairs)
                  (push nil lst)
                  (let* ((cell lst)
                         (val (car pair))
                         (mapper (cdr pair))
                         (this-finisher (lambda (res)
                                          (setq pending (delq val pending))
                                          (setcar cell (funcall mapper res))
                                          (funcall finisher))))
                    (if (not (eq :async (car-safe val)))
                        (funcall this-finisher val)
                      (let ((fetcher (cdr val)))
                        (funcall fetcher this-finisher))))))))))

(defun tcomplete--prefix-str (prefix)
  (or (car-safe prefix) prefix))

;; TODO: tcomplete--preprocess-candidates



;; TODO: tcomplete-pre-command
;; TODO: tcomplete-post-command

;; TODO: tcomplete-cancel
;; TODO: tcopmlete-abort

;; TODO: tcomplete-select-next-or-abort
;; TODO: tcomplete-select-previous-or-abort

;; TODO: tcomplete-next-page
;; TODO: tcomplete-previous-page

;; TODO: tcomplete-complete-selection
;; TODO: tcomplete-complete-common

;; TODO: tcomplete-show-doc-buffer

;; TODO: tcomplete-search-candidates
;; TODO: tcomplete-filter-candidates
