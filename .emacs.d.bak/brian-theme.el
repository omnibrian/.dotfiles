;;; brian-theme.el --- Custom Emacs theme

;;; Commentary:
;;; Brian's custom theme for Emacs

;;; Code:
(deftheme brian
  "Brian's custom theme")

(let ((black         "#000000")  ;; was "#101114"
      (white         "#C8CCD5")
      (red           "#E06C75")
      (green         "#99C37A")
      (yellow        "#E5C07C")
      (blue          "#61AFEF")
      (purple        "#C877DD")
      (teal          "#56B6C4")
      (light-gray    "#ABB1BF")
      (dark-gray     "#545863")
      (dark-yellow   "#85601C")
      (hl-background "#272931"))
  (custom-theme-set-faces
   'brian
   `(default                             ((t (:background ,black :foreground ,white :height 100))))
   `(cursor                              ((t (:background ,white :foreground ,black))))
   `(escape-glyph                        ((t (:foreground ,teal))))
   `(homoglyph                           ((t (:foreground ,teal))))
   `(minibuffer-prompt                   ((t (:foreground ,teal))))
   `(highlight                           ((t (:background ,dark-yellow))))
   `(region                              ((t (:background ,white :foreground ,black))))
   `(shadow                              ((t (:foreground ,light-gray))))
   `(secondary-selection                 ((t (:background ,dark-gray))))
   `(trailing-whitespace                 ((t (:background ,red))))
   `(font-lock-builtin-face              ((t (:foreground ,yellow))))
   `(font-lock-comment-delimiter-face    ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face              ((t (:foreground ,teal))))
   `(font-lock-constant-face             ((t (:foreground ,blue :weight bold))))
   `(font-lock-doc-face                  ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face        ((t (:foreground ,teal :weight bold))))
   `(font-lock-keyword-face              ((t (:foreground ,yellow))))
   `(font-lock-negation-char-face        ((t nil)))
   `(font-lock-preprocessor-face         ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face               ((t (:foreground ,purple))))
   `(font-lock-type-face                 ((t (:foreground ,green))))
   `(font-lock-variable-name-face        ((t (:foreground ,yellow))))
   `(font-lock-warning-face              ((t (:inherit (error)))))
   `(button                              ((t (:inherit (link)))))
   `(link                                ((t (:foreground ,teal :underline t))))
   `(link-visited                        ((t (:inherit link :foreground ,purple))))
   `(fringe                              ((t (:background ,dark-gray))))
   `(header-line                         ((t (:inherit mode-line :background ,dark-gray :foreground ,white :box nil))))
   `(tooltip                             ((t (:inherit variable-pitch :background ,light-gray :foreground ,black))))
   `(mode-line                           ((t (:box (:line-width -1 :style released-button) :foreground ,white :background ,dark-gray))))
   `(mode-line-buffer-id                 ((t (:weight bold))))
   `(mode-line-emphasis                  ((t (:weight bold))))
   `(mode-line-highlight                 ((t (:box (:line-width 2 :color ,dark-gray :style released-button)))))
   `(mode-line-inactive                  ((t (:weight light :box (:line-width -1 :color ,light-gray) :foreground ,white :background ,dark-gray :inherit mode-line))))
   `(isearch                             ((t (:background ,yellow :foreground ,black))))
   `(isearch-fail                        ((t (:background ,red :foreground ,black))))
   `(lazy-highlight                      ((t (:background ,teal :foreground ,black))))
   `(match                               ((t (:background ,blue :foreground ,black))))
   `(next-error                          ((t (:inherit (region)))))
   `(query-replace                       ((t (:inherit (isearch)))))
   `(hl-line                             ((t (:background ,hl-background))))))

(provide-theme 'brian)

(provide 'brian-theme)
;;; brian-theme.el ends here
