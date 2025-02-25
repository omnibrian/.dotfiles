;; -*- lexical-binding: t -*-

;; cfn-json
(define-derived-mode cfn-json-mode js-mode
  "CFN-JSON"
  "Simple mode to edit JSON CloudFormation templates."
  (setq js-indent-level 2))

(add-to-list 'magic-mode-alist
             '("\\({\n *\\)? *[\"']AWSTemplateFormatVersion" . cfn-json-mode))

;; cfn-yaml
;;
;; depends on yaml-mode (yaml.el)
(define-derived-mode cfn-yaml-mode yaml-mode
  "CFN-YAML"
  "Simple mode to edit YAML CloudFormation templates.")

(add-to-list 'magic-mode-alist
             '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode))

;; cfn-lint
;;
;; depends on flycheck (linting.el)
(flycheck-define-checker cfn-lint
  "AWS CloudFormation linter using cfn-lint.

Install cfn-lint: pip install cfn-lint"
  :command ("cfn-lint" "-f" "parseable" source)
  :error-patterns ((warning line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "W" (one-or-more digit)) ":" (message) line-end)
                   (error line-start (file-name) ":" line ":" column
                          ":" (one-or-more digit) ":" (one-or-more digit) ":"
                          (id "E" (one-or-more digit)) ":" (message) line-end))
  :modes (cfn-json-mode cfn-yaml-mode))

(add-to-list 'flycheck-checkers 'cfn-lint)
(add-hook 'cfn-json-mode-hook 'flycheck-mode)
(add-hook 'cfn-yaml-mode-hook 'flycheck-mode)
