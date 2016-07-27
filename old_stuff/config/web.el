;; Web-mode
(require 'web-mode)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-enable-auto-pairing nil)
  (setq show-trailing-whitespace t)
  (setq fill-column 90)
  (column-marker-1 90)
  )

(add-hook 'web-mode-hook  'my-web-mode-hook)

(defun sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

(defun my-npm-eslint-error-filter (errors)
  (seq-do (lambda (err)
            ;; Parse error ID from the error message
            (setf (flycheck-error-message err)
                  (replace-regexp-in-string
                   (rx " ("
                       (group (one-or-more (not (any ")"))))
                       ")" string-end)
                   (lambda (s)
                     (setf (flycheck-error-id err)
                           (match-string 1 s))
                     "")
                   (flycheck-error-message err))))
          (flycheck-sanitize-errors errors))
  errors)

(flycheck-define-checker npm-eslint
  "eslint, but run with npm"
  :command ("npm" "run" "--silent" "--" "eslint" "--format=checkstyle"
            (config-file "--config" flycheck-eslintrc)
            (option "--rulesdir" flycheck-eslint-rulesdir)
            "--stdin" "--stdin-filename" source-original)
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :error-filter my-npm-eslint-error-filter
  :modes (web-mode)
  :predicate (lambda () (member web-mode-content-type '("javascript" "jsx"))))
(add-to-list 'flycheck-checkers 'npm-eslint)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(setq web-mode-content-types-alist
      '(("jsx"  . "/affinity/assets/javascripts/.*\\.js[x]?\\'")
        ("underscorejs"  . ".*\\.tpl\\'")))
