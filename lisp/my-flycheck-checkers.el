;;; my-flycheck-checkers.el --- Custom flycheck checkers.
;;; Commentary:

;;; Code:

(defun my-npm-eslint-error-filter (errors)
  "Copy of eslint error filter."
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

(provide 'my-flycheck-checkers)
;;; my-flycheck-checkers.el ends here
