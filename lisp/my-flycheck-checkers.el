;;; my-flycheck-checkers.el --- Custom flycheck checkers.
;;; Commentary:

;;; Code:

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(flycheck-add-mode 'javascript-eslint 'web-mode)

(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  "Taken from https://github.com/flycheck/flycheck/issues/1762#issuecomment-750458442."
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'typescript-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (javascript-eslint)))))))))

;; (flycheck-add-mode 'typescript-tslint 'web-mode)

(provide 'my-flycheck-checkers)
;;; my-flycheck-checkers.el ends here
