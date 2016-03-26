
;; Eclim setup
(require 'eclim)
(add-hook 'after-init-hook 'global-eclim-mode)
(global-set-key (kbd "C-S-o") 'eclim-java-import-organize)
(require 'eclimd)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook
	  '(lambda ()
	     (java-mode-indent-annotations-setup)))

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-indent-setup)

(add-hook 'after-init-hook (lambda ()
			     (message "activate-malabar-mode")
			     (activate-malabar-mode)))

(add-hook 'malabar-java-mode-hook 'flycheck-mode)
(add-hook 'malabar-groovy-mode-hook 'flycheck-mode)
