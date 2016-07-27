

(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook
	  '(lambda ()
	     (java-mode-indent-annotations-setup)))

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-indent-setup)
