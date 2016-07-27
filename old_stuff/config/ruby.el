;; Ruby Stuff

(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))


(require 'highlight-indentation)
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (highlight-indentation-current-column-mode)
	    (fci-mode)
	    (set-fill-column 90)
            (setq show-trailing-whitespace t)
            ))

(setq ruby-align-chained-calls nil
      ruby-use-smie t)

(defun my-ruby-align-chain (orig-fun kind token)
  (pcase (cons kind token)
    (`(:before . ".")
     (if (smie-rule-sibling-p)
         (when ruby-align-chained-calls
	   (smie-backward-sexp ".")
	   (cons 'column (current-column)))
       (smie-backward-sexp ";")
       (cons 'column (+ (smie-indent-virtual) ruby-indent-level))))
    (`(:before . "+")
       (smie-backward-sexp ";")
     (cons 'column (+ (smie-indent-virtual) ruby-indent-level)))
    (otherwise (funcall orig-fun kind token))))

(advice-add 'ruby-smie-rules :around 'my-ruby-align-chain)

(defun my-sp-ruby-prepipe-handler (orig-fun id action context)
  nil)

(advice-add 'sp-ruby-pre-pipe-handler :around 'my-sp-ruby-prepipe-handler)
