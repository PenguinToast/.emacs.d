
(add-to-list 'load-path "~/.emacs.d/ramcloud/")
(load "ramcloud-c-style")
(add-hook 'c-mode-common-hook 'ramcloud-set-c-style)
(add-hook 'c-mode-common-hook 'ramcloud-make-newline-indent)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defconst ramcloud-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
	  (lambda () (c-add-style "ramcloud-protobuf-style" ramcloud-protobuf-style t)))

(add-hook 'c-mode-common-hook
	 (lambda ()
	   (when (derived-mode-p 'c-mode 'c++-mode)
	     (subword-mode 1))))

(add-hook 'c-mode-common-hook
	 (lambda ()
	   (when (derived-mode-p 'c-mode 'c++-mode)
	     (flycheck-select-checker 'c/c++-gcc))))
