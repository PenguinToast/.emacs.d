
;; lua mode 2 spaces for an indent
(setq lua-indent-level 2)
(add-hook 'json-mode-hook
	  (lambda ()
	     (setq js-indent-level 2)))

;; json-mode files for Starbound modding
(add-to-list 'auto-mode-alist
	     '("Starbound.*\\.\\([^l]\\|l\\($\\|[^u]\\|u\\($\\|[^a]\\)\\)\\)" . json-mode))
