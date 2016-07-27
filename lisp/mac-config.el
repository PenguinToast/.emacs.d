;;; mac-config.el --- Custom configuration for macs
;;; Commentary:

;;; Code:

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'meta)
(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
(setq ns-function-modifier 'hyper)

(provide 'mac-config)
;;; mac-config.el ends here
