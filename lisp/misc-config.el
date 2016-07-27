;;; misc-config.el --- Miscellaneous custom config
;;; Commentary:

;;; Code:

;; Basic customizations
(menu-bar-mode 0)
(tool-bar-mode 0)
(delete-selection-mode 1)
(setq x-select-enable-clipboard t)
(global-font-lock-mode 1)
(xterm-mouse-mode 1)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(blink-cursor-mode 1)
(setq-default fill-column 80)
(set-face-attribute 'default nil :height 110)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(winner-mode 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Binding C-[
(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))

;; Theme stuff
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(provide 'misc-config)
;;; misc-config.el ends here
