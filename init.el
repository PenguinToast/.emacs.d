;; (setq stack-trace-on-error t)

(set-face-attribute 'default nil :height 90)

(add-to-list 'load-path "~/.emacs.d/lisp")

(delete-selection-mode 1)
;(add-hook 'lua-mode-hook 'ggtags-mode)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'auto-complete-config)
(ac-config-default)
;; (setq-default ac-sources (append ac-sources '(ac-source-semantic)))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

(add-hook 'dired-load-hook
          (lambda () (load "dired-x")))
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

;; lua mode 2 spaces for an indent 
(setq lua-indent-level 2)
(add-hook 'json-mode-hook
          (lambda ()
             (setq js-indent-level 2)))

;; json-mode files for Starbound modding
(add-to-list 'auto-mode-alist
             '("Starbound.*\\.\\([^l]\\|l\\($\\|[^u]\\|u\\($\\|[^a]\\)\\)\\)" . json-mode))

(setq x-select-enable-clipboard t)

;; basic mouse stuff
(xterm-mouse-mode 1)

(add-to-list 'load-path "~/.emacs.d/ramcloud/")
(load "ramcloud-c-style")
(add-hook 'c-mode-common-hook 'ramcloud-set-c-style)
(add-hook 'c-mode-common-hook 'ramcloud-make-newline-indent)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; turn on syntax highlighting 
(global-font-lock-mode 1) 
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;;; use groovy-mode when file ends in .groovy or .gradle or has #!/bin/groovy at
;;; start 
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t) 
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
;;; make Groovy mode electric by default. 
(add-hook 'groovy-mode-hook 
          '(lambda () 
             (require 'groovy-electric) 
             (groovy-electric-mode)))

(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook
          '(lambda ()
             (java-mode-indent-annotations-setup)))

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-indent-setup)

(defadvice kill-region (before unix-werase activate compile)
      "When called interactively with no active region, delete a single word
    backwards instead."
      (interactive
       (if mark-active (list (region-beginning) (region-end))
         (list (save-excursion (backward-word 1) (point)) (point)))))

(require 'revbufs)
(setq-default fill-column 80)
(require 'fill-column-indicator)
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'c-mode-common-hook 'fci-mode)
(add-hook 'java-mode-hook 'fci-mode)
(add-hook 'lua-mode-hook 'fci-mode)
(add-hook 'json-mode-hook 'fci-mode)

(blink-cursor-mode 1)
(set-face-attribute 'default nil :height 100)

(require 'win-switch)
(win-switch-authors-configuration)
(setq win-switch-other-window-first
      (lambda ()
          (null (nthcdr 3 (window-list)))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)

;; Workgroups2
(require 'workgroups2)
;; Change some settings
(setq wg-prefix-key (kbd "C-z"))
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")
;; What to do on Emacs exit / workgroups-mode exit?
(setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
(setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil
(setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
(setq wg-flag-modified t)                 ; Display modified flags as well
(setq wg-mode-line-decor-left-brace "["
      wg-mode-line-decor-right-brace "]"  ; how to surround it
      wg-mode-line-decor-divider ":")
(workgroups-mode 1)

(setq explicit-shell-file-name "/bin/bash")
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

(add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)
(add-hook 'term-mode-hook 'shell-switcher-manually-register-shell)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote light))
 '(shell-switcher-mode t)
 '(shell-switcher-new-shell-function (quote shell-switcher-make-ansi-term)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

