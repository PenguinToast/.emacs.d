;; (setq stack-trace-on-error t)

(add-to-list 'load-path "~/.emacs.d/lisp")
(load-file "~/.emacs.d/config/cedet.el")
(add-to-list 'load-path (expand-file-name
      "~/.emacs.d/site-lisp/ecb/"))
(require 'ecb)
(setq ecb-tip-of-the-day nil)

(delete-selection-mode 1)
;(add-hook 'lua-mode-hook 'ggtags-mode)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(add-hook 'dired-load-hook
          (lambda () (load "dired-x")))
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-/") 'company-complete)

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

(defconst ramcloud-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "ramcloud-protobuf-style" ramcloud-protobuf-style t)))

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
(set-face-attribute 'default nil :height 110)

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

(add-hook 'c-mode-common-hook
         (lambda ()
           (when (derived-mode-p 'c-mode 'c++-mode)
             (subword-mode 1))))

(add-hook 'c-mode-common-hook
         (lambda ()
           (when (derived-mode-p 'c-mode 'c++-mode)
             (flycheck-select-checker 'c/c++-gcc))))

;(add-hook 'c-mode-common-hook
;          (lambda ()
;            (when (derived-mode-p 'c-mode 'c++-mode)
;              (ggtags-mode 1))))

;(require 'icicles)
;(icy-mode 1)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
;; (require 'magit)

;; (with-eval-after-load 'info
 ;; (info-initialize)
 ;; (add-to-list 'Info-directory-list
	       ;; "~/.emacs.d/lib/magit/Documentation/"))
(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq swapping-buffer nil)
(setq swapping-window nil)
(defun swap-buffers-in-windows ()
  "Swap buffers between two windows"
  (interactive)
  (if (and swapping-window
           swapping-buffer)
      (let ((this-buffer (current-buffer))
            (this-window (selected-window)))
        (if (and (window-live-p swapping-window)
                 (buffer-live-p swapping-buffer))
            (progn (switch-to-buffer swapping-buffer)
                   (select-window swapping-window)
                   (switch-to-buffer this-buffer)
                   (select-window this-window)
                   (message "Swapped buffers."))
          (message "Old buffer/window killed.  Aborting."))
        (setq swapping-buffer nil)
        (setq swapping-window nil))
    (progn
      (setq swapping-buffer (current-buffer))
      (setq swapping-window (selected-window))
      (message "Buffer and window marked for swapping."))))
(global-set-key (kbd "C-x p") 'swap-buffers-in-windows)

(add-hook 'after-init-hook #'global-flycheck-mode)
(winner-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-xcode company-cmake company-capf
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(company-idle-delay 1)
 '(ecb-layout-name "left3")
 '(ecb-layout-window-sizes
   (quote
    (("left3"
      (ecb-directories-buffer-name 0.20165745856353592 . 0.29)
      (ecb-sources-buffer-name 0.20165745856353592 . 0.35)
      (ecb-methods-buffer-name 0.20165745856353592 . 0.35)))))
 '(ecb-options-version "2.40")
 '(ecb-source-path
   (quote
    (("~/rc/ramcloud/src" "ramcloud")
     ("~/rc/ramcloud/src/btreeRamCloud" "ramcloud_btree"))))
 '(ede-project-directories (quote ("/home/william/rc/ramcloud")))
 '(flycheck-c/c++-gcc-executable "gcc-4.9")
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoc c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck haml handlebars haskell-ghc haskell-hlint html-tidy javascript-jshint javascript-eslint javascript-gjslint javascript-jscs javascript-standard json-jsonlint less luacheck lua perl perl-perlcritic php php-phpmd php-phpcs puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint rst rst-sphinx ruby-rubocop ruby-rubylint ruby ruby-jruby rust sass scala scala-scalastyle scss-lint scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-gcc-include-path
   (quote
    ("/home/william/rc/ramcloud/obj.transaction-slik/" "/home/william/rc/ramcloud/gtest/include/")))
 '(flycheck-gcc-language-standard "c++11")
 '(frame-background-mode (quote light))
 '(package-selected-packages
   (quote
    (actionscript-mode list-processes+ protobuf-mode magit magit-gh-pulls magit-tramp flycheck workgroups2 win-switch starter-kit shell-switcher multi-eshell lua-mode json-mode icicles gradle-mode glsl-mode company cmake-project cmake-mode)))
 '(shell-switcher-mode t)
 '(shell-switcher-new-shell-function (quote shell-switcher-make-eshell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
