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
(add-to-list 'package-archives
	     '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(tool-bar-mode -1)
(define-global-minor-mode global-idle-highlight-mode ;;the name of the new global mode
  idle-highlight-mode ;;the name of the minor mode you want to turn on
  (lambda () (idle-highlight-mode t) ;;the function to turn on the mode
    ))
(global-idle-highlight-mode 1)

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
(set-face-attribute 'default nil :height 80)

(require 'win-switch)
(win-switch-authors-configuration)
(setq win-switch-other-window-first
      (lambda ()
	  (null (nthcdr 3 (window-list)))))

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

;; (setq explicit-shell-file-name "/bin/bash")
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

;; (require 'icicles)
(icy-mode 1)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
;; (require 'magit)

;; (with-eval-after-load 'info
 ;; (info-initialize)
 ;; (add-to-list 'Info-directory-list
	       ;; "~/.emacs.d/lib/magit/Documentation/"))
(global-set-key (kbd "C-x g") 'magit-status)

;; (add-hook 'before-save-hook 'whitespace-cleanup)

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

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(elpy-enable)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-xcode company-cmake company-capf
		  (company-dabbrev-code company-gtags company-etags company-keywords)
		  company-oddmuse company-files company-dabbrev)))
 '(company-idle-delay 1)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
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
 '(ede-project-directories (quote ("~/rc/ramcloud")))
 '(fci-rule-color "#eee8d5")
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoc c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck haml handlebars haskell-ghc haskell-hlint html-tidy javascript-jshint javascript-eslint javascript-gjslint javascript-jscs javascript-standard json-jsonlint less luacheck lua perl perl-perlcritic php php-phpmd php-phpcs puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint rst rst-sphinx ruby-rubocop ruby-rubylint ruby ruby-jruby rust sass scala scala-scalastyle scss-lint scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-gcc-language-standard "c++11")
 '(frame-background-mode (quote light))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(icicle-buffer-include-recent-files-nflag 5)
 '(icicle-find-file-expand-directory-flag t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (solarized-theme elpy facemenu+ floobits idle-highlight-mode paredit groovy-mode actionscript-mode list-processes+ protobuf-mode magit magit-gh-pulls magit-tramp flycheck workgroups2 win-switch shell-switcher multi-eshell lua-mode json-mode icicles gradle-mode glsl-mode company cmake-project cmake-mode)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(python-shell-interpreter "python3")
 '(shell-switcher-mode t)
 '(shell-switcher-new-shell-function (quote shell-switcher-make-eshell))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
