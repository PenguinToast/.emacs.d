;;; init.el --- Main Emacs init file
;;; Commentary:

;;; Code:
;; (setq stack-trace-on-error t)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/yaml-mode")

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
(tool-bar-mode 0)
(delete-selection-mode 1)

(projectile-global-mode)

(define-global-minor-mode global-idle-highlight-mode
  idle-highlight-mode
  (lambda () (idle-highlight-mode t)))
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

(setq x-select-enable-clipboard t)

;; basic mouse stuff
(xterm-mouse-mode 1)

;;; turn on syntax highlighting
(global-font-lock-mode 1)

(global-flycheck-mode)

(require 'load-directory)
(load-directory "~/.emacs.d/config")

;; LaTeX setup
(defun my-latex-setup ()
  (setq-local fill-column 115))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'my-latex-setup)
(setq reftex-plug-into-AUCTeX t)

(require 'revbufs)
(setq-default fill-column 80)
(require 'fill-column-indicator)
(setq column-number-mode t)
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'c-mode-common-hook 'fci-mode)
(add-hook 'java-mode-hook 'fci-mode)
(add-hook 'lua-mode-hook 'fci-mode)
(add-hook 'json-mode-hook 'fci-mode)

(setq-default indent-tabs-mode nil)

(blink-cursor-mode 1)
(if (eq system-type 'cygwin)
    (set-face-attribute 'default nil :height 80)
    (set-face-attribute 'default nil :height 110))

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

(setq ag-highlight-search t)

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

(icy-mode 1)
;; TODO: Still doesn't work
(defun icicles-rebind-hook ()
  (define-key icicle-mode-map (kbd "C-c `") nil))
(add-hook 'icy-mode-hook 'icicles-rebind-hook)

(global-set-key (kbd "C-x g") 'magit-status)

;; (add-hook 'after-init-hook #'global-flycheck-mode)
(winner-mode 1)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(elpy-enable)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setq ns-function-modifier 'hyper)
  (exec-path-from-shell-initialize)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(company-idle-delay 1)
 '(compilation-message-face (quote default))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(eclim-eclipse-dirs (quote ("/home/william/util/eclipse")))
 '(eclim-executable "/home/william/util/eclipse/eclim")
 '(ede-project-directories (quote ("~/rc/ramcloud")))
 '(fci-rule-color "windowFrameColor")
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
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (wgrep wgrep-ag elpy json-mode hydra use-package exec-path-from-shell web-mode ag js2-mode haml-mode column-marker stylus-mode sws-mode fuzzy-match projectile malabar-mode inf-groovy groovy-mode smartparens highlight-indent-guides robe rvm auctex emacs-eclim seq geiser highlight-parentheses solarized-theme facemenu+ floobits idle-highlight-mode paredit actionscript-mode list-processes+ protobuf-mode magit magit-gh-pulls magit-tramp flycheck workgroups2 win-switch shell-switcher multi-eshell lua-mode icicles gradle-mode glsl-mode company cmake-project cmake-mode)))
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
