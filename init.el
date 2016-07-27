;;; init.el --- Main Emacs init file
;;; Commentary:
;; Some commentary

;;; Code:
;; `package' initialization
(require 'package)
(add-to-list 'package-archives
	     '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Packages:
;; Custom Before:
(use-package misc-config
  :load-path "lisp/")

(use-package mac-config
  :if (eq system-type 'darwin)
  :load-path "lisp/")

(use-package my-functions
  :load-path "lisp/")

;; site-lisp:
(use-package revbufs
  :load-path "site-lisp/")

;; Themes:

(use-package solarized-theme
  :ensure t)

;; This one was causing problems.
(use-package twilight-bright-theme
  :disabled
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :disabled
  :ensure t)

(use-package leuven-theme
  :disabled
  :ensure t)

(use-package hemisu-theme
  :disabled
  :ensure t)

(use-package material-theme
  :disabled
  :ensure t)

;; General:

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package idle-highlight-mode
  :ensure t
  :config
  (define-global-minor-mode global-idle-highlight-mode
    idle-highlight-mode
    (lambda () (idle-highlight-mode t)))
  (global-idle-highlight-mode 1))

(use-package seq
  :ensure t)

(use-package company
  :ensure t
  :config (global-company-mode)
  :bind ("M-/" . company-complete)
  )

(use-package projectile
  :ensure t
  :config (projectile-global-mode))

(use-package fuzzy-match
  :ensure t)

(use-package icicles
  :ensure t
  :config
  (icy-mode 1))

(use-package win-switch
  :ensure t
  :config
  (win-switch-authors-configuration)
  (setq win-switch-other-window-first
        (lambda ()
          (null (nthcdr 3 (window-list))))))

(use-package workgroups2
  :ensure t
  :config
  (setq wg-prefix-key (kbd "C-z"))
  (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
  (setq wg-emacs-exit-save-behavior 'save)
  (setq wg-workgroups-mode-exit-save-behavior 'save)
  (setq wg-mode-line-display-on t)
  (setq wg-flag-modified t)
  (setq wg-mode-line-decor-left-brace "["
        wg-mode-line-decor-right-brace "]"  ; how to surround it
        wg-mode-line-decor-divider ":")
  (workgroups-mode 1)
  )

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package magit-gh-pulls
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package hydra
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (defhydra smartparens-hydra ()
    "Smartparens"
    ("d" sp-down-sexp "Down")
    ("e" sp-up-sexp "Up")
    ("u" sp-backward-up-sexp "Up")
    ("a" sp-backward-down-sexp "Down")
    ("f" sp-forward-sexp "Forward")
    ("b" sp-backward-sexp "Backward")
    ("k" sp-kill-sexp "Kill" :color blue)
    ("q" nil "Quit" :color blue))
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)

        ("C-M-d" . sp-down-sexp)
        ("C-M-a" . sp-backward-down-sexp)
        ("C-S-d" . sp-beginning-of-sexp)
        ("C-S-a" . sp-end-of-sexp)

        ("C-M-e" . sp-up-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-t" . sp-transpose-sexp)

        ("C-M-n" . sp-next-sexp)
        ("C-M-p" . sp-previous-sexp)

        ("C-M-k" . sp-kill-sexp)
        ("C-M-w" . sp-copy-sexp)

        ("M-<delete>" . sp-unwrap-sexp)
        ("M-<backspace>" . sp-backward-unwrap-sexp)

        ("C-<right>" . sp-forward-slupr-sexp)
        ("C-<left>" . sp-forward-barf-sexp)
        ("C-M-<left>" . sp-backward-slurp-sexp)
        ("C-M-<right>" . sp-backward-barf-sexp)

        ("M-D" . sp-splice-sexp)
        ("C-M-<delete>" . sp-splice-sexp-killing-forward)
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" . sp-splice-sexp-killing-around)

        ("C-]" . sp-select-next-thing)
        ("<C-[>" . sp-select-previous-thing-exchange)
        ("C-M-]" . sp-select-next-thing-exchange)
        ("M-ESC" . sp-select-previous-thing) ; This is technically C-M-[

        ("M-F" . sp-forward-symbol)
        ("M-B" . sp-backward-symbol)

        ("C-M-s" . smartparens-hydra/body))
  :config
  (require 'smartparens-config)
  (require 'smartparens-lua)
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  )

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t))

(use-package highlight-indentation
  :ensure t)

(use-package dired
  :config
  (use-package dired-x
    :bind ("C-x C-j" . dired-jump))
  (use-package dired-details+
    :ensure t))

;; Language:

;; Python
(use-package elpy
  :pin elpy
  :ensure t
  :config
  (require 'smartparens-python)
  (elpy-enable))

;; Ruby
(use-package rvm
  :ensure t
  :config
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

(use-package ruby-mode
  :ensure t
  :init
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
  :mode ("\\.\\(gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|thor\\)\\'"
         "Gemfile\\(\\.lock\\)?\\|\\(Cap\\|Guard\\|[rR]ake\\|Vagrant\\)file\\'")
  :interpreter "ruby"
  :config
  (require 'smartparens-ruby)
  (setq ruby-align-chained-calls nil
	ruby-use-smie t)
  (advice-add 'ruby-smie-rules :around 'my-ruby-align-chain)
  (add-hook
   'ruby-mode-hook
   (lambda ()
     (robe-mode)
     (highlight-indentation-current-column-mode)
     (setq show-trailing-whitespace t)
     (set-fill-column 90)))
  )

(use-package robe
  :ensure t
  :commands (robe-mode)
  :config
  (robe-start t)
  (push 'company-robe company-backends))

;; LaTeX
;; TODO: Put this in use-package LaTeX-mode
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (visual-line-mode)
              (LaTeX-math-mode)
              (turn-on-reftex)
              (turn-on-auto-fill)
              (set-fill-column 115))))

;; Web
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")

(use-package js2-mode
  :ensure t)

(use-package stylus-mode
  :ensure t
  :mode "\\.styl\\'")

(use-package json-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.js?\\'" "\\.hbs\\'" "\\.css\\'" "\\.tpl\\'")
  :config
  (setq web-mode-content-types-alist
        '(("jsx"  . "/affinity/assets/javascripts/.*\\.js[x]?\\'")
          ("underscorejs"  . ".*\\.tpl\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-enable-auto-pairing nil)
  (add-hook 'web-mode-hook
            (lambda ()
              (set-fill-column 90)
              (setq show-trailing-whitespace t)
              )))

;; Lua
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config
  (setq lua-indent-level 2))

;; Custom after

(use-package my-flycheck-checkers
  :load-path "lisp/")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "c36614262f32c16cd71e0561a26e5c02486b6a476a6adec7a5cc5582128e665e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(fci-rule-character-color "#d9d9d9")
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (material-theme hemisu-theme leuven-theme color-theme-sanityinc-tomorrow dired-details+ yaml-mode workgroups2 win-switch web-mode use-package stylus-mode solarized-theme smartparens rvm robe projectile magit-gh-pulls lua-mode list-processes+ json-mode js2-mode idle-highlight-mode icicles hydra highlight-indent-guides haml-mode geiser fuzzy-match flycheck facemenu+ exec-path-from-shell elpy column-marker auctex ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
