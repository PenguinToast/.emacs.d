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

(use-package goto-last-change
  :load-path "site-lisp/"
  :bind ("C-x C-\\" . goto-last-change))

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

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer-other-window))

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

(use-package pos-tip
  :ensure t)

(use-package company
  :ensure t
  :bind ("M-/" . company-complete)
  :config
  (global-company-mode)
  (use-package company-quickhelp
    :ensure t
    :bind (:map company-active-map
                ("M-h" . company-quickhelp-manual-begin))
    :config
    (company-quickhelp-mode 1))
  (use-package company-go
    :ensure t)
  )

(use-package projectile
  :ensure t
  :config (projectile-mode))

(use-package fuzzy-match
  :ensure t)

(use-package icicles
  :ensure t
  :config
  (icy-mode 1))

(use-package ivy
  :ensure t
  :demand
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq projectile-completion-system 'ivy)
  (use-package counsel
    :ensure t
    :bind (("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           :map read-expression-map
           ("C-r" . counsel-expression-history))
    :config
    (use-package counsel-projectile
      :ensure t
      :config
      (counsel-projectile-mode))
    )
  (use-package ivy-hydra
    :ensure t)
  )

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
  (run-with-idle-timer 30 t
                       (lambda ()
                         (unless (minibufferp)
                           (let ((inhibit-message t))
                             (wg-save-session)))
                         ))
  )

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq git-commit-fill-column 72))

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
  ;; Java generics
  (defun not-sp-point-after-word-p (&rest args)
    (not (apply 'sp-point-after-word-p args)))
  (sp-with-modes 'jdee-mode
    (sp-local-pair "<" ">" :when '(sp-point-after-word-p))
    )
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

;; Bison
(use-package bison-mode
  :ensure t)

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
  :disabled
  :config
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

(use-package ruby-mode
  :ensure t
  :disabled
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
  :disabled
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

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.js?\\'"
         "\\.jsx\\'"
         "\\.hbs\\'"
         "\\.css\\'"
         "\\.tpl\\'"
         "\\.json\\'"
         "\\.eslintrc\\'")
  :config
  (setq web-mode-content-types-alist
        '(("jsx"  . "/affinity/assets/javascripts/.*\\.js[x]?\\'")
          ("jsx"  . "/workspace/mobile/.*\\.js[x]?\\'")
          ("json" . "\\.eslintrc\\'")
          ("underscorejs"  . ".*\\.tpl\\'")))
  (defvaralias 'standard-indent 'tab-width)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-enable-auto-pairing nil)
  (add-hook 'web-mode-hook
            (lambda ()
              (set-fill-column 90)
              (setq show-trailing-whitespace t)
              ;; (setq web-mode-markup-indent-offset tab-width)
              ;; (setq web-mode-css-indent-offset tab-width)
              ;; (setq web-mode-code-indent-offset tab-width)
              ;; (setq web-mode-attr-indent-offset tab-width)
              )))

;; Elm
(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :config
  (use-package flycheck-elm
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
  (setq elm-tags-on-save t)
  (defvaralias 'elm-indent-offset 'tab-width)
  (add-to-list 'company-backends 'company-elm)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  )

;; Lua
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config
  (defvaralias 'lua-indent-level 'tab-width))

;; Java
(use-package jdee
  :ensure t
  ;:mode ("\\.java\\'" . jdee-mode)
  :bind (:map jdee-mode-map
              ("M-." . jdee-open-class-at-point))
  :config
  (setq jdee-server-dir "/home/william/package_source/jdee-server/target"))

;; Research
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-log-done t))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              ("M-." . godef-jump))
  :config
  (add-hook
   `go-mode-hook
   (lambda ()
     (setq-local indent-tabs-mode t)))
  )

(use-package flycheck-gometalinter
  :disabled
  :ensure t
  :config
  ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  (setq-default flycheck-gometalinter-vendor t)
  ;; only run fast linters
  (setq-default flycheck-gometalinter-fast t)
  ;; use in tests files
  (setq-default flycheck-gometalinter-test t)
  (progn
    (flycheck-gometalinter-setup)))

(use-package rtags
  :ensure t
  :bind (:map c-mode-base-map
              ("M-." . rtags-find-symbol-at-point)
              ("M-," . rtags-find-references-at-point)
              ("C-." . rtags-find-symbol)
              ("C-," . rtags-find-references)
              ("C-c r s" . rtags-display-summary))
  :config
  (defun setup-flycheck-rtags ()
    (interactive)
    (flycheck-select-checker 'rtags)
    ;; RTags creates more accurate overlays.
    (setq-local flycheck-highlighting-mode nil)
    (setq-local flycheck-check-syntax-automatically nil))

  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
  (rtags-enable-standard-keybindings)
  ;; company completion setup
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (use-package company-rtags
    :ensure t)
  (push 'company-rtags company-backends)
  ;; use rtags flycheck mode -- clang warnings shown inline
  (use-package flycheck-rtags
    :ensure t)
  (require 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags)
  )

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

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
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "c36614262f32c16cd71e0561a26e5c02486b6a476a6adec7a5cc5582128e665e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(dired-details-initially-hide nil)
 '(fci-rule-character-color "#d9d9d9")
 '(flycheck-gometalinter-fast t)
 '(flycheck-gometalinter-tests t)
 '(flycheck-gometalinter-vendor t)
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (bison-mode dockerfile-mode counsel-projectile ivy-hydra counsel jdee org company-go go-mode flycheck-elm material-theme hemisu-theme leuven-theme color-theme-sanityinc-tomorrow dired-details+ yaml-mode workgroups2 win-switch web-mode use-package stylus-mode solarized-theme smartparens rvm robe projectile magit-gh-pulls lua-mode list-processes+ js2-mode idle-highlight-mode icicles hydra highlight-indent-guides haml-mode geiser fuzzy-match flycheck facemenu+ exec-path-from-shell elpy column-marker auctex ag)))
 '(safe-local-variable-values
   (quote
    ((flycheck-gcc-include-path "/home/william/workspace/BerkeleyCS/cs162/code/group/pintos/src" "/home/william/workspace/BerkeleyCS/cs162/code/group/pintos/src/lib" "/home/william/workspace/BerkeleyCS/cs162/code/group/pintos/src/lib/user" "/home/william/workspace/BerkeleyCS/cs162/code/group/pintos/src/lib/kernel")
     (projectile-project-compilation-cmd . "go install")
     (flycheck-gcc-include-path . "/home/william/workspace/BerkeleyCS/cs162/code/group/pintos/src")
     (projectile-project-name . "cs186-project"))))
 '(sp-escape-quotes-after-insert nil)
 '(tags-revert-without-query t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
