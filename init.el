;;; init.el --- Main Emacs init file
;;; Commentary:
;; Some commentary

;;; Code:
;; `package' initialization

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; Bootstrap `use-package'
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

;; Packages:
;; Custom Before:
(use-package misc-config
  :straight nil
  :load-path "lisp/")

(use-package mac-config
  :straight nil
  :if (eq system-type 'darwin)
  :load-path "lisp/")

(use-package my-functions
  :straight nil
  :load-path "lisp/")

(use-package google-c-style
  :straight nil
  :load-path "lisp/"
  :hook (c-mode-common . google-set-c-style))

(use-package magit-workgroups
  :straight nil
  :disabled
  :load-path "lisp/"
  :after (magit projectile workgroups2))

;; site-lisp:
(use-package revbufs
  :straight nil
  :load-path "site-lisp/")

(use-package goto-last-change
  :straight nil
  :load-path "site-lisp/"
  :bind ("C-x C-\\" . goto-last-change))

(use-package prolog
  :straight nil
  :load-path "site-lisp/"
  :mode "\\.pl\\'"
  :bind (:map prolog-mode-map
              ("C-c C-e" . ediprolog-dwim))
  :config
  (setq prolog-system 'swi)
  (use-package ediprolog
    :ensure t))

;; Themes:
(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-solarized-light t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package doom-modeline
  :ensure t
  :after minions
  :hook (after-init . doom-modeline-mode)
  :config
  ;; Needed for doom-modeline
  (add-to-list 'global-mode-string '(t (:eval (wg-mode-line-string)))))

(use-package solarized-theme
  :disabled
  :ensure t)

;; General:

(use-package ibuffer)

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
  ;; :ensure t
  )

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

(use-package company-box
  :disabled
  :ensure t
  :after company
  :hook (company-mode . company-box-mode))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package fuzzy-match
  :ensure t)

(use-package ivy
  :ensure t
  :demand
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)
         ("C-c C-r" . ivy-resume))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (projectile-completion-system 'ivy)
  :config
  (ivy-mode 1)
  (setq magit-completing-read-function 'ivy-completing-read)
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

(use-package perspective
  :ensure t
  :demand
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*)
         ("C-x C-b" . persp-ibuffer)
         :map perspective-map
         ("C-s" . (lambda () (interactive) (persp-state-save persp-state-default-file)))
         ("1" . (lambda () (interactive) (persp-switch-by-number 2)))
         ("2" . (lambda () (interactive) (persp-switch-by-number 3)))
         ("3" . (lambda () (interactive) (persp-switch-by-number 4)))
         ("4" . (lambda () (interactive) (persp-switch-by-number 5)))
         ("5" . (lambda () (interactive) (persp-switch-by-number 6)))
         ("6" . (lambda () (interactive) (persp-switch-by-number 7)))
         ("7" . (lambda () (interactive) (persp-switch-by-number 8)))
         ("8" . (lambda () (interactive) (persp-switch-by-number 9)))
         ("9" . (lambda () (interactive) (persp-switch-by-number 10)))
         ("0" . (lambda () (interactive) (persp-switch-by-number 1))))
  :custom
  (persp-state-default-file "~/.emacs.d/.perspective_saved")
  (persp-mode-prefix-key (kbd "C-z"))
  (persp-modestring-short t)
  :config
  (persp-mode)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (persp-state-load persp-state-default-file))

(use-package workgroups2
  :straight (workgroups2 :type git :host github :repo "pashinin/workgroups2"
                         :files (:defaults "src/*.el")
                         :fork (:host github
                                      :repo "PenguinToast/workgroups2"))
  :disabled
  :init
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

(use-package forge
  :ensure t
  :after magit)

(use-package git-link
  :ensure t
  :bind (("C-c g l" . git-link)
         ("C-c g c" . git-link-commit)))

(use-package hydra
  :ensure t)

(use-package smartparens
  :ensure t
  :defer 1
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

        ("C-<right>" . sp-forward-slurp-sexp)
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
  (require 'smartparens-python)
  (smartparens-global-mode t)
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
  :custom
  (ag-highlight-search t))

(use-package winnow
  :ensure t
  :after ag
  :hook (ag-mode . winnow-mode))

(use-package highlight-indentation
  :ensure t
  :hook ((yaml-mode . highlight-indentation-mode)
         (yaml-mode . highlight-indentation-current-column-mode)))

(use-package dired
  :straight nil
  :config
  (use-package dired-x
    :straight nil
    :bind ("C-x C-j" . dired-jump)))

(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode . lsp)
         (python-mode . lsp)
         (go-mode . lsp)
         (terraform-mode . lsp)
         (rustic-mode . lsp)
         (web-mode . lsp)
         (dockerfile-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-keymap-prefix "C-c l")
  :config
  (willsheu/lsp-pylsp-setup)
  (willsheu/lsp-ts-ls-setup)
  (willsheu/lsp-rust-setup)
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp . lsp-ui-mode)
  )

(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  ;; (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
  )

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode)
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol))

(use-package origami
  :ensure t
  :hook (lsp-mode . origami-mode)
  :bind (:map origami-mode-map
              ("C-<tab>" . origami-recursively-toggle-node)))

(use-package lsp-origami
  :ensure t
  :after (lsp-mode origami)
  :hook (lsp-mode . lsp-origami-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package lsp-treemacs
  :after (treemacs lsp doom-themes)
  :ensure t)

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons lsp-treemacs)
  :ensure t
  :config
  (setq lsp-treemacs-theme "all-the-icons")
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-perspective
  :after (treemacs perspective)
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; Language:

;; GLSL
(use-package glsl-mode
  :ensure t
  :mode ("\\.\\(vert\\|frag\\)\\'"))

(use-package flycheck-glsl
  :straight nil
  :load-path "site-lisp/")

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package flymd
  :ensure t
  :commands (flymd-flyit)
  :config
  (defun my-flymd-browser-function (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  (setq flymd-browser-open-function 'my-flymd-browser-function))

;; C/C++
;; (defun ccls//enable ()
;;   (condition-case nil
;;       (lsp-ccls-enable)
;;     (user-error nil)))

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-executable "/home/william/package_src/ccls/Release/ccls"))

;; Bison
(use-package bison-mode
  :ensure
  :mode ("\\.\\(l\\|y\\)\\'"))

;; Rust
;; (use-package rust-mode
;;   :ensure
;;   :mode ("\\.rs\\'")
;;   :config
;;   (setq rust-format-on-save t))

(use-package rustic
  :ensure
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

;; Python

(use-package pipenv
  :ensure t
  :init
  (setq pipenv-with-flycheck nil)
  (setq pipenv-with-projectile nil))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode))

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

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  ;; (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  ;; (company-mode +1)
  )

;; Web
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'")
  :config
  (setq-default typescript-indent-level 2))

(use-package tide
  :ensure t
  :disabled
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;; (before-save . tide-format-before-save)
         )
  :config
  (defadvice tide-jump-to-definition
      (before advice-tide-jump activate)
    (push-mark))
  )

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
         "\\.eslintrc\\'"
         "\\.tsx\\'")
  :init
  (makunbound 'standard-indent)
  (defvaralias 'standard-indent 'tab-width)
  (unintern 'web-mode-markup-indent-offset)
  (defvaralias 'web-mode-markup-indent-offset 'tab-width)
  (unintern 'web-mode-css-indent-offset)
  (defvaralias 'web-mode-css-indent-offset 'tab-width)
  (unintern 'web-mode-code-indent-offset)
  (defvaralias 'web-mode-code-indent-offset 'tab-width)
  (unintern 'web-mode-attr-indent-offset)
  (defvaralias 'web-mode-attr-indent-offset 'tab-width)
  :config
  (setq web-mode-content-types-alist
        '(("jsx"  . "/affinity/assets/javascripts/.*\\.js[x]?\\'")
          ("jsx"  . "/workspace/mobile/.*\\.js[x]?\\'")
          ("jsx"  . ".*/scaleapi/.*\\.js[x]?\\'")
          ("json" . "\\.eslintrc\\'")
          ("underscorejs"  . ".*\\.tpl\\'")))
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq-default web-mode-enable-auto-pairing nil)
  (add-hook 'web-mode-hook
            (lambda ()
              (set-fill-column 100)
              (setq show-trailing-whitespace t)
              (when (seq-contains-p
                     '("tsx", "js", "jsx")
                     (file-name-extension buffer-file-name))
                (lsp))
              ;; (when (string-equal "tsx" (file-name-extension buffer-file-name))
              ;;   (setup-tide-mode))
              ;; (when (string-equal "js" (file-name-extension buffer-file-name))
              ;;   (setup-tide-mode))
              ;; (when (string-equal "jsx" (file-name-extension buffer-file-name))
              ;;   (setup-tide-mode))
              )))

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode))

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
  ;; :bind ("C-c l" . org-store-link)
  :hook (org-mode . auto-fill-mode)
  :config
  (setq org-log-done t))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  ;; :bind (:map go-mode-map
  ;;             ("M-." . godef-jump))
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  ;; (add-hook
  ;;  `go-mode-hook
  ;;  (lambda ()
  ;;    (setq-local indent-tabs-mode t)))
  )

(use-package antlr-mode
  :mode "\\.g4\\'")

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
  :disabled
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

;; Scale

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile(\\..*)")

(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'"
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package company-terraform
  :ensure t
  :disabled
  :after terraform-mode
  :init
  (company-terraform-init))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

;; Custom after

(use-package my-flycheck-checkers
  :straight nil
  :after (flycheck)
  :load-path "lisp/")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ccls-executable "/home/william/package_src/ccls/Release/ccls")
 '(comment-multi-line t)
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-safe-themes
   '("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "c36614262f32c16cd71e0561a26e5c02486b6a476a6adec7a5cc5582128e665e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(dired-details-initially-hide nil)
 '(elpy-rpc-python-command "/home/william/envs/elpy-rpc-venv/bin/python3")
 '(fci-rule-character-color "#d9d9d9")
 '(flycheck-disabled-checkers '(python-pycompile python-mypy))
 '(flycheck-gometalinter-fast t)
 '(flycheck-gometalinter-tests t)
 '(flycheck-gometalinter-vendor t)
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(lsp-prefer-flymake nil)
 '(lsp-pyls-server-command '("pyls_pipenv"))
 '(lsp-restart 'auto-restart)
 '(lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
 '(lsp-rust-analyzer-cargo-run-build-scripts t)
 '(lsp-rust-analyzer-proc-macro-enable t)
 '(lsp-rust-server 'rust-analyzer)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-peek-enable nil)
 '(lsp-ui-sideline-enable t)
 '(lsp-ui-sideline-show-code-actions nil)
 '(lsp-ui-sideline-show-diagnostics t)
 '(lsp-use-native-json t)
 '(magit-diff-use-overlays nil)
 '(magit-refresh-verbose nil)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(pcache workgroups2 quelpa-use-package quelpa git-link company-box which-key pipenv lsp-origami origami lsp-ivy minions company-terraform terraform-mode winnow forge cmake-mode glsl-mode flymd ccls prettier-js tide ediprolog dockerfile-mode counsel-projectile ivy-hydra counsel jdee org company-go go-mode flycheck-elm material-theme hemisu-theme leuven-theme color-theme-sanityinc-tomorrow dired-details+ web-mode solarized-theme robe lua-mode list-processes+ js2-mode idle-highlight-mode icicles hydra highlight-indent-guides haml-mode geiser fuzzy-match flycheck facemenu+ exec-path-from-shell elpy column-marker auctex ag))
 '(prettier-js-show-errors 'echo)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "*.ccls-cache" ".ccls-cache"))
 '(python-indent-offset 4)
 '(safe-local-variable-values
   '((flycheck-disabled-checkers emacs-lisp-checkdoc)
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (flycheck-gcc-include-path "/home/william/workspace/BerkeleyCS/cs162/code/group/pintos/src" "/home/william/workspace/BerkeleyCS/cs162/code/group/pintos/src/lib" "/home/william/workspace/BerkeleyCS/cs162/code/group/pintos/src/lib/user" "/home/william/workspace/BerkeleyCS/cs162/code/group/pintos/src/lib/kernel")
     (projectile-project-compilation-cmd . "go install")
     (flycheck-gcc-include-path . "/home/william/workspace/BerkeleyCS/cs162/code/group/pintos/src")
     (projectile-project-name . "cs186-project")))
 '(sp-escape-quotes-after-insert nil)
 '(tags-revert-without-query t)
 '(web-mode-enable-auto-indentation t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indentation-current-column-face ((t (:background "#c3b3b3"))))
 '(lsp-ui-sideline-current-symbol ((t (:foreground "#69B7F0" :box (:line-width -1 :color "#69B7F0") :weight ultra-bold :height 0.99))))
 '(lsp-ui-sideline-global ((t (:background "#eee8d5"))))
 '(lsp-ui-sideline-symbol ((t (:foreground "grey60" :box (:line-width -1 :color "grey60") :height 0.99)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
