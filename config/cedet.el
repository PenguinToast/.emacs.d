;;; minimial-cedet-config.el --- Working configuration for CEDET from bzr

;; Copyright (C) Alex Ott
;;
;; Author: Alex Ott <alexott@gmail.com>
;; Keywords: cedet, C++, Java
;; Requirements: CEDET from bzr (http://cedet.sourceforge.net/bzr-repo.shtml)

;; Do checkout of fresh CEDET, and use this config (don't forget to change path below)

;; (setq cedet-root-path
      ;; (file-name-as-directory (expand-file-name
                               ;; "~/.emacs.d/site-lisp/cedet/")))

;; (load-file (concat cedet-root-path "cedet-devel-load.el"))
;; (load-file (concat cedet-root-path "contrib/cedet-contrib-load.el"))

;; (add-to-list 'Info-directory-list
                     ;; "~/.emacs.d/site-lisp/cedet/doc/info")

;; select which submodes we want to activate
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

;; Activate semantic
(semantic-mode 1)

;; (require 'semantic/bovine/c)
;; (require 'semantic/bovine/gcc)

;; (require 'cedet-files)

;; load contrib library
;; (require 'eassist)

;; customisation of modes
(defun alexott/cedet-hook ()
  
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\M-." 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-ct" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'scheme-mode-hook 'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook 'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

;; (when (cedet-gnu-global-version-check t)
  ;; (semanticdb-enable-gnu-global-databases 'c-mode t)
  ;; (semanticdb-enable-gnu-global-databases 'c++-mode t))

;; (when (cedet-ectag-version-check t)
  ;; (semantic-load-enable-primary-ectags-support))

;; SRecode
;; (global-srecode-minor-mode 1)

;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)

;; Setup JAVA....
;; (require 'cedet-java)

;;; minimial-cedet-config.el ends here
