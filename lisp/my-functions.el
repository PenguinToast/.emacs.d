;;; my-functions.el --- Custom interactive functions.
;;; Commentary:

;;; Code:

(defadvice kill-region (before unix-werase activate compile)
  "Delete a single word backwards when no region."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (sp-backward-sexp 1) (point)) (point)))))


(defvar swapping-buffer nil)
(defvar swapping-window nil)
(defun swap-buffers-in-windows ()
  "Swap buffers between two windows."
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

(defun move-buffer-to-largest-window ()
  "Move current buffer to largest window."
  (interactive)
  (setq swapping-buffer (current-buffer))
  (setq swapping-window (selected-window))
  (select-window (get-largest-window nil nil t))
  (swap-buffers-in-windows))
(global-set-key (kbd "C-x c") 'move-buffer-to-largest-window)

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

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; dir-locals
(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))
(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook (make-variable-buffer-local 'after-save-hook)
                        'my-reload-dir-locals-for-all-buffer-in-this-directory))))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'

(defun willsheu/lsp-get-pipenv-environment (orig-fun &rest args)
  "Return the venv folder using pipenv."
  (if-let* ((pipenv-command-path (executable-find "pipenv"))
            (python-env (let ((process-environment
                               (append '("WORKON_HOME=/home/william/envs"
                                         "LC_ALL=C.UTF-8"
                                         "LANG=C.UTF-8")
                                     process-environment)))
                          (condition-case nil
                              (car (process-lines pipenv-command-path "--venv"))
                            (error nil)))))
      (progn
        (lsp--info "Found pipenv environment: %s" python-env)
        python-env)
    (progn
      (lsp--warn "Couldn't find pipenv environment")
      (apply orig-fun args))))

(defun willsheu/lsp-get-pipenv-pyls-command ()
  "Return the pyls in pipenv, or default."
  (if-let* ((python-env (lsp-pyls-get-pyenv-environment))
            (pyls-command (concat (file-name-as-directory python-env) "bin/pyls"))
            (pyls-executable (file-executable-p pyls-command)))
      pyls-command
      lsp-pyls-server-command))

(defun willsheu/lsp-pyls-setup ()
  "Do setup to customize lsp-pyls."
  (advice-add 'lsp-pyls-get-pyenv-environment :around 'willsheu/lsp-get-pipenv-environment)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 'willsheu/lsp-get-pipenv-pyls-command)
                    :major-modes '(python-mode cython-mode)
                    :priority 1
                    :server-id 'pyls-pipenv
                    :library-folders-fn (lambda (_workspace) lsp-clients-python-library-directories)
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration (lsp-configuration-section "pyls")))))))

(defun willsheu/lsp-ts-ls-setup ()
  "Do setup to give more RAM to tsls"
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                            `(,(lsp-package-path 'typescript-language-server)
                                                              "--tsserver-path"
                                                              ,(lsp-package-path 'typescript)
                                                              ,@lsp-clients-typescript-server-args)))
                    :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                    :priority 0
                    :environment-fn (lambda ()
                                      '(("NODE_OPTIONS" . "--max_old_space_size=4096")))
                    :completion-in-comments? t
                    :initialization-options (lambda ()
                                              (list :plugins lsp-clients-typescript-plugins
                                                    :logVerbosity lsp-clients-typescript-log-verbosity
                                                    :tsServerPath (lsp-package-path 'typescript)))
                    :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                    :server-id 'ts-ls-2)))

(provide 'my-functions)
;;; my-functions.el ends here
