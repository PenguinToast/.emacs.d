;;; my-functions.el --- Custom interactive functions.

;; Author: PenguinToast <sheu.william@gmail.com>

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

;; Taken from https://github.com/emacs-lsp/lsp-mode/pull/161/files#diff-89805084d36a788cb21a777663a63538R359
(defun willsheu/merge-plists (first &rest rest)
  "Deeply merge plists.
FIRST is the plist to be merged into. The rest of the arguments
can be either plists or nil. The non-nil plists in the rest of
the arguments will be merged into FIRST.
Return the merged plist."
  (cl-assert (listp first))
  (seq-each (lambda (pl)
              (setq first
                (willsheu/merge-two-plists first pl)))
    rest)
  first)

(defun willsheu/merge-two-plists (first second)
  "Deeply merge two plists.
All values in SECOND are merged into FIRST. FIRST can be nil or a
plist. SECOND must be a plist.
Return the merged plist."
  (when second
    (if (not (listp second))
      (warn "Cannot merge non-list value into a plist. The value is %s" second)
      (cl-loop for (key second-value) on second
        collect (progn
                  (let ((first-value (plist-get first key))
                         merged-value)
                    (cond
                      ((null second-value)) ; do nothing
                      ((null first-value)
                        (if (listp second-value)
                          ;; Deep copy second-value so that the original value won't
                          ;; be modified.
                          (setq merged-value
                            (willsheu/merge-two-plists nil second-value)))
                        (setq merged-value second-value))
                      ((and (listp first-value) (listp second-value))
                        (setq merged-value (willsheu/merge-two-plists first-value second-value)))
                      ;; Otherwise, the first value is a leaf entry and should
                      ;; not be overridden.
                      )
                    (when merged-value
                      (setq first (plist-put first key merged-value))))))))
  first)

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
                    :priority -2
                    :server-id 'pyls-pipenv
                    :library-folders-fn (lambda (_workspace) lsp-clients-python-library-directories)
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration (lsp-configuration-section "pyls")))))))

(defgroup willsheu nil
  "My configs."
  :group 'willsheu)

(defcustom willsheu/pylsp-packages-environment "/home/william/workspace/pylsp-packages/"
  "Path to folder containing Pipfile for pylsp packages."
  :type 'string
  :group 'willsheu)

(defvar willsheu/python-venv-cache (make-hash-table :test 'equal))
(defconst willsheu/cache-empty (make-symbol "willsheu/cache-empty"))
(defmacro willsheu/cached (cache key value)
  "Retrieve or compute & cache a VALUE in CACHE by KEY."
  `(let* ((cache-evaluated ,cache)
          (key-evaluated ,key)
          (cached (gethash key-evaluated cache-evaluated willsheu/cache-empty)))
     (if (eq cached willsheu/cache-empty)
         (let ((value-evaluated ,value))
           (puthash key-evaluated value-evaluated cache-evaluated)
           value-evaluated)
       cached)))

(defun willsheu/resolve-python-venv (directory)
  "Find the path to DIRECTORY's venv, if it exists."
  (let ((default-directory (file-name-as-directory directory)))
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
        (if-let* ((venv-cmd lsp-pylsp-get-pyenv-environment)
                  (venv (venv-cmd)))
            venv
          "/usr/")))))

(defun willsheu/resolve-current-python-venv ()
  "Call willsheu/resolve-python-venv with the cwd, cached."
  (willsheu/cached willsheu/python-venv-cache
                   default-directory
                   (willsheu/resolve-python-venv default-directory)))


(defun willsheu/lsp-pylsp-setup ()
  "Do setup to customize lsp-pylsp."
  (setq lsp-pylsp-server-command
        (concat (file-name-as-directory
                 (willsheu/cached willsheu/python-venv-cache
                                  willsheu/pylsp-packages-environment
                                  (willsheu/resolve-python-venv willsheu/pylsp-packages-environment)))
                "bin/pylsp"))
  (if (file-executable-p lsp-pylsp-server-command)
      (lsp-register-custom-settings
       '(("pylsp.plugins.jedi.environment" willsheu/resolve-current-python-venv)
         ("pylsp.plugins.pylsp_mypy.overrides"
          (lambda ()
            (vector "--python-executable"
                    (concat (file-name-as-directory
                             (willsheu/resolve-current-python-venv))
                            "bin/python")
                    t)))
         ;;("pylsp.plugins.pylsp_mypy.dmypy" t t)
         ))
    (message "Pylsp venv not set up correctly."))
  )

(defun willsheu/lsp-modify-rust-init (base)
  "Modify init plist.
BASE is the original init plist."
  (willsheu/merge-plists
   base '(:diagnostics (:disabled ["unresolved-import"]))))

(defun willsheu/lsp-rust-setup ()
  "Do setup to customize rust-analyzer."
  (advice-add 'lsp-rust-analyzer--make-init-options :filter-return 'willsheu/lsp-modify-rust-init))

(defun willsheu/lsp-eslint-setup ()
  "Do setup to customize eslint."
  (el-patch-lsp-defun lsp-eslint--configuration (_workspace (&ConfigurationParams :items))
                      (->> items
                           (seq-map (-lambda ((&ConfigurationItem :scope-uri?))
                                      (-when-let* ((file (lsp--uri-to-path scope-uri?))
                                                   (buffer (find-buffer-visiting file))
                                                   (workspace-folder (lsp-find-session-folder (lsp-session) file)))
                                        (with-current-buffer buffer
                                          (list :validate (if (member (lsp-buffer-language) lsp-eslint-validate) "on" "probe")
                                                :packageManager lsp-eslint-package-manager
                                                :codeAction (list
                                                             :disableRuleComment (list
                                                                                  :enable (lsp-json-bool lsp-eslint-code-action-disable-rule-comment)
                                                                                  :location lsp-eslint-code-action-disable-rule-comment-location)
                                                             :showDocumentation (list
                                                                                 :enable (lsp-json-bool lsp-eslint-code-action-show-documentation)))
                                                :codeActionOnSave (list :enable (lsp-json-bool lsp-eslint-auto-fix-on-save)
                                                                        :mode lsp-eslint-fix-all-problem-type)
                                                :format (lsp-json-bool lsp-eslint-format)
                                                :quiet (lsp-json-bool lsp-eslint-quiet)
                                                :onIgnoredFiles (if lsp-eslint-warn-on-ignored-files "warn" "off")
                                                :options (or lsp-eslint-options (ht))
                                                :rulesCustomizations lsp-eslint-rules-customizations
                                                :run lsp-eslint-run
                                                :nodePath lsp-eslint-node-path
                                                :workspaceFolder (list :uri (lsp--path-to-uri workspace-folder)
                                                                       :name (f-filename workspace-folder))
                                                (el-patch-add :workingDirectories (vector lsp-eslint-working-directories)))))))
                           (apply #'vector)))
  )

(defun ap/garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used = (* used size)
                    for free = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used free))
                    for used = (file-size-human-readable used)
                    for free = (file-size-human-readable free)
                    concat (format "%s: %s + %s = %s\n" type used free total))))


(provide 'my-functions)
;;; my-functions.el ends here
