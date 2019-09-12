;;; magit-workgroups.el --- Add function to magit to save/restore workgroups
;;; wconfig & change branch
;;; Commentary:

;;; Code:

(require 'projectile)
(require 'magit)
(require 'workgroups2)

(defvar magit-workgroups-saved-wconfigs
  (make-hash-table
   :test 'equal))

;;;###autoload
(defun magit-change-branch-workgroup ()
  "Change branch and wconfig."
  (interactive)
  (let ((project (projectile-project-root))
        (projects magit-workgroups-saved-wconfigs))
    (let ((branch (magit-get-current-branch)))
      (unless (gethash project projects)
        (puthash project (make-hash-table :test 'equal) projects))
      (let ((branches (gethash project projects)))
        (puthash branch (wg-current-wconfig) branches)))
    (call-interactively 'magit-checkout)
    (when-let* ((branch (magit-get-current-branch))
                (branches (gethash project projects))
                (wconfig (gethash branch branches)))
      (wg-restore-wconfig-undoably wconfig))))

;;;###autoload

(transient-append-suffix 'magit-branch "l"
  '("w" "with workgroup" magit-change-branch-workgroup))

(provide 'magit-workgroups)
;;; magit-workgroups.el ends here
