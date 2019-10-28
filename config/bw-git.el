;;; bw-git.el -- Configure Git
;;; Commentary:
;;; Code:

(defun bw-magithub-pull-request ()
  "Simple pull request command."
  (interactive)
  (with-editor-async-shell-command "hub pull-request"))

(use-package magit
  :ensure t
  :defer t)

(use-package git-gutter
  :ensure t
  :defer t
  :init
  (global-git-gutter-mode +1))

(use-package browse-at-remote
  :ensure t
  :defer t)

(provide 'bw-git)

;;; bw-git ends here
