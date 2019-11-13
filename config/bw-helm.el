;;; bw-helm.el -- Setup helm
;;; Commentary:
;;; Code:

(use-package helm
             :ensure t
             :init
             (setq helm-M-x-fuzzy-match t
                   helm-mode-fuzzy-match t
                   helm-buffers-fuzzy-matching t
                   helm-recentf-fuzzy-match t
                   helm-locate-fuzzy-match t
                   helm-semantic-fuzzy-match t
                   helm-imenu-fuzzy-match t
                   helm-completion-in-region-fuzzy-match t
                   helm-candidate-number-list 10
                   helm-move-to-line-cycle-in-source t)
             :config
             (helm-mode 1))

(provide 'bw-helm)
;;; bw-helm ends here
