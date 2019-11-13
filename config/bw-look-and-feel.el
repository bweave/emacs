;;; bw-look-and-feel --- look and feel
;;; Code:
;;; Commentary:

;; All The Icons
(use-package all-the-icons
             :ensure t)

;; NeoTree
(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

(use-package neotree
  :ensure t
  :init
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Eyebrowse for vim style tab navigation
(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-wrap-around t)
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode t))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Powerline
(use-package spaceline
             :ensure t
             :init
             (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
             :config
             (spaceline-spacemacs-theme)
             (spaceline-helm-mode)
             (spaceline-toggle-workspace-number-on)
             (spaceline-toggle-evil-state-on)
             (spaceline-toggle-minor-modes-off))

(provide 'bw-look-and-feel)

;;; bw-look-and-feel ends here
