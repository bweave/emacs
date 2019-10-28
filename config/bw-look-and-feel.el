;;; bw-look-and-feel --- look and feel
;;; Code:
;;; Commentary:

;; All The Icons
(use-package all-the-icons
             :ensure t)

;; NeoTree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

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
             (spaceline-toggle-evil-state-on)
             (spaceline-toggle-minor-modes-off))

(provide 'bw-look-and-feel)

;;; bw-look-and-feel ends here
