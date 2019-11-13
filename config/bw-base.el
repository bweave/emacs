;;; bw-base --- Base settings
;;; Code:
;;; Commentary:

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message ";; Happy Hacking")
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default truncate-lines t)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq ring-bell-function 'ignore)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(setq-default display-line-numbers 'relative)
(column-number-mode t)
(setq tab-width 2)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(fset 'yes-or-no-p 'y-or-n-p) ; make prompts y/n instead of yes/no

;; Resume previous sesh
(desktop-save-mode 1)

(provide 'bw-base)

;;; bw-base ends here
