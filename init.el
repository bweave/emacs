;;; init.el -- BWeave Emacs configuration
;;; Commentary:
;;; Code:

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Leave this here, or package.el will just add it again.
(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

; (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; My Configs
(require 'bw-elpa)
(require 'bw-base)
(require 'bw-look-and-feel)
(require 'bw-global-functions)
(require 'bw-platform-setup)
(require 'bw-tiny-menu)
(require 'bw-helm)
(require 'bw-projectile)
(require 'bw-fonts)
(require 'bw-code-style)
(require 'bw-search)
(require 'bw-completion)
(require 'bw-goto-def)
(require 'bw-lang)
(require 'bw-rails)
(require 'bw-git)
(require 'bw-evil)

(use-package multi-term
  :ensure t
  :init
  (setq multi-term-program "/usr/local/bin/zsh"))

(server-start)

;;; init.el ends here
