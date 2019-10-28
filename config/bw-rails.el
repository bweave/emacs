;;; bw-rails.el -- Rails support stuff
;;; Commentary:
;;; Code:

;; Projectile Rails
(use-package projectile-rails
             :ensure t
             :diminish
             :after (projectile)
             :init
             (projectile-rails-global-mode)
             :config
             (setq projectile-rails-vanilla-command "bin/rails"
                   projectile-rails-spring-command "bin/spring"))

;; Evil Rails
(use-package evil-rails
             :ensure t)


(provide 'bw-rails)
; bw-rails ends here
