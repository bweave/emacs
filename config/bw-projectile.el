;;; bw-projectile -- Setup projectile
;;; Commentary:
;;; Code:

; (setq ctags_cmd "ctags -R -e -a -f \"%s\" %s \"%s\" --exclude=.md --exclude=.markdown --exclude=.css --exclude=.scss --exclude=.html --exclude=.erb --exclude=.json --exclude=.xml --exclude=.git --exclude=node_modules --exclude=Gemfile --exclude=Gemfile.lock --exclude=package.json --exclude=yarn.lock --exclude=Procfile --exclude=Guardfile --exclude=vendor/assets")

(use-package projectile
             :ensure t
             :init
             (setq projectile-require-project-root nil)
             ; (setq projectile-tags-command ctags_cmd)
             :config
             (setq large-file-warning-threshold nil)
             (projectile-mode 1))

(use-package helm-projectile
             :ensure t
             :init
             (setq helm-projectile-fuzzy-match t)
             :config
             (helm-projectile-on))

(provide 'bw-projectile)
; bw-projectile ends here
