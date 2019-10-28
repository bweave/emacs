;;; bw-lang.el -- Setup languages
;;; Commentary:
;;; Code:

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (sp-local-pair '(js-mode js-jsx-mode typescript-mode rjsx-mode ruby-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair '(js-mode js-jsx-mode typescript-mode rjsx-mode ruby-mode) "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; JavaScript & JSX
(use-package eslintd-fix
  :ensure t)

(use-package tern
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning nil)
  :init
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package rjsx-mode
  :ensure t
  :init
  (add-hook 'rjsx-mode-hook 'eslintd-fix-mode)
  (add-hook 'rjsx-mode-hook 'smartparens-mode)
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode)))

;; Web-Mode
(use-package web-mode
  :ensure t
  :commands web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\(\+modal\\)?\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.module\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-hook 'web-mode-hook 'my-web-mode-setup)
  :config
  (dolist (width '(web-mode-attr-indent-offset web-mode-code-indent-offset web-mode-css-indent-offset web-mode-markup-indent-offset web-mode-sql-indent-offset))
    (set width 2))
  (setq web-mode-engines-alist
        '(("php" . "\\.module\\'"))))

;; Emmet
(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  :config
  (setq emmet-indentation 2))

;; Ruby
(use-package robe
  :ensure t)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(use-package enh-ruby-mode
  :ensure t
  :mode (("\\.rb\\'"       . enh-ruby-mode)
         ("\\.ru\\'"       . enh-ruby-mode)
         ("\\.jbuilder\\'" . enh-ruby-mode)
         ("\\.gemspec\\'"  . enh-ruby-mode)
         ("\\.rake\\'"     . enh-ruby-mode)
         ("Rakefile\\'"    . enh-ruby-mode)
         ("Gemfile\\'"     . enh-ruby-mode)
         ("Guardfile\\'"   . enh-ruby-mode)
         ("Capfile\\'"     . enh-ruby-mode)
         ("Vagrantfile\\'" . enh-ruby-mode))
  :init
  (add-hook 'enh-ruby-mode-hook 'robe-mode))

(use-package rubocop
  :ensure t
  :defer t
  :init (add-hook 'enh-ruby-mode-hook 'rubocop-mode))


;; TODO: this doesn't work, so let's try ruby-test-mode instead
;; (use-package minitest
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'enh-ruby-mode-hook 'minitest-mode)
;;   :config
;;   (setq minitest-default-command (quote ("bin/rails" "test")))
;;   (setq minitest-use-bundler nil))

;; (use-package rspec-mode
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'enh-ruby-mode-hook 'rspec-mode))


;; Checkout evil-multiedit!

(defun bw-rbenv--modeline (current-ruby)
  "Custom icon for modeline with CURRENT-RUBY."
  (concat (propertize "  " 'face 'rbenv-active-ruby-face) current-ruby))

(use-package rbenv
  :ensure t
  :hook ((enh-ruby-mode . rbenv-use-corresponding)
         (ruby-mode . rbenv-use-corresponding))
  :config
  (setq rbenv-modeline-function 'bw-rbenv--modeline)
  (global-rbenv-mode))

(use-package ruby-end
  :ensure t)

;; Slim
(use-package slim-mode
  :ensure t
  :mode ("\\.slim\\'" . slim-mode))

;; YAML
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

;; Web
(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config (progn
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)))


(provide 'bw-lang)
                                        ; bw-lang ends here
