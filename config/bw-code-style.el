;;; bw-code-style --- linters and such
;;; Code:
;;; Commentary:

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                 (or (buffer-file-name) default-directory)
                 "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/use-rubocop-from-bin ()
  (let* ((root (locate-dominating-file
                 (or (buffer-file-name) default-directory)
                 "Gemfile"))
         (rubocop (and root
                       (expand-file-name "bin/rubocop" root))))
    (when (and rubocop (file-executable-p rubocop))
      (setq-local flycheck-ruby-rubocop-executable rubocop))))

;; Flycheck
(use-package flycheck
             :ensure t
             :init
             (global-flycheck-mode)
             :config
             (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
             (add-hook 'flycheck-mode-hook #'my/use-rubocop-from-bin)
             (setq flycheck-javascript-eslint-executable nil)
             (setq-default flycheck-emacs-lisp-load-path 'inherit)
             (setq-default
               flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint ruby-reek))
               flycheck-temp-prefix ".flycheck")
             (use-package flycheck-status-emoji
                          :ensure t
                          :config
                          (flycheck-status-emoji-mode)
                          ))


(provide 'bw-code-style)

;;; bw-code-style ends here
