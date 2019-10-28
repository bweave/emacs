;;; bw-completion.el -- Setup completion
;;; Commentary:
;;; Code:

(use-package company
             :ensure t
             :init
             (setq company-minimum-prefix-length 3)
             (setq company-auto-complete nil)
             (setq company-idle-delay 0)
             (setq company-require-match 'never)
             (setq company-frontends
                   '(company-pseudo-tooltip-unless-just-one-frontend
                      company-preview-frontend
                      company-echo-metadata-frontend))
             (setq tab-always-indent 'complete)
             (defvar completion-at-point-functions-saved nil)
             :config
             (global-company-mode 1)
             (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
             (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
             (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
             (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
             (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)
             (defun company-indent-for-tab-command (&optional arg)
               (interactive "P")
               (let ((completion-at-point-functions-saved completion-at-point-functions)
                     (completion-at-point-functions '(company-complete-common-wrapper)))
                 (indent-for-tab-command arg)))

             (defun company-complete-common-wrapper ()
               (let ((completion-at-point-functions completion-at-point-functions-saved))
                 (company-complete-common))))

(use-package company-lsp
             :ensure t
             :init
             (push 'company-lsp company-backends))

(provide 'bw-completion)
; bw-completion ends here
