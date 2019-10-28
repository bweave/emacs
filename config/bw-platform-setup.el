;;; bw-platform-setup --- Platform-specific settings
;;; Commentary:
;; Barrowed from Aaron Bieber's dotfiles
;;; Code:
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SHELL")
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "MYSQL_PORT_3306_TCP_ADDR")
  (exec-path-from-shell-copy-env "MYSQL_SLAVE_PORT_3306_TCP_ADDR")
  (exec-path-from-shell-copy-env "MYSQL_SLAVE_PORT_3306_TECP_PORT"))

;; ;; This must run after window setup or it seems to have no effect.
;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (when (memq window-system '(mac ns))
;;               (bw/set-frame-font-size 14)
;;               (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen))

;;             (when (fboundp 'powerline-reset)
;;               (powerline-reset))))

;; Display emoji on Macs where the font is already there.
(when (memq window-system '(mac))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(provide 'bw-platform-setup)
;;; bw-platform-setup.el ends here
