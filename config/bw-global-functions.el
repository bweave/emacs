;;; bw-global-functions.el --- Global functions mostly used by mappings.
;;; Commentary:
;;; Code:

;; (defun seeing-is-believing ()
;;   "Replace the current region (or the whole buffer, if none) with the output of seeing_is_believing."
;;   (interactive)
;;   (let ((beg (if (region-active-p) (region-beginning) (point-min)))
;;         (end (if (region-active-p) (region-end) (point-max)))
;;         (origin (point)))
;;     (shell-command-on-region beg end "seeing_is_believing" nil 'replace)
;;     (goto-char origin)))

(use-package seeing-is-believing
  :ensure t
  :hook ((ruby-mode . seeing-is-believing)
         (enh-ruby-mode . seeing-is-believing)))

;; (defun bw-open-eshell (arg)
;;   "Start the Emacs shell.
;; With one prefix argument ARG, start the shell in the current window.
;; With two prefix arguments, force the creation of a new session.
;; With three prefix arguments, create a new session in the current window."
;;   (interactive "p")
;;   (let ((same-window (or (= arg 4)
;;                          (= arg 64)))
;;         (new-session (or (= arg 16)
;;                          (= arg 64))))
;;     (if same-window
;;         (eshell new-session)
;;       (let ((buf (eshell new-session)))
;;         (switch-to-buffer (other-buffer buf))
;;         (switch-to-buffer-other-window buf)))))

(provide 'bw-global-functions)
;;; bw-global-functions.el ends here
