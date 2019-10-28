;;; bw-global-functions.el --- Global functions mostly used by mappings.
;;; Commentary:
;;; Code:

(defun bw-open-eshell (arg)
  "Start the Emacs shell.
With one prefix argument ARG, start the shell in the current window.
With two prefix arguments, force the creation of a new session.
With three prefix arguments, create a new session in the current window."
  (interactive "p")
  (let ((same-window (or (= arg 4)
                         (= arg 64)))
        (new-session (or (= arg 16)
                         (= arg 64))))
    (if same-window
        (eshell new-session)
      (let ((buf (eshell new-session)))
        (switch-to-buffer (other-buffer buf))
        (switch-to-buffer-other-window buf)))))

(provide 'bw-global-functions)
;;; bw-global-functions.el ends here
