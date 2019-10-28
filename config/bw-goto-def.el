;;; bw-goto-def --- GoTo Definition setup
;;; Code:
;;; Commentary:
; Use dumb-jump cuz it just works!

(use-package dumb-jump
  :ensure
  :config (setq dumb-jump-selector 'helm))

(provide 'bw-goto-def)

;;; bw-goto-def ends here
