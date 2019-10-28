;;; bw-tiny-menu.el -- Tiny Menu is similar to which-key
;;; Commentary:
;;; Code:

(use-package tiny-menu
  :ensure t
  :commands (tiny-menu-run-item)
  :config
  (setq tiny-menu-items
        '(("reverts"      ("Revert"
                           ((?r "This buffer"     revert-buffer)
                            (?o "All Org buffers" org-revert-all-org-buffers))))
          ("org-captures" ("Org Captures"
                                    ((?c "Task/idea" (lambda () (interactive (org-capture nil "h"))))
                                     (?n "Note" (lambda () (interactive (org-capture nil "o")))))))

          )))

(provide 'bw-tiny-menu)
; bw-tiny-menu ends here
