;;; bw-search.el -- Setup search
;;; Commentary:
;;; Code:

(use-package ag
             :ensure t
             :config
             (use-package helm-ag :ensure t))

(provide 'bw-search)
; bw-search ends here
