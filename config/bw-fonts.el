;;; bw-fonts --- Font setup
;;; Commentary:
;; Barrowed from Aaron Bieber's dotfiles
;;; Code:

;; Font
(add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font Mono-14"))

(defcustom bw/force-default-font-for-symbols nil
  "When non-nil, force Emacs to use your default font for symbols."
  :type 'boolean)

(defun bw/maybe-use-default-font-for-symbols ()
  "Force Emacs to render symbols using the default font, if so configured."
  (when bw/force-default-font-for-symbols
    (set-fontset-font "fontset-default" 'symbol (face-attribute 'default :family))))

(add-hook 'after-init-hook 'bw/maybe-use-default-font-for-symbols)

;;; Changing font sizes

(require 'cl)

(defun bw/font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun bw/set-frame-font-size (size)
  (set-frame-font (bw/font-name-replace-size (face-font 'default) size) t t))

(defun bw/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
Emacs will keep the pixel size of the frame approximately the
same.  DELTA should be a multiple of 10, to match the units used
by the :height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (bw/set-frame-font-size new-point-height)
    (set-face-attribute 'default nil :height new-height)
    (message "Default font size is now %d" new-point-height)))

(defun bw/increase-default-font-height ()
  (interactive)
  (bw/increment-default-font-height 10)
  (if (fboundp 'powerline-reset)
      (powerline-reset)))

(defun bw/decrease-default-font-height ()
  (interactive)
  (bw/increment-default-font-height -10)
  (if (fboundp 'powerline-reset)
      (powerline-reset)))

(bind-key (kbd "s-=") 'bw/increase-default-font-height)
(bind-key (kbd "s--") 'bw/decrease-default-font-height)

(provide 'bw-fonts)

;;; bw-fonts ends here
