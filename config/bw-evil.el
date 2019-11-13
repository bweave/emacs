;;; bw-evil.el -- My evil mode configuration.
;;; Commentary:
;;; Code:

(defun bw-config-evil ()
  "Configure evil mode."
  :init
  (setq evil-vsplit-window-right t)
  :config
  ;; Use Emacs state in these additional modes.
  ;; (dolist (mode '(ag-mode
  ;;                 custom-mode
  ;;                 custom-new-theme-mode
  ;;                 dired-mode
  ;;                 eshell-mode
  ;;                 flycheck-error-list-mode
  ;;                 git-rebase-mode
  ;;                 org-capture-mode
  ;;                 sunshine-mode
  ;;                 term-mode))
  ;;   (add-to-list 'evil-emacs-state-modes mode))

  ;; (delete 'term-mode evil-insert-state-modes)
  ;; (delete 'eshell-mode evil-insert-state-modes)

  ;; Use insert state in these additional modes.
  (dolist (mode '(twittering-edit-mode
                  magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Global bindings.
  (evil-define-key 'normal global-map (kbd "C-h")     'evil-window-left)
  (evil-define-key 'normal global-map (kbd "C-j")     'evil-window-down)
  (evil-define-key 'normal global-map (kbd "C-k")     'evil-window-up)
  (evil-define-key 'normal global-map (kbd "C-l")     'evil-window-right)
  (evil-define-key 'normal global-map (kbd "C-=")     'bw/increase-default-font-height)
  (evil-define-key 'normal global-map (kbd "C--")     'bw/decrease-default-font-height)
  (evil-define-key 'normal global-map (kbd "<down>")  'evil-next-visual-line)
  (evil-define-key 'normal global-map (kbd "<up>")    'evil-previous-visual-line)
  (evil-define-key 'normal global-map (kbd "-")       'helm-find-files)
  (evil-define-key 'normal global-map (kbd "gd")      'dumb-jump-go)
  (evil-define-key 'normal global-map (kbd "gD")      'dumb-jump-go-other-window)
  (evil-define-key 'normal global-map (kbd "C-p")     'helm-projectile-find-file)
  (evil-define-key 'normal global-map (kbd "M-s-p")   'helm-projectile-switch-project)
  ;; (evil-define-key 'normal global-map (kbd "C-t")     'bw-open-eshell)
  (evil-define-key 'normal global-map (kbd "C-t")     'multi-term-dedicated-toggle)
  (evil-define-key 'normal global-map (kbd "\\ \\")   'tiny-menu)
   ;; Neotree
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
    In Delete Selection mode, if the mark is active, just deactivate it;
    then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(defun bw-apply-evil-other-package-configs ()
  "Apply evil-dependent settings specific to other packages."

  (defun next-conflict-marker ()
    (interactive)
    (evil-next-visual-line)
    (if (not (search-forward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-max) t))
        (evil-previous-visual-line))
    (move-beginning-of-line nil))

  (defun previous-conflict-marker ()
    (interactive)
    (search-backward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-min) t)
    (move-beginning-of-line nil))

  ;; Dired
  (evil-define-key 'normal dired-mode-map (kbd "C-e") 'dired-toggle-read-only))

(defmacro define-evil-or-global-key (key def &optional state)
  "Define a key KEY with DEF in an Evil map, or in the global map.
  If the Evil map for STATE is defined (or `normal' if STATE is not
                                           provided) the key will be defined in that map.  Failing that, it will
  be defined globally.
  Note that STATE should be provided as an unquoted symbol.
  This macro provides a way to override Evil mappings in the appropriate
  Evil map in a manner that is compatible with environments where Evil
  is not used."
  (let* ((evil-map-name (if state
                            (concat "evil-" (symbol-name state) "-state-map")
                          "evil-normal-state-map"))
         (map (if (boundp (intern evil-map-name))
                  (intern evil-map-name)
                global-map)))
    `(define-key ,map ,key ,def)))

(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :config
  (add-hook 'evil-mode-hook 'bw-config-evil)
  (evil-mode 1)

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t)

  (use-package evil-nerd-commenter
    :ensure t)


  (bw-apply-evil-other-package-configs))

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   ;; Utils
   "x"   '(helm-M-x :which-key "M-x")
   "e"   '(neotree-toggle :which-key "Toggle Neotree")
   "s"   '(helm-projectile-ag :which-key "Find in project")
   "/"   '(evilnc-comment-or-uncomment-lines :which-key "Comment toggle")
   "SPC" '(mode-line-other-buffer :which-key "Toggle previous buffer")
   "TAB" (general-simulate-key ":vs RET :A RET"
           :state 'normal
           :name general-SPC-TAB-simulates-split-to-alternate-file
           :docstring "Split to alternate file"
           :which-key "Split to alternate file")
   ;; Projects
   "p"   '(:ignore t :which-key "Project")
   "pp"  '(helm-projectile-switch-project :which-key "Switch")
   "pr"  '(helm-show-kill-ring :which-key "Show kill ring")
   ;; Rails
   "r"   '(:ignore t :which-key "Rails")
   "rc"  '(projectile-rails-console :which-key "Console")
   "rs"  '(projectile-rails-server :which-key "Server")
   "rS"  '(projectile-rails-goto-seeds :which-key "Seeds")
   "rg"  '(projectile-rails-generate :which-key "Generate")
   "rG"  '(projectile-rails-goto-gemfile :which-key "Gemfile")
   "rr"  '(projectile-rails-goto-routes :which-key "Routes")
   "re"  '(:ignore t :which-key "Edit")
   "rem" '(projectile-rails-find-current-model :which-key "Model")
   "rev" '(projectile-rails-find-current-view :which-key "View")
   "rec" '(projectile-rails-find-current-controller :which-key "Controller")
   ;; Buffers
   "b"   '(:ignore t :which-key "Buffers")
   "be"  '(helm-buffers-list :which-key "List")
   "f"   '(neotree-project-dir :which-key "Find")
   "w"   '(evil-delete-buffer :which-key "Delete buffer")
   ;; Git
   "g"   '(:ignore t :which-key "Git")
   "gs"  '(magit-status :which-key "Status")
   "gb"  '(magit-branch-checkout :which-key "Checkout Branch")
   "gc"  '(magit-branch-and-checkout :which-key "Checkout New Branch")
   "gp"  '(magit-pull :which-key "Pull")
   "gP"  '(magit-push :which-key "Push")
   "gr" '(bw-magithub-pull-request :which-key "Pull Request")
   ;; Tests
   "t"   '(:ignore t :which-key "Tests")
   "tt"  '(minitest-verify :which-key "Run file")
   "tT"  '(minitest-verify-single :which-key "Run single")
   "ta"  '(minitest-verify-all :which-key "Run all")
   ;; Themes
   "T"   '(load-theme :which-key "Themes")
   ))

(use-package evil-multiedit
  :ensure t
  :bind (:map evil-visual-state-map
              ("R" . evil-multiedit-match-all)
              ("s-d" . evil-multiedit-match-and-next)
              ("s-u" . evil-multiedit-match-and-previous)
              :map evil-normal-state-map
              ("s-d" . evil-multiedit-match-and-next)
              ("s-u" . evil-multiedit-match-and-previous)
              ("M-D" . evil-multiedit-restore)
              :map evil-multiedit-state-map
              ("s-n" . evil-multiedit-next)
              ("s-p" . evil-multiedit-prev)
              :map evil-multiedit-insert-state-map
              ("s-n" . evil-multiedit-next)
              ("s-p" . evil-multiedit-prev))
  )

(use-package evil-matchit
  :ensure t
  :init
  (global-evil-matchit-mode))

(provide 'bw-evil)
;;; bw-evil.el ends here
