(use-package bind-key
  :config
  (bind-keys :map global-map
             ("C-z" . nil)
             ("C-x C-z" . nil)
             ("C-h" . delete-backward-char)
             ("C-x v =" . git-gutter:popup-diff)
             ("C-:" . highlight-symbol-at-point)
             ("C-M-:" . highlight-symbol-remove-all)
             ("C-x g" . magit-status)))

(use-package smartrep
  :config
  (progn
    (defvar ctl-z-map (make-keymap))
    (define-key global-map "\C-z" ctl-z-map)

    (use-package moz
      :bind
      ("C-z C-r" . my/moz-reload)
      :config
      (defun my/moz-reload ()
        (interactive)
        (comint-send-string
         (inferior-moz-process)
         "BrowserReload();")))

    (smartrep-define-key
        global-map "C-x" '(("p" . 'git-gutter:previous-diff)
                           ("n" . 'git-gutter:next-diff)
                           ("o" . 'other-window)
                           ("-" . 'text-scale-decrease)
                           ("+" . 'text-scale-increase)))
    (smartrep-define-key
        global-map "C-z" '(("n" . (lambda () (scroll-other-window 1)))
                           ("p" . (lambda () (scroll-other-window -1)))
                           ("k" . 'flycheck-previous-error)
                           ("j" . 'flycheck-next-error)))))

(use-package multiple-cursors
  :config
  (bind-keys :map global-map
             ("C->" . mc/mark-next-like-this)
             ("C-<" . mc/mark-previous-like-this)))

(use-package highlight-symbol
  :config
  (bind-keys :map global-map
             ("C-;" . highlight-symbol-at-point)
             ("C-M-;" . highlight-symbol-remove-all)))

;; expand-region
(use-package expand-region
  :bind ("C-@" . er/expand-region))

(use-package helm
  :config
  (progn
    (setq helm-quick-update t
          helm-buffers-fuzzy-matching t
          helm-ff-transformer-show-only-basename nil)

    (add-hook 'minibuffer-setup-hook
              (lambda ()
                (deactivate-input-method)))
    (add-hook 'helm-minibuffer-set-up-hook
              (lambda ()
                (deactivate-input-method)))

    (bind-keys :map global-map
               ("M-x" . helm-M-x))
    (use-package helm-ag
      :config
      (bind-keys :map global-map
                 ("C-x C-g" . helm-projectile-ag)))
    (use-package helm-projectile
      :config
      (bind-keys :map global-map
                 ("C-x C-r" . helm-projectile-recentf)
                 ("C-x b" . (lambda (arg)
                              (interactive "p")
                              (case arg
                                (4 (call-interactively 'switch-to-buffer))
                                (t (helm-projectile-switch-to-buffer)))))
                 ("C-x C-f" . (lambda (arg)
                                (interactive "p")
                                (case arg
                                  (4 (helm-projectile-find-file))
                                  (t (call-interactively 'find-file)))))))))
