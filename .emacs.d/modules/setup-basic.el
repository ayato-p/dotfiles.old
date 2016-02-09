;; Performance
(setq gc-cons-threshold 20000000)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Show active region
(transient-mark-mode 1)

;; auto pair
;; (electric-pair-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; No lock files
(setq create-lockfiles nil)

(setq next-screen-context-lines 5)

(use-package smart-newline
  :config
  (add-hook 'clojure-mode-hook 'smart-newline-mode)
  (add-hook 'org-mode-hook 'smart-newline-mode)
  (defun my/smart-newline (of &rest arglist)
    (apply (cond (current-prefix-arg #'newline)
                 (t of))
           arglist))
  (advice-add 'smart-newline :around #'my/smart-newline))

(defun display-prefix (arg)
  "Display the value of the raw prefix arg."
  (interactive "P")
  (message "%s" arg))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; anzu
(use-package anzu
  :config (global-anzu-mode 1))

;; recentf
(use-package recentf
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 50)))

(use-package ace-isearch
  :config
  (global-ace-isearch-mode 1))

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; (use-package helm-wordnet
;;   :config (setq helm-wordnet-wordnet-location "/usr/share/wordnet"))

(use-package helm-swoop
  :config
  (progn
    (setq helm-swoop-split-with-multiple-windows t)))

;; editor config
(setq edconf-exec-path "/usr/bin/editorconfig")

(setq visible-bell t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      ;; disable to buckup funciton
      backup-inhibited t
      delete-auto-save-files t
      ;; completion ignore case (lower/upper)
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      inhibit-startup-message t)

(savehist-mode 1)
(column-number-mode 1)

(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package paren
  :init (progn (setq show-paren-style 'parenthesis)
               (show-paren-mode 1)))

(use-package rainbow-mode
  :init (progn (add-hook 'css-mode-hook 'rainbow-mode)
               (add-hook 'clojure-mode-hook 'rainbow-mode)
               (add-hook 'js2-mode-hook 'rainbow-mode)
               (add-hook 'web-mode-hook 'rainbow-mode)
               (add-hook 'emacs-lisp-mode 'rainbow-mode)))

(use-package eww
  :init
  (setq eww-search-prefix "https://www.google.co.jp/search?q="))

(use-package emojify
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package emoji-cheat-sheet-plus
  :defer t
  :init
  (progn
    ;; enabled emoji in buffer
    (add-hook 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    ;; (add-hook 'text-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    ;; insert emoji with helm
    (bind-keys :map global-map
               ("C-c C-e" . emoji-cheat-sheet-plus-insert))))

(when window-system
  (load-theme 'zenburn t)
  ;; (load-theme 'hc-zenburn t)
  ;; (load-theme 'gruvbox t)
  ;; (load-theme 'ample t t)
  ;; (load-theme 'ample-flat t t)
  ;; (enable-theme 'ample-flat)

  (custom-set-faces
   '(e2wm:face-history-list-normal ((t (:foreground "LightGoldenrod1")))))

  (setq default-frame-alist '((font . "Droid Sans Mono-8")))
  (set-default-font "Droid Sans Mono-8")
  (set-face-font 'variable-pitch "Droid Sans Mono-8")
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    '("Takaoゴシック" . "unicode-bmp"))

  (use-package server
    :config (unless (server-running-p)
              (server-start))))

(use-package hl-line
  :init
  (progn
    (global-hl-line-mode 1)
    (set-face-background 'hl-line "#525252")))

(use-package skk
  :config
  (setq default-input-method "japanese-skk")

  :init
  (use-package skk-decor)
  (setq skk-kakutei-when-unique-candidate nil)
  (setq skk-egg-like-newline t)
  (setq skk-isearch-mode-enable nil)
  (setq skk-show-inline 'vertical)
  (setq skk-cdb-large-jisyo "/usr/share/skk/SKK-JISYO.LL.cdb")
  (setq skk-sticky-key ":")
  (setq skk-auto-insert-paren nil)
  (add-to-list 'context-skk-programming-mode 'clojure-mode)

  (add-hook 'skk-load-hook
            (lambda ()
              (require 'context-skk)))

  (add-hook 'find-file-hook
            (lambda ()
              (skk-latin-mode t)))

  ;; conflict skk && paredit
  (defun paredit-newline\\skk-kakutei (origfun &rest arglist)
    (apply (cond ((not skk-mode) origfun)
                 (t #'skk-kakutei))
           arglist))

  (advice-add 'paredit-newline :around #'paredit-newline\\skk-kakutei)

  (defun paredit-newline\\skk-insert (origfun &rest arglist)
    (apply (cond (skk-j-mode #'skk-insert)
                 (t origfun))
           arglist))

  (advice-add 'cljr-slash :around #'paredit-newline\\skk-insert)
  )

;; for whitespace
(use-package whitespace
  :init
  (progn
    (setq whitespace-style '(face
                             trailing
                             tabs
                             spaces
                             empty
                             space-mark
                             tab-mark
                             ))
    (setq whitespace-space-regexp "\\(\x3000+\\)")
    (setq whitespace-display-mappings
          '((space-mark ?\u3000 [?\u25a1])
            (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
    (global-whitespace-mode 1)

    (set-face-attribute 'whitespace-trailing nil
                        :background "tomato"
                        :underline t)
    (set-face-attribute 'whitespace-tab nil
                        :background "deep sky blue"
                        :underline t)
    (set-face-attribute 'whitespace-space nil
                        :background "green"
                        :weight 'bold)
    (set-face-attribute 'whitespace-empty nil
                        :background "#313131")
    (setq-default tab-width 4 indent-tabs-mode nil)))

;; git gutter
(use-package git-gutter
  :config
  (progn
    (global-git-gutter-mode +1)
    ;; update-commands に e2wm のコマンドを入れていないと更新されない問題?
    (add-to-list 'git-gutter:update-commands 'e2wm:pst-history-forward-command)
    (add-to-list 'git-gutter:update-commands 'e2wm:pst-history-back-command)
    (set-face-foreground 'git-gutter:modified "black")
    (set-face-background 'git-gutter:modified "white")
    (set-face-foreground 'git-gutter:added "black")
    (set-face-background 'git-gutter:added "dark orange")
    (set-face-foreground 'git-gutter:deleted "black")
    (set-face-background 'git-gutter:deleted "blue")
    (custom-set-variables
     '(git-gutter:update-interval 1)
     '(git-gutter:window-width 2)
     '(git-gutter:modified-sign "❄")
     '(git-gutter:added-sign "☀")
     '(git-gutter:deleted-sign "☂"))))

(prefer-coding-system 'utf-8)
(setq ruby-insert-encoding-magic-comment nil)
(set-file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8-unix)

(use-package edit-server
  :config
  (edit-server-start))
