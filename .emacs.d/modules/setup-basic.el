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
(electric-pair-mode t)

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

;; migemo
(use-package migemo
  :config
  (progn
    (setq migemo-command "cmigemo")
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (load-library "migemo")
    (migemo-init)))

(use-package helm-migemo
  :config
  (progn
    (defun helm-compile-source--candidates-in-buffer (source)
      (helm-aif (assoc 'candidates-in-buffer source)
          (append source
                  `((candidates
                     . ,(or (cdr it)
                            (lambda ()
                              ;; Do not use `source' because other plugins
                              ;; (such as helm-migemo) may change it
                              (helm-candidates-in-buffer (helm-get-current-source)))))
                    (volatile) (match identity)))
        source))
    ;; [2015-09-06 Sun]helm-match-plugin -> helm-multi-match変更の煽りを受けて
    (defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
    (defalias 'helm-mp-3-search-base 'helm-mm-3-search-base)))

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

(use-package smooth-scroll
  :config
  (progn
    (smooth-scroll-mode t)
    (setq smooth-scroll/vscroll-step-size 4)))
;; (require 'inertial-scroll)
;; (setq inertias-global-minor-mode-map
;;       (inertias-define-keymap
;;        '(
;;          ("<next>"  . inertias-up)
;;          ("<prior>" . inertias-down)
;;          ("C-v"     . inertias-up)
;;          ("M-v"     . inertias-down)
;;          ) inertias-prefix-key))
;; (inertias-global-minor-mode 1)

(use-package paren
  :init (progn (setq show-paren-style 'parenthesis)
               (show-paren-mode 1)))

(use-package rainbow-mode
  :init (progn (add-hook 'css-mode-hook 'rainbow-mode)
               (add-hook 'clojure-mode-hook 'rainbow-mode)
               (add-hook 'js2-mode-hook 'rainbow-mode)
               (add-hook 'web-mode-hook 'rainbow-mode)
               (add-hook 'emacs-lisp-mode 'rainbow-mode)))

(when window-system
  ;; (load-theme 'hc-zenburn t)
  ;; (load-theme 'gruvbox t)
  (load-theme 'ample t t)
  (load-theme 'ample-flat t t)
  (enable-theme 'ample-flat)

  (custom-set-faces
   '(e2wm:face-history-list-normal ((t (:foreground "LightGoldenrod1")))))

  (setq default-frame-alist '((font . "Inconsolata-10")))
  (set-default-font "Inconsolata-10")
  (set-face-font 'variable-pitch "Inconsolata-10")
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    '("Takaoゴシック" . "unicode-bmp"))

  ;; (set-frame-parameter nil 'fullscreen 'fullboth)

  (defun toggle-fullscreen ()
    (interactive)
    (set-frame-parameter nil 'fullscreen
                         (if (frame-parameter nil 'fullscreen)
                             nil 'fullboth)))
  (bind-keys :map global-map
             ("<f11>" . toggle-fullscreen)))

(use-package hl-line
  :init
  (progn
    (global-hl-line-mode 1)
    (set-face-background 'hl-line "#525252")))

;; mozc
(when (require 'mozc nil t)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay)
  ;; faces
  (set-face-attribute 'mozc-cand-overlay-even-face 'nil
                      :background "aquamarine" :foreground "black")
  (set-face-attribute 'mozc-cand-overlay-odd-face 'nil
                      :background "aquamarine" :foreground "black"))

(use-package skk
  :init
  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
  (setq default-input-method "japanese-skk")
  (use-package skk-study))

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

;; (use-package server
;;   :config (unless (server-running-p)
;;             (server-start)))

(prefer-coding-system 'utf-8-unix)
(setq ruby-insert-encoding-magic-comment nil)
(set-file-name-coding-system 'cp932)
(setq locale-coding-system 'utf-8-unix)
