;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; general
;;;

;;; setup language env
(set-language-environment "UTF-8")

;;; setup your user-emacs-directory
(let* ((user-init-dir (file-name-as-directory (or (getenv "EMACS_USER_DIRECTORY")
                                                  user-emacs-directory))))
  (setq user-emacs-directory user-init-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; setup package.el
;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(when (not (require 'use-package nil t))
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; basic configurations
;;;

;;; enable use-package
(eval-when-compile
  (require 'use-package))
(use-package diminish)
(use-package bind-key)

(setq use-package-always-ensure t
      use-package-always-pin "melpa-stable"
      use-package-verbose t)
;;; -------------------------------------------------------------------------------------

(setq load-prefer-newer t)

;;; set up exec-path
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package undo-tree
  :pin melpa
  :diminish undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package bm
  :config
  (defface my/bm-face
    '((t (:inherit highlight)))
    "My bm highlight")

  (setq-default bm-buffer-persistence t)
  (setq bm-cycle-all-buffers t
        bm-face 'my/bm-face
        bm-persistent-face 'my/bm-face
        bm-highlight-style 'bm-highlight-line-and-fringe
        bm-repository-file (concat user-emacs-directory "bm-repository"))

  (add-hook 'after-init-hook #'bm-repository-load)
  (add-hook 'find-file-hooks #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save))))

(use-package beacon
  :config
  (beacon-mode))

;;; happy (((()))) !!!
(use-package paren
  :init
  (setq show-paren-style 'parenthesis
        show-paren-delay 0)
  (show-paren-mode 1))

;;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("C-i" . nil)
              ("C-o" . yas-expand)))

;;; comapany-mode!
(use-package company
  :diminish ""
  :bind (("C-i" . company-indent-or-complete-common)
         :map company-mode-map
         ("C-i" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-search-candidates)
         ("C-d" . company-quickhelp-manual-begin)
         ("C-h" . nil)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-h" . nil))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t)

  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-frontend
          company-echo-metadata-frontend))

  (add-hook 'company-mode-hook 'company-statistics-mode)

  (add-to-list 'company-backends
               '(company-capf :with company-dabbrev-code
                              :with company-yasnippet)))

(use-package company-quickhelp
  :diminish ""
  :bind (:map company-active-map
              ("C-d" . company-quickhelp-manual-begin))
  :config
  (setq company-quickhelp-delay nil))

(use-package company-statistics
  :commands company-statics-mode)

;;; magit
(use-package magit
  :commands magit-status)

(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1))

(use-package highlight-symbol
  :commands highlight-symbol)

(use-package subword
  :commands subword-mode)

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode)

(use-package shut-up)

(use-package restclient
  :pin melpa
  :commands restclient-mode)

(use-package iflipb
  :commands iflipb-interesting-buffers
  ;; :bind (:map global-map
  ;;             ("C-." . iflipb-previous-buffer)
  ;;             ("C-," . iflipb-next-buffer))
  )

(use-package tabbar
  :pin melpa
  :commands tabbar-mode
  :bind (("C-." . tabbar-forward)
         ("C-," . tabbar-backward))

  :init
  (add-hook 'find-file-hook #'tabbar-mode)

  :config
  (tabbar-mwheel-mode -1)

  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  (setq tabbar-buffer-groups-function nil
        tabbar-use-images nil
        tabbar-home-button nil
        tabbar-cycle-scope nil
        tabbar-buffer-list-function #'iflipb-interesting-buffers)

  (set-face-attribute
   'tabbar-default nil
   :background "brightblack"
   :foreground "white"
   :box nil)

  (set-face-attribute
   'tabbar-unselected nil
   :inherit 'tabbar-default
   :box nil)

  (set-face-attribute
   'tabbar-selected nil
   :background "white"
   :foreground "black"
   :box nil)

  (set-face-attribute
   'tabbar-modified nil
   :foreground "white"
   :box nil)

  (set-face-attribute
   'tabbar-selected-modified nil
   :background "white"
   :foreground "black"
   :box nil)

  (when window-system
    (set-face-attribute
     'tabbar-separator nil
     :height 1.5)))

(use-package visual-regexp
  :bind (("C-M-%" . vr/query-replace)))

;;; answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;;; turn off graphical user interface if you want
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;;; save recent files
(defmacro with-suppressed-message ($rest body)
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(use-package recentf
  :ensure nil
  :diminish recentf-mode
  :init
  (setq recentf-max-saved-items 2000
        recent-exclude '(".recentf" "recentf")
        recentf-auto-cleanup 300
        recentf-auto-cleanup-timer
        (run-with-idle-timer 30 t '(lambda () (shut-up (recentf-save-list)))))
  :config
  (recentf-mode 1))

;;; some useful settings
(setq visible-bell t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      echo-keystrokes 0.1
      create-lockfiles nil
      make-pointer-invisible t
      ;; disable to buckup funciton
      backup-inhibited t
      delete-auto-save-files t
      ;; completion ignore case (lower/upper)
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      inhibit-startup-message t)

;; show me empty lines after buffer end
(set-default 'indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)
(setq uniquify-buffer-name-style 'post-forward)

;; whitespace
(use-package whitespace
  :defer t
  :config
  (setq whitespace-style '(face
                           trailing
                           tabs
                           spaces
                           empty
                           space-mark
                           tab-mark))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (global-whitespace-mode 1)
  (setq-default tab-width 4 indent-tabs-mode nil))

;;; cleanup whitespace before file save
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; Window setting
;;;


(use-package frame-fns
  :ensure nil
  :load-path "elisp/frame-fns")

(use-package frame-cmds
  :ensure nil
  :load-path "elisp/frame-cmds")

(use-package moom
  :ensure nil
  :load-path "elisp/moom"
  :config
  (setq moom-ascii-font "Dejavu Sans Mono"
        moom-ja-font "TakaoGothic")
  (moom-set-font-size-input 12))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-exclude-modes '(org-mode)
        golden-ratio-exclude-buffer-regexp '("\\.org")))

;; (use-package hc-zenburn-theme
;;   :pin melpa
;;   :config
;;   (load-theme 'hc-zenburn t))

;; (use-package leuven-theme
;;   :pin melpa
;;   :config
;;   (load-theme 'leuven t))

(use-package neotree
  :commands neotree-toggle
  :config
  (setq neo-show-hidden-files t
        neo-create-file-auto-open t))

(use-package centered-cursor-mode
  :pin melpa
  :commands centered-cursor-mode)

;;; modeline
(setq display-time-string-forms
      '((format
         "%s/%s(%s) %s:%s" month day dayname 24-hours minutes))
      line-number-mode t
      column-number-mode t)
(display-time-mode 1)

;; http://flex.phys.tohoku.ac.jp/texi/eljman/eljman_142.html
(setq-default
 mode-line-format
 '(""
   mode-line-mule-info
   mode-line-modified
   " "
   mode-line-buffer-identification

   " / "
   (line-number-mode "L%l ")
   (column-number-mode "C%c ")
   (-3 . "%p")
   " / "
   mode-name
   minor-mode-alist "%n" mode-line-process
   " / "
   global-mode-string
   ))

;; (setq-default
;;  header-line-format
;;  '(""
;;    (:propertize (:eval (shorten-directory default-directory 30))
;;                 face mode-line-folder-face)
;;    (:propertize "%b"
;;                 face mode-line-filename-face)))

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; Utility function
;;;

(defun my/delete-current-line ()
  (interactive)
  (beginning-of-line)
  (kill-line))

(defun my/find-file-right (filename)
  (let ((new-buffer (find-file-noselect (expand-file-name filename)))
        (new-window (split-window-right)))
    (set-window-buffer new-window new-buffer)))

(defun my/find-file-below (filename)
  (let ((new-buffer (find-file-noselect (expand-file-name filename)))
        (new-window (split-window-below)))
    (set-window-buffer new-window new-buffer)))

(defun my/copy-file-name-to-clipboard ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; key-bindings
;;;

;;; global keymap
(use-package bind-key
  :config
  (bind-keys :map global-map
             ("C-'" . my/delete-current-line)
             ("s-q" . quoted-insert)
             ("C-h" . delete-backward-char)
             ("C-;" . highlight-symbol)
             ("C-x t" . toggle-truncate-lines)
             ("M-#" . my/copy-file-name-to-clipboard)))

(use-package hydra
  :config
  (defhydra hydra-goto (goto-map "")
    "Goto"
    ("o" occur "Occur")
    ("g" goto-line "Goto line")
    ("n" next-error "Next error")
    ("p" previous-error "Previous Error")
    ("m" set-mark-command "mark" :bind nil)
    ("q" nil "quit"))

  (defhydra hydra-bm (global-map "C-c b")
    "Bookmark"
    ("b" bm-toggle "toggle")
    ("n" bm-next "Next bookmark")
    ("p" bm-previous "Previous bookmark"))

  (defhydra hydra-git-commands (global-map "C-x g")
    "Git"
    ("g" magit-status "Show git status")
    ("n" git-gutter:next-hunk "Next hunk")
    ("p" git-gutter:previous-hunk "Previous hunk")
    ("d" git-gutter:popup-diff "Popup diff")
    ("q" nil "quit"))

  (defhydra hydra-highlight-symbol (global-map "C-c h")
    "Highlight symbol"
    ("n" highlight-symbol-next "Next symbol")
    ("p" highlight-symbol-prev "Previous symbol")
    ("r" highlight-symbol-query-replace "Replace")
    ("q" nil "quit"))

  (defhydra hydra-zoom (global-map "C-x")
    "Zoom"
    ("C-+" text-scale-increase "in")
    ("C--" text-scale-decrease "out")
    ("C-0" text-scale-adjust "adjust")
    ("q" nil "quit"))

  (defhydra hydra-window (global-map "C-x")
    "Window"
    ("o" other-window "other")
    ("q" nil "quit")))

(use-package mykie
  :config
  (setq mykie:use-major-mode-key-override t)
  (mykie:initialize)

  (defhydra hydra-rectangle (:exit t)
    "Rectangle"
    ("k" kill-rectangle "Kill")
    ("d" delete-rectangle "Delete")
    ("c" clear-rectangle "Clear")
    ("M-w" copy-rectangle-as-kill "Copy Rectangle")
    ("o" open-rectangle "Insert space")
    ("N" rectangle-number-lines "Number")
    ("t" string-rectangle "Replace")
    ("T" string-insert-rectangle "Insert"))

  (mykie:set-keys with-self-key
    "r"
    :region hydra-rectangle/body)

  (mykie:set-keys nil
    "C-y"
    :default yank
    :C-u yank-rectangle
    :C-u*2 counsel-yank-pop
    "C-x C-f"
    :default my/find-file
    :C-u ivy-git-ls
    "C-x b"
    :default ivy-switch-buffer
    :C-u (call-interactively 'switch-to-buffer)))

(use-package ace-jump-mode
  :config
  (defun add-keys-to-ace-jump-mode (prefix c &optional mode)
    (define-key global-map
      (read-kbd-macro (concat prefix (string c)))
      `(lambda ()
         (interactive)
         (funcall (if (eq ',mode 'word)
                      #'ace-jump-word-mode
                    #'ace-jump-char-mode) ,c))))

  (loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "H-" c))
  (loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "H-" c))
  (loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "H-M-" c 'word))
  (loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "H-M-" c 'word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; ivy
;;;


(use-package counsel
  :commands (counsel-find-file
             counsel-git
             ivy-switch-buffer
             hydra-my-counsel-git/body
             my/find-file)

  :bind (("M-x" . counsel-M-x)
         ("C-s" . swiper)
         ("C-r" . swiper)
         ("C-x C-g" . counsel-ag)
         ("C-x C-r" . ivy-recentf)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         :map swiper-map
         ("C-r" . ivy-previous-line))

  :config
  (require 'ivy)
  (setq ivy-use-virtual-buffers t
        ivy-height 20
        ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  (defvar my/--ivy-current-buffer "")

  (defun my/ivy-insert-current-directory ()
    (interactive)
    (insert my/--ivy-current-buffer))

  (defvar my/find-file-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-s") 'my/ivy-insert-current-directory)
      map))

  (defun my/find-file (&optional initial-input)
    (interactive)
    (let* ((current-directory (file-name-directory (or buffer-file-name default-directory)))
           (default-directory (expand-file-name (or (locate-dominating-file default-directory ".git")
                                                    current-directory)))
           (find-dir-cmd "find . -not -path '*\/.git*' | sort")
           (cands (split-string
                   (shell-command-to-string find-dir-cmd)
                   "\n"
                   t)))
      (setq my/--ivy-current-buffer
            (replace-regexp-in-string default-directory "" (expand-file-name current-directory)))
      (ivy-read "Choose: " cands
                :initial-input initial-input
                :action (lambda (x)
                          (let ((expanded-name (expand-file-name x)))
                            (if (file-directory-p expanded-name)
                                (counsel-find-file (concat expanded-name "/"))
                              (find-file expanded-name))))
                :caller 'my/find-file
                :keymap my/find-file-map)))

  (ivy-set-actions
   'my/find-file
   '(("@"
      my/find-file-below
      "find file below")
     ("#"
      my/find-file-right
      "find file right"))))

(use-package ivy-git-ls
  :ensure nil
  :load-path "myext"
  :commands ivy-git-ls

  :config
  (ivy-set-actions
   'ivy-git-ls
   '(("@"
      my/find-file-below
      "find file below")
     ("#"
      my/find-file-right
      "find file right"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; skk
;;;

(eval-when-compile
  (unless (require 'skk nil t)
    (package-install 'ddskk t)))

(use-package skk
  :ensure nil
  :config
  (setq-default skk-kutouten-type 'jp)
  (setq default-input-method "japanese-skk"
        skk-kuten-touten-alist (cons '(my-en "." . ",") skk-kuten-touten-alist)
        skk-kakutei-when-unique-candidate nil
        skk-egg-like-newline t
        skk-isearch-mode-enable nil
        skk-show-inline 'vertical
        skk-cdb-large-jisyo "/usr/share/skk/SKK-JISYO.LL.cdb"
        skk-auto-insert-paren nil)
  (setq skk-sticky-key ":")

  (add-to-list 'context-skk-programming-mode 'clojure-mode)

  (add-hook 'skk-load-hook
            (lambda ()
              (require 'context-skk)))

  (add-hook 'find-file-hook
            (lambda ()
              (skk-latin-mode t)))

  ;; conflict skk && paredit
  (defun paredit-newline/skk-kakutei (origfun &rest arglist)
    (apply (cond ((not skk-mode) origfun)
                 (t #'skk-kakutei))
           arglist))

  (advice-add 'paredit-newline :around #'paredit-newline/skk-kakutei)

  (defun paredit-newline/skk-insert (origfun &rest arglist)
    (apply (cond (skk-j-mode #'skk-insert)
                 (t origfun))
           arglist))

  (advice-add 'cljr-slash :around #'paredit-newline/skk-insert))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; for programming modes
;;;

(use-package aggressive-indent
  :commands aggressive-indent-mode)

(use-package origami
  :pin melpa
  :commands origami-mode
  :config
  (mykie:set-keys origami-mode-map
    "s-<tab>"
    :default origami-toggle-node
    :C-u origami-toggle-all-nodes))

(defun my/prog-mode-hook ()
  (aggressive-indent-mode 1)
  (company-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; org-mode
;;;

(use-package open-junk-file
  :pin melpa
  :bind (("C-x j" . open-junk-file))
  :config
  (setq open-junk-file-format "~/memo/junk/%Y-%m%d-%H%M%S."))

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :bind (:map org-mode-map
              ("C-," . nil))
  :config
  (setq org-directory "~/memo/junk"
        my/notebook-directory "~/notebook"
        org-agenda-files (list org-directory my/notebook-directory))

  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t)

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (concat my/notebook-directory "/todo.org") "Tasks")
           "* TODO %?\n %i\n %a")
          ("d" "Discussion" entry (file+headline (concat my/notebook-directory "/discussion.org") "Discussion")
           "%[~/notebook/templates/discussion.txt]")
          ("j" "Journal" entry (file+datetree (concat my/notebook-directory "/journal.org"))
           "* %?\n %U\n %i\n %a")
          ("n" "Note" entry (file+headline (concat my/notebook-directory "/notes.org") "Notes")
           "* %?\n %U\n %i")))

  (use-package ob
    :ensure org-plus-contrib
    :config
    (setq org-confirm-babel-evaluate nil)

    (use-package ob-clojure
      :ensure org-plus-contrib
      :config
      (setq org-babel-clojure-backend 'cider))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (scheme . t)
       (clojure . t)))))

(use-package org-tree-slide
  :bind (("<f8>" . org-tree-slide-mode)
         :map org-tree-slide-mode-map
         ("<right>" . org-tree-slide-move-next-tree)
         ("<left>" . org-tree-slide-move-previous-tree))
  :config

  (use-package org-bullets
    :config
    (add-hook 'org-tree-slide-mode-play-hook #'(lambda () (org-bullets-mode 1)))
    (add-hook 'org-tree-slide-mode-stop-hook #'(lambda () (org-bullets-mode -1))))
  (use-package hide-lines
    :config
    (defvar my/org-src-block-faces nil)
    (defun my/show-headers ()
      (setq org-src-block-faces 'my/org-src-block-faces)
      (hide-lines-show-all))

    (defun my/hide-headers ()
      (setq my/org-src-block-faces 'org-src-block-faces)
      (hide-lines-matching "#\\+BEGIN_SRC")
      (hide-lines-matching "#\\+END_SRC"))

    (add-hook 'org-tree-slide-mode-play-hook 'my/hide-headers)
    (add-hook 'org-tree-slide-mode-stop-hook 'my/show-headers)

    (defun advice/org-edit-src-code ()
      (interactive)
      (my/show-headers))
    (advice-add 'org-edit-src-code :before #'advice/org-edit-src-code)

    (defun advice/org-edit-src-exit ()
      (interactive)
      (my/hide-headers))
    (advice-add 'org-edit-src-exit :after #'advice/org-edit-src-exit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; XML
;;;

(use-package nxml-mode
  :ensure nil
  :config
  (setq nxml-child-indent 2
        nxml-attribute-indent 2
        nxml-slash-auto-complete-flag t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; Yaml
;;;

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :config
  (add-hook 'yaml-mode-hook #'subword-mode)
  (add-hook 'yaml-mode-hook #'company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; CSS
;;;

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode)
         ("\\.css\\'" . css-mode))
  :config
  (setq scss-compile-at-save nil)
  (add-hook 'scss-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '(company-css :with company-abbrev
                                 :with company-yasnippet)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; JavaScript
;;;

(use-package js2-mode
  :mode ("\\.js\\'"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; lisp
;;;

(use-package paredit
  :diminish paredit-mode
  :bind (:map paredit-mode-map
              ("C-h" . paredit-backward-delete))
  :config
  (defun conditionally-enable-paredit-mode ()
    (if (eq this-command 'eval-expression)
        (paredit-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode))

(use-package eldoc
  :commands eldoc-mode
  :init
  (setq eldoc-idle-delay 0.1
        eldoc-minor-mode-string ""))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(defun my/lisp-mode-defaults ()
  (paredit-mode 1)
  ;; (rainbow-delimiters-mode 1)
  (eldoc-mode 1)
  (origami-mode 1))

(defun my/lisp-mode-hook ()
  (my/lisp-mode-defaults))

(use-package lisp-mode
  :ensure nil
  :mode (("\\.lisp\\'" . lisp-mode)
         ("\\.el\\'" . emacs-lisp-mode))
  :bind (:map emacs-lisp-mode-map
              ("C-c C-m" . pp-macroexpand-last-sexp))
  :init
  (add-hook 'emacs-lisp-mode-hook 'my/prog-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'my/lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-elisp :with company-abbrev
                                           :with company-yasnippet))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; clojure
;;;

(use-package clojure-mode
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljx\\'" . clojurex-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :config
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'my/prog-mode-hook)
  (add-hook 'clojure-mode-hook #'my/lisp-mode-hook)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-capf :with company-dabbrev-code
                                          :with company-yasnippet)))))
  (put-clojure-indent 'go-loop-sub 3))

(use-package spiral
  :pin melpa)

(use-package cider
  ;; :diminish cider-mode
  :pin melpa
  :bind (:map cider-mode-map
              ("C-x *" . my/zou-go)
              ("C-x @" . my/split-window-below-and-switch-to-repl)
              ("C-x #" . my/split-window-right-and-switch-to-repl)
              ("C-c C-s" . cider-browse-spec)
              ("C-c s" . cider-browse-spec)
              ("C-c C-S" . cider-browse-spec-all)
              ("C-c S" . cider-browse-spec-all)
              :map cider-repl-mode-map
              ("C-x *" . my/zou-go))
  :commands (cider-mode cider-jack-in)
  :config
  (use-package dash
    :config

    (defun my/find-cider-repl-buffer ()
      (-first
       (lambda (b)
         (with-current-buffer b
           (derived-mode-p 'cider-repl-mode)))
       (buffer-list))))

  (defun my/split-window-below-and-switch-to-repl ()
    (interactive)
    (let* ((window (split-window-below))
           (buffer (my/find-cider-repl-buffer)))
      (set-window-buffer window buffer)))

  (defun my/split-window-right-and-switch-to-repl ()
    (interactive)
    (let* ((window (split-window-right))
           (buffer (my/find-cider-repl-buffer)))
      (set-window-buffer window buffer)))

  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'my/lisp-mode-hook)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)

  (setq nrepl-log-messages t
        cider-repl-display-help-banner nil
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-repl-result-prefix ";;=> "
        cider-save-file-on-load t
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)

  (use-package ivy
    :config
    (use-package mykie
      :config
      (require 'dash)

      (defun my/ivy-find-var-in-current-ns ()
        (interactive)
        (let* ((ns (cider-current-ns))
               (sexp (concat "(->> (ns-interns '" ns ")
                                   vals
                                   (map meta)
                                   (keep (comp seq (juxt :line :name)))
                                   (sort-by first))"))
               (candidate (->> (my/cider-eval sexp :list)
                               (-map (lambda (l)
                                       (let ((n (car l))
                                             (fname (symbol-name (cadr l))))
                                         (propertize fname
                                                     'value n
                                                     'display (concat (number-to-string n)
                                                                      " "
                                                                      fname))))))))
          (ivy-read "Find Var" candidate
                    :action (lambda (matched)
                              (goto-line (get-text-property 0 'value matched))))))

      (mykie:set-keys cider-mode-map
        "C-s"
        :default swiper
        :C-u my/ivy-find-var-in-current-ns)))

  (defun my/zou-go ()
    (interactive)
    (with-current-buffer (cider-current-connection "clj")
      (if current-prefix-arg
          (progn
            (save-some-buffers)
            (cider-interactive-eval
             "(zou.framework.repl/reset)"))
        (cider-interactive-eval
         "(zou.framework.repl/go)")))))

;; (use-package clomacs
;;   ;; :pin melpa
;;   :commands my/cider-eval
;;   :config
;;   (defun my/cider-eval (sexp &optional return-type return-value)
;;     (let* ((return-type (or return-type :string))
;;            (return-value (or return-value :value))
;;            (connection (cider-current-connection)))
;;       (clomacs-get-result
;;        (nrepl-sync-request:eval
;;         sexp
;;         connection)
;;        return-value return-type nil))))

(use-package clj-refactor
  :pin melpa
  :diminish clj-refactor-mode
  :commands clj-refactor-mode
  :config
  (cljr-add-keybindings-with-prefix "C-c j"))

(use-package inf-clojure
  :commands (inf-clojure inf-clojure-minor-mode)
  :bind (:map inf-clojure-mode-map
              ("C-c C-z" . inf-clojure-switch-to-repl)
              ("C-c C-k" . inf-clojure-load-file))
  :config
  (add-hook 'inf-clojure-mode-hook #'company-mode)
  (add-hook 'inf-clojure-mode-hook #'my/lisp-mode-hook))

;;; stop modify init.el from emacs
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

(message "init.el loaded!!")
