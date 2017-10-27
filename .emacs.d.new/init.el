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
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t
      use-package-always-pin "melpa-stable"
      use-package-verbose t)

;;; load your preferred theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;;; set up exec-path
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;;; happy (((()))) !!!
(use-package paren
  :init
  (setq show-paren-style 'parenthesis)
  (show-paren-mode 1))

;;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("C-i" . nil)
              ("C-o" . yas-expand))
  :config
  (yas-global-mode 1))

;;; comapany-mode!
(use-package company
  :diminish company-mode
  :bind (:map company-mode-map
              ("C-i" . company-complete)
              ("TAB" . nil)
              :map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-s" . company-search-words-regexp)
              ("C-h" . nil)
              :map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-h" . nil))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t))

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

(use-package iflipb
  :bind (:map global-map
              ("C-." . iflipb-next-buffer)
              ("C-," . iflipb-previous-buffer)))

;;; answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;;; turn off graphical user interface if you want
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

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

(setq-default
 header-line-format
 '(""
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)))

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

(defun my/basic-mode-hook ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; key-bindings
;;;

;;; global keymap
(use-package bind-key
  :config
  (bind-keys :map global-map
             ("C-h" . delete-backward-char)
             ("C-;" . highlight-symbol)))

(use-package hydra
  :config
  (defhydra hydra-git-commands (global-map "C-x g")
    "Git"
    ("g" magit-status "Show git status")
    ("n" git-gutter:next-hunk "Next hunk")
    ("p" git-gutter:previous-hunk "Previous hunk")
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
  (mykie:initialize))

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
;;; helm
;;;

(use-package helm
  :bind (("M-x" . helm-M-x)
         :map helm-map
         ("C-h" . nil))
  :init
  (setq helm-quick-update t
        helm-buffers-fuzzy-matching t
        helm-ff-transformer-show-only-basename nil))

(use-package helm-projectile
  :commands (helm-projectile-find-file helm-projectile-switch-to-buffer)
  :init
  (mykie:set-keys nil
    "C-x C-f"
    :default (call-interactively 'find-file)
    :C-u helm-projectile-find-file
    :C-u*2! helm-ls-git-ls
    "C-x b"
    :default (call-interactively 'switch-to-buffer)
    :C-u helm-projectile-switch-to-buffer
    :C-u*2! helm-buffers-list)
  :config
  (use-package projectile
    :config
    (projectile-mode 1))
  (use-package helm-ls-git))

(use-package helm-ag
  :bind ("C-x C-g" . helm-do-ag-project-root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; programming languages
;;;

(use-package aggressive-indent
  :commands aggressive-indent-mode)

(defun my/prog-mode-hook ()
  (aggressive-indent-mode 1)
  (company-mode 1))

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
  (rainbow-delimiters-mode 1)
  (eldoc-mode 1))

(defun my/lisp-mode-hook ()
  (my/lisp-mode-defaults))

(use-package lisp-mode
  :ensure nil
  :mode (("\\.lisp\\'" . lisp-mode)
         ("\\.el\\'" . emacs-lisp-mode))
  :init
  (add-hook 'emacs-lisp-mode-hook 'my/prog-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'my/lisp-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;
;;;
;;; clojure
;;;

(use-package clojure-mode
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljx\\'" . clojurex-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :commands cider-jack-in
  :config
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  (add-hook 'clojure-mode-hook #'my/basic-mode-hook)
  (add-hook 'clojure-mode-hook #'my/prog-mode-hook)
  (add-hook 'clojure-mode-hook #'my/lisp-mode-hook)

  (use-package cider
    :diminish subword-mode git-gutter-mode
    :functions (cider-repl-toggle-pretty-printing)
    :bind (:map cider-mode-map
                ("C-x *" . my/zou-go))
    :config
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-repl-mode-hook #'my/lisp-mode-hook)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)

    (setq nrepl-log-messages t
          cider-repl-display-help-banner nil
          cider-repl-display-in-current-window t
          cider-repl-use-clojure-font-lock t
          cider-save-file-on-load 'always-save
          cider-font-lock-dynamically '(macro core function var)
          cider-overlays-use-font-lock t)
    (cider-repl-toggle-pretty-printing)

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

  (use-package clj-refactor
    :diminish clj-refactor-mode
    :config (cljr-add-keybindings-with-prefix "C-c j")))

(message "init.el loaded!!")

(add-hook 'after-init-hook
          (lambda ()
            (message "init time: %.3f sec"
                     (float-time (time-subtract after-init-time before-init-time)))))
