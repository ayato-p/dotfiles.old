;; Determine `user-emacs-directory'.
(when load-file-name
  (setq user-emacs-directory (expand-file-name
                              (file-name-directory load-file-name))))

;; Customization variables
(defgroup my/settings nil
  "My settings."
  :group 'emacs)

;; Loading Cask, configuring paths...
(load (locate-user-emacs-file "bootstrap"))

;; Load modules
(require 'core-loader)

(setq my/modules (list
                  "setup-modeline"
                  "setup-basic"
                  "setup-key-bindings"
                  "setup-gtags"
                  "setup-lisp"
                  "setup-clojure-mode"
                  "setup-e2wm"
                  "setup-auto-complete-mode"
                  "setup-yasnippet"
                  "setup-web-mode"
                  "setup-org-mode"
                  "utils"))

(my/load-modules)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "☀")
 '(git-gutter:deleted-sign "☂")
 '(git-gutter:modified-sign "❄")
 '(git-gutter:update-interval 1)
 '(git-gutter:window-width 2)
 '(helm-gtags-auto-update t)
 '(helm-gtags-fuzzy-match t)
 '(helm-gtags-ignore-case t)
 '(safe-local-variable-values
   (quote
    ((cider-cljs-repl . "(zou.framework.repl/cljs-repl)")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-result-overlay-face ((((class color) (background dark)) (:foreground "#DCDCCC"))))
 '(e2wm:face-history-list-normal ((t (:foreground "LightGoldenrod1"))))
 '(eval-sexp-fu-flash ((((class color)) (:background "#F0DFAF" :foreground "#3F3F3F"))))
 '(helm-ls-git-added-copied-face ((t :foreground "#AFD8AF")))
 '(helm-ls-git-added-modified-face ((t :foreground "#8CD0D3")))
 '(helm-ls-git-conflict-face ((t :foreground "#DC8CC3")))
 '(helm-ls-git-deleted-and-staged-face ((t :foreground "#DCDCCC")))
 '(helm-ls-git-deleted-not-staged-face ((t :foreground "#D0BF8F")))
 '(helm-ls-git-modified-and-staged-face ((t :foreground "#DFAF8F")))
 '(helm-ls-git-modified-not-staged-face ((t :foreground "#F0DFAF")))
 '(helm-ls-git-renamed-modified-face ((t :foreground "#DFAF8F")))
 '(helm-ls-git-untracked-face ((t :foreground "#DCA3A3"))))
