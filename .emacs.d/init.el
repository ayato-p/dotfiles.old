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
                  "utils"))

(my/load-modules)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(column-number-mode t)
;;  '(display-time-mode t)
;;  '(git-gutter:added-sign "☀")
;;  '(git-gutter:deleted-sign "☂")
;;  '(git-gutter:modified-sign "❄")
;;  '(git-gutter:update-interval 1)
;;  '(git-gutter:window-width 2)
;;  '(menu-bar-mode nil)
;;  '(show-paren-mode t)
;;  '(tool-bar-mode nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(e2wm:face-history-list-normal ((t (:foreground "LightGoldenrod1")))))
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
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))
     (cider-cljs-repl . "(zou.framework.repl/cljs-repl)"))))
 '(wakatime-api-key "5292f201-d960-4344-862e-a2a5aa404bab")
 '(wakatime-cli-path "/usr/local/bin/wakatime"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(e2wm:face-history-list-normal ((t (:foreground "LightGoldenrod1")))))
