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

(let ((user-custom-file (concat user-emacs-directory "user-custom.el")))
  (when (not (file-exists-p user-custom-file))
    (shell-command (concat "touch " user-custom-file)))
  (setq custom-file user-custom-file)
  (load custom-file))

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
