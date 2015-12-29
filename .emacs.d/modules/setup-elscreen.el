;;; setup-elscreen.el --- elscreen-settings

;;; Commentary:

;;; Code:

;; (use-package elscreen
;;   :config
;;   (progn
;;     (setq elscreen-prefix-key (kbd "C-t"))
;;     (setq elscreen-tab-display-kill-screen nil)
;;     (setq elscreen-tab-display-control nil)
;;     (setq elscreen-buffer-to-nickname-alist
;;        '(("^dired-mode$" .
;;           (lambda ()
;;             (format "Dired(%s)" dired-directory)))
;;          ("^Info-mode$" .
;;           (lambda ()
;;             (format "Info(%s)" (file-name-nondirectory Info-current-file))))
;;          ("^mew-draft-mode$" .
;;           (lambda ()
;;             (format "Mew(%s)" (buffer-name (current-buffer)))))
;;          ("^mew-" . "Mew")
;;          ("^irchat-" . "IRChat")
;;          ("^liece-" . "Liece")
;;          ("^lookup-" . "Lookup")))
;;     (setq elscreen-mode-to-nickname-alist
;;        '(("[Ss]hell" . "shell")
;;          ("compilation" . "compile")
;;          ("-telnet" . "telnet")
;;          ("dict" . "OnlineDict")
;;          ("*WL:Message*" . "Wanderlust")))
;;     (elscreen-start)))
