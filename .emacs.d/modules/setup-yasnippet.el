(use-package yasnippet
  :init
  (progn
    (yas-global-mode 1)
    (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))
    (bind-keys :map yas-minor-mode-map
	       ("<tab>" . nil)
	       ("TAB" . nil)
	       ("C-i" . nil)
	       ("C-o" . yas/expand)
	       ("C-x C-n" . yas/new-snippet))

    (eval-after-load "helm"
      '(progn (defun my/yas-helm-prompt (prompt choices &optional display-fn)
		"Use helm to select a snippet. Put this into `yas/prompt-functions.'"
		(interactive)
		(setq display-fn (or display-fn 'identity))
		(if (require 'helm-config)
		    (let (tmpsource cands result rmap)
		      (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
		      (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
		      (setq tmpsource
			    (list
			     (cons 'name prompt)
			     (cons 'candidates cands)
			     '(action . (("Expand" . (lambda (selection) selection))))
			     ))
		      (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
		      (if (null result)
			  (signal 'quit "user quit!")
			(cdr (assoc result rmap))))
		  nil))
	      (setq yas/prompt-functions '(my/yas-helm-prompt yas-ido-prompt yas-no-prompt))))))
