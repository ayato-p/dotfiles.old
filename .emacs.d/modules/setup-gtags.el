(use-package helm-gtags
  :config
  (progn
    (custom-set-variables
     '(helm-gtags-fuzzy-match t)
     '(helm-gtags-ignore-case t)
     '(helm-gtags-auto-update t))

    (add-to-list 'helm-gtags--prompt-alist '(tag-without-ns . "Find Definition: "))

    (defun my/helm-gtags--read-tagname (type &optional default-tagname)
      (let ((tagname (helm-gtags--token-at-point type))
            (prompt (assoc-default type helm-gtags--prompt-alist))
            (comp-func (assoc-default type helm-gtags-comp-func-alist)))
        (if (and tagname helm-gtags-use-input-at-cursor)
            tagname
          (when (and (not tagname) default-tagname)
            (setq tagname default-tagname))
          (when (eq type 'tag-without-ns)
            ;; (first (last (split-string (symbol-name (symbol-at-point)) "/"))
            (setq tagname (first (last (split-string tagname "/")))))
          (when tagname
            (setq prompt (format "%s(default \"%s\") " prompt tagname)))
          (let ((completion-ignore-case helm-gtags-ignore-case)
                (completing-read-function 'completing-read-default))
            (completing-read prompt comp-func nil nil nil
                             'helm-gtags--completing-history tagname)))))

    (bind-keys :map helm-gtags-mode-map
               ("<f4>" . helm-gtags-find-tag-from-here))
    (bind-keys :prefix-map helm-gtags-mode-map
               :prefix "C-c"
               ("M-t" . helm-gtags-find-tag)
               ("M-r" . helm-gtags-find-rtag)
               ("M-s" . helm-gtags-find-symbol)
               ("C-t" . helm-gtags-pop-stack))))
