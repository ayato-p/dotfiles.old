(load (locate-user-emacs-file "init"))

(defun directory-files-recursive (direcotry match)
  (let* ((files '())
         (current-directory-list
          (directory-files direcotry t)))
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond
         ((and (file-regular-p f)
               (file-readable-p f)
               (string-match match f))
          (setq files (cons f files)))

         ((and (file-directory-p f)
               (file-readable-p f)
               (not (string-equal ".." (substring f -2)))
               (not (string-equal "." (substring f -1))))
          (setq files (append files (directory-files-recursive f match))))
         (t)))

      (setq current-directory-list (cdr current-directory-list)))
    files))

(defun re-format-file (file)
  (find-file file)
  (indent-region (point-min) (point-max))
  (save-buffer))

(let* ((dir-name (car argv))
       (match (cadr argv))
       (matched-files (directory-files-recursive (car argv) (cadr argv))))
  (mapcar 're-format-file matched-files))
