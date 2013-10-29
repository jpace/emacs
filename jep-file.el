;;; extensions to file functions
;;; author: jeugenepace at gmail dot com

(defun jep:file-split (file)
  "Split file name x.y by dot, returning the list (x y)."
  (let* (fn
	 ;; extended so that foo.cpp<2> => foo.cpp
	 (re  "^\\(.*\\)[.]\\([^<]*\\)")
         ext)

    (if (not (and (string-match re file)
                  (match-end 1) (match-end 2)))
        nil
      (setq fn  (substring file (match-beginning 1) (match-end 1)))
      (setq ext (substring file (match-beginning 2) (match-end 2))))

    ;; return a list containing the file name and the extension
    (if (and fn ext)
        (list fn ext)
      nil)))

(defun jep:file-basename ()
  "Returns the file name, minus the directory, extension, and trailing </tmp>,
unlike file-name-sans-extension, which includes that."
  (let* ((bn (buffer-name))
         (namelist (jep:file-split bn))
         fn)

    (if (or (null namelist) (= 1 (length namelist)))
        nil
      
      ;; first in the name list is the file name; second is the extension
      (setq fn (nth 0 namelist))
      fn)))

(defun jep:file-insert-basename ()
  "Inserts the basename at the current point."
  (interactive)
  (let* ((fn (jep:file-basename))
         )
    ;; Not doing a save-excursion, because we want to go to the end of what we
    ;; inserted.
    (insert fn)))

(defun jep:file-get-counterpart (fname patterns)
  (let (pattern lhs rhs)
    (progn 
      (if (null patterns)
	  nil
	(setq pattern (car patterns))
	(setq lhs     (nth 0 pattern))
	(setq rhs     (nth 1 pattern))
	(setq other   (and (string-match lhs fname) (replace-regexp-in-string lhs rhs fname)))
	(message (concat "other>> " other " <<"))
	(if (or (null other) (not (file-exists-p other)))
	    (jep:file-get-counterpart fname (cdr patterns))
	  other)))))

(defun jep:file-toggle-files (patterns)
  "*Toggles between the current buffer and another file, based on the given patterns."
  
  (let ((other (jep:file-get-counterpart (buffer-file-name) patterns)))
    (if (null other)
	(message "no companion file")
      (if (setq buf (get-buffer other))
	  (switch-to-buffer buf)
	(switch-to-buffer (find-file-noselect other))))))

(defun jep:reindent-buffer ()
  "Reindents the entire buffer."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max))))

(provide 'jep:file)
