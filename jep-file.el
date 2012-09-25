;;; extensions to file functions
;;; author: jeugenepace at gmail dot com

(defun jep:file-split (file)
  "Split file name x.y by dot, returning the list (x y)."
  (let* (fn
	 ;; extended so that foo.cpp<2> => foo.cpp
	 (re  "^\\(.*\\)[.]\\([^<]*\\)")
         ext)

    (if (not (and (string-match re file)
                  (match-end 1) (match-end 2) ))
        nil
      (setq fn  (substring file (match-beginning 1) (match-end 1)))
      (setq ext (substring file (match-beginning 2) (match-end 2))))

    ; return a list containing the file name and the extension
    (if (and fn ext)
        (list fn ext)
      nil)))

(defun jep:file-basename ()
  "Returns the file name, minus the directory and suffix."
  (let* ((bn (buffer-name))
         (namelist (jep:file-split bn))
         fn)

    (if (or (null namelist) (= 1 (length namelist)))
        nil
      
      ;; first in the name list is the file name; second is the extension
      (setq fn  (nth 0 namelist))
      fn)))

(defun jep:file-insert-basename ()
  "Inserts the basename at the current point."
  (interactive)
  (let* ((fn (jep:file-basename))
         )
    ;; Not doing a save-excursion, because we want to go to the end of what we
    ;; inserted.
    (insert fn)))

(provide 'jep:file)
