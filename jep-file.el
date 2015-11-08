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
  (indent-region (point-min) (point-max)))

(defun jep:toggle-read-only ()
  "Toggle the current buffer as read only."
  (interactive)
  (if buffer-read-only
      (shell-command (concat "chmod u+w " (buffer-file-name)))
    (shell-command (concat "chmod u-w " (buffer-file-name))))
  (message "")
  (toggle-read-only))

;;*** find-file advice
;; Prompt for a file that does not exist.
(defadvice find-file (around confirm-new-file)
  "Prompts to create the file, if it does not exist and there is no prefix"
  "argument to find-file. Thus, C-u C-x C-f will skip the prompt."
  (let ((file (ad-get-arg 0)))
    (if (or (file-exists-p file)	; load it if it does exist, of course
	    (or current-prefix-arg	; prefix argument bypasses the test
		(or (not (interactive-p)) ; otherwise, ask to create it
		    (yes-or-no-p
		     (format "`%s' does not exist, create buffer? " file))
		    )))
	ad-do-it
      )))

(ad-activate 'find-file)

(setq completion-ignored-extensions		; don't do file name completion on
      (append 
       (list "~" "\\.class" "\\.obj" "\\.o")	; backup files, .class files (Java), and object files (C/C++)
       completion-ignored-extensions))

;;*** Use crypt++ for automatic switching between Unix and DOS files
(require 'crypt++)

;; For multiple buffers with the same basename, instead of, for example
;; "Foo.txt<2>", this displays "Foo.txt<bar>".
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(add-hook 'find-file-hooks 'auto-insert)
(load-library "autoinsert")
(setq auto-insert-alist
      (append '(((java-mode . "Java Mode") . jep:java-new-file))
	      '(((c++-mode  . "C++ Mode")  . jep:c++-new-file))
	      '(((perl-mode . "Perl Mode") . jep:perl-new-file))
	      '((("\\.erb"  . "ERB Mode")  nil  ""))
	      '(((ruby-mode . "Ruby Mode") . jep:ruby-new-file))
	      auto-insert-alist))

(define-key jep:keymap "O" 'jep:toggle-read-only)
(define-key jep:keymap "B" 'jep:file-insert-basename)

(provide 'jep-file)
