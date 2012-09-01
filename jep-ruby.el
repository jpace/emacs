;;; Ruby extensions

;; Ruby mode
(add-hook 'ruby-mode-hook 
	  '(lambda()
	     (setq 
	      indent-tabs-mode nil
	      )))

(defun jep:ruby-new-file ()
  (interactive)
  (insert "#!/usr/bin/ruby -w\n"
	  "# -*- ruby -*-\n"
	  "\n"
	  "require 'rubygems'\n"
	  "require 'riel'\n"
	  "\n"))

(defvar jep:ruby-test-source-patterns 
  '(("^\\(/Depot/work/project/trunk\\)/tests/junit\\(.*\\)Test.java" "\\1\\2.java")
    ("^\\(/Depot/work/project/trunk\\)/\\(.*\\).java"                "\\1/tests/junit/\\2Test.java")

    ;; should support multiple conventions here ...
    ("^\\(.*/\\)lib/\\(.*\\).rb"                            "\\1test/unit/\\2_test.rb")
    ("^\\(.*/\\)test/unit/\\(.*\\)_test.rb"                 "\\1lib/\\2.rb")))

(defun jep:ruby-get-counterpart (fname patterns)
  (let (pattern lhs rhs)
    (progn 
      (if (null patterns)
	  nil
	(setq pattern (car patterns))
	(setq lhs     (nth 0 pattern))
	(setq rhs     (nth 1 pattern))
	(or (and (string-match lhs fname) (replace-regexp-in-string lhs rhs fname))
	    (jep:ruby-get-counterpart fname (cdr patterns)))))))

(defun jep:ruby-find-counterpart (fname)
  "*Toggles between a test and source Java file."
  
  (interactive)
  (jep:ruby-get-counterpart fname jep:ruby-test-source-patterns))

(defun jep:ruby-show-counterpart ()
  "*Toggles between a test and source Java file."
  
  (interactive)
  (let ((other (jep:ruby-find-counterpart (buffer-file-name))))
    (message (concat "other file " other))))

(defun jep:ruby-toggle-between-test-and-source ()
  "*Toggles between a test and source Java file."
  
  (interactive)
  (let ((other (jep:ruby-find-counterpart (buffer-file-name))))
    (if (null other)
	(message "no companion file")
      (if (setq buf (get-buffer other))
	  (switch-to-buffer buf)
	(switch-to-buffer (find-file-noselect other))))))

(add-hook 'ruby-mode-hook
	  (lambda ()
	    ;; (local-set-key (kbd "C-j t") 'jep:ruby-toggle-between-test-and-source)
	    (local-set-key (kbd "M-j t") 'jep:ruby-toggle-between-test-and-source)
	    )
	  )

(provide 'jep:ruby)
