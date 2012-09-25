;;; Ruby extensions

;; Ruby mode
(add-hook 'ruby-mode-hook 
	  '(lambda()
	     (setq 
	      indent-tabs-mode nil)))

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

(defvar jep:ruby-integration-test-source-patterns 
  '(("^\\(.*/\\)lib/\\(.*\\).rb"                            "\\1test/integration/\\2_test.rb")
    ("^\\(.*/\\)test/integration/\\(.*\\)_test.rb"          "\\1lib/\\2.rb")))

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

(defun jep:ruby-find-counterpart (fname patterns)
  "*Toggles between a test and source Ruby file."  
  (jep:ruby-get-counterpart fname patterns))

(defun jep:ruby-find-test-counterpart (fname)
  "*Toggles between a test and source Ruby file."
  
  (interactive)
  (jep:ruby-find-counterpart fname jep:ruby-test-source-patterns))

(defun jep:ruby-show-counterpart ()
  "*Shows the other file, for toggling between a test and source Ruby file."
  
  (interactive)
  (let ((other (jep:ruby-find-counterpart (buffer-file-name))))
    (message (concat "other file " other))))

(defun jep:ruby-toggle-files (patterns)
  "*Toggles between a test and source Ruby file."
  
  (let ((other (jep:ruby-find-counterpart (buffer-file-name) patterns)))
    (if (null other)
	(message "no companion file")
      (if (setq buf (get-buffer other))
	  (switch-to-buffer buf)
	(switch-to-buffer (find-file-noselect other))))))

(defun jep:ruby-toggle-between-test-and-source ()
  "*Toggles between a test and source Ruby file."
  
  (interactive)
  (jep:ruby-toggle-files jep:ruby-test-source-patterns))

(defun jep:ruby-toggle-between-integration-test-and-source ()
  "*Toggles between a test and source Ruby file."
  
  (interactive)
  (jep:ruby-toggle-files jep:ruby-integration-test-source-patterns))

(add-hook 'ruby-mode-hook
	  (lambda ()
	    ;; (local-set-key (kbd "C-j t") 'jep:ruby-toggle-between-test-and-source)
	    (local-set-key (kbd "M-j t") 'jep:ruby-toggle-between-test-and-source)
	    (local-set-key (kbd "M-j i") 'jep:ruby-toggle-between-integration-test-and-source)))

(provide 'jep:ruby)
