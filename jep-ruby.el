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
    ("^\\(.*/\\)lib/\\(.*\\).rb"                       "\\1test/unit/\\2_test.rb")
    ("^\\(.*/\\)test/unit/\\(.*\\)_test.rb"            "\\1lib/\\2.rb")
    
    ("^\\(.*/\\)lib/\\(.*\\).rb"                       "\\1test/integration/\\2_test.rb")
    ("^\\(.*/\\)test/integration/\\(.*\\)_test.rb"     "\\1lib/\\2.rb")
    
    ("^\\(.*/\\)lib/\\(.*\\).rb"                       "\\1spec/\\2_spec.rb")
    ("^\\(.*/\\)spec/\\(.*\\)_spec.rb"                 "\\1lib/\\2.rb")
    
    ("^\\(.*/\\)lib/\\(.*\\).rb"                       "\\1test/\\2_test.rb")
    ("^\\(.*/\\)test/\\(.*\\)_test.rb"                 "\\1lib/\\2.rb")))

(defvar jep:ruby-integration-test-source-patterns 
  '(("^\\(.*/\\)lib/\\(.*\\).rb"                       "\\1test/integration/\\2_test.rb")
    ("^\\(.*/\\)test/integration/\\(.*\\)_test.rb"     "\\1lib/\\2.rb")))

(defun jep:ruby-find-counterpart (fname patterns)
  "*Toggles between a test and source Ruby file."  
  (jep:file-get-counterpart fname patterns))

(defun jep:ruby-show-counterpart ()
  "*Shows the other file, for toggling between a test and source Ruby file."
  
  (interactive)
  (let ((other (jep:file-get-counterpart (buffer-file-name) jep:ruby-test-source-patterns)))
    (message (concat "other file >> " other " <<"))))

(defun jep:ruby-toggle-between-test-and-source ()
  "*Toggles between a test and source Ruby file."
  
  (interactive)
  (jep:file-toggle-files jep:ruby-test-source-patterns))

(defun jep:ruby-toggle-between-integration-test-and-source ()
  "*Toggles between a test and source Ruby file."
  
  (interactive)
  (jep:file-toggle-files jep:ruby-integration-test-source-patterns))

(add-hook 'ruby-mode-hook
	  (lambda ()
	    ;; (local-set-key (kbd "C-j t") 'jep:ruby-toggle-between-test-and-source)
	    (local-set-key (kbd "M-j t") 'jep:ruby-toggle-between-test-and-source)
	    (local-set-key (kbd "M-j i") 'jep:ruby-toggle-between-integration-test-and-source)))

(add-to-list 'auto-mode-alist '("\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))

(provide 'jep-ruby)
