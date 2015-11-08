;;; Ruby extensions

;; Ruby mode
(add-hook 'ruby-mode-hook 
	  '(lambda()
	     (setq 
	      indent-tabs-mode nil)))

(defun jep:ruby-file-to-class-name (filename)
  (replace-regexp-in-string "_" "" (capitalize filename) t nil))

(defun jep:ruby-new-library ()
  (interactive)
  (let ((name (jep:file-basename)))
    (insert "#!/usr/bin/ruby -w\n"
	    "# -*- ruby -*-\n"
	    "\n"
	    "require 'pathname'\n"
	    "\n"
	    "class " (jep:ruby-file-to-class-name name) "\n"
	    "  def initialize\n"
	    "  end\n"
	    "end\n")))

(defun jep:ruby-new-program ()
  (interactive)
  (let ((name (jep:file-basename)))
    (insert "#!/usr/bin/ruby -w\n"
	    "# -*- ruby -*-\n"
	    "\n"
	    "dir = File.dirname(File.dirname(File.expand_path(__FILE__)))\n"
	    "libpath = dir + \"/lib\"\n"
	    "$:.unshift libpath\n"
	    "\n"
	    "require 'pathname'\n"
	    "\n"
	    "class " (jep:ruby-file-to-class-name name) "\n"
	    "  def initialize args\n"
	    "  end\n"
	    "end\n"
	    "\n"
	    (capitalize name) ".new ARGV\n")))

(defun jep:ruby-file-to-class-name (filename)
  (replace-regexp-in-string "_" "" (capitalize filename) t nil))

(defun jep:ruby-new-test ()
  (interactive)
  (let ((name (jep:file-basename)))
    (insert "#!/usr/bin/ruby -w\n"
	    "# -*- ruby -*-\n"
	    "\n"
	    "require 'minitest/autorun'\n"
	    "require 'pathname'\n"
	    "\n"
	    "class " (jep:ruby-file-to-class-name name) " < Minitest::Test\n"
	    "  def test_something\n"
	    "  \n"
	    "  end\n"
	    "end\n")))

(defun jep:ruby-new-file ()
  "Creates a new Ruby file."
  "    standard - Does not contain a main function."
  "    application - Contains a main function. The default."
  "    JUnit - Derived from junit.framework.TestCase. Does not contain a main function."
  (interactive)
  (let (type)
    (setq type (read-from-minibuffer "Type [l(ibrary), p(rogram), t(est)]: "))
    (if (or (= (length type) 0)
	    (string-match type "l"))
	(jep:ruby-new-library)
      (if (string-match type "p")
	  (jep:ruby-new-program)
	(if (string-match type "t")
	    (jep:ruby-new-test))))))

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
    
    ("^\\(.*/\\)app/controllers/\\(.*\\).rb"           "\\1test/controllers/\\2_test.rb")
    ("^\\(.*/\\)test/controllers/\\(.*\\)_test.rb"     "\\1app/controllers/\\2.rb")
    
    ("^\\(.*/\\)app/models/\\(.*\\).rb"           "\\1test/models/\\2_test.rb")
    ("^\\(.*/\\)test/models/\\(.*\\)_test.rb"     "\\1app/models/\\2.rb")
    
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
