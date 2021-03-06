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
	    "require 'test/unit'\n"
	    "require 'pathname'\n"
	    "\n"
	    "class " (jep:ruby-file-to-class-name name) " < Test::Unit::TestCase\n"
	    "  def test_init\n"
	    "    x = " (jep:ruby-file-to-class-name name) ".new args\n"
	    "    assert_equal expected, result\n"
	    "  \n"
	    "  end\n"
	    "end\n")))

(defun jep:ruby-new-file ()
  "Creates a new Ruby file."
  "    library (the default)"
  "    program"
  "    test"
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

(defun jep:ruby-new-file-guess ()
  "Creates a new Ruby file."
  (interactive)
  (let ((name (jep:file-basename)))
    (if (string-match "/lib/" name)
	(jep:ruby-new-library)
      (if (string-match "/test/" name)
	  (message "test")
	(if (string-match "/bin/" name)
	    (jep:ruby-new-test)
	  (jep:ruby-new-file))))))

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

(defun jep:ruby-add-line (str)
  "Inserts a string as a separate, indented line."
  (interactive)
  (move-end-of-line nil)
  (newline)
  (ruby-indent-line)
  (insert str)
  (ruby-indent-line))

(defun jep:ruby-output-current-word (str)
  "Inserts a line that writes the word to standard output."
  (interactive)
  (let* ((word (current-word))
	 (line (concat str " \"" word "\: #{" word "}\"")))
    (jep:ruby-add-line line)))

(defun jep:ruby-puts-current-word ()
  "Inserts a line that writes the word to standard output."
  (interactive)
  (jep:ruby-output-current-word "puts"))

(defun jep:ruby-info-current-word ()
  "Inserts a line that writes the word to info (logging)."
  (interactive)
  (jep:ruby-output-current-word "info"))

(defun jep:current-variable ()
  "Returns the current word, containing only word characters (A-Za-z0-9)"
  (current-word nil t))

(defun jep:ruby-assign-current-variable ()
  "Inserts a line that assigns as @var = var."
  (interactive)
  (let* ((word (jep:current-variable))
	 (line (concat "@" word " = " word)))
    (jep:ruby-add-line line)))

(add-hook 'ruby-mode-hook
	  (lambda ()
	    ;; (local-set-key (kbd "C-j t") 'jep:ruby-toggle-between-test-and-source)
	    (local-set-key (kbd "M-j t") 'jep:ruby-toggle-between-test-and-source)
	    (local-set-key (kbd "M-j i") 'jep:ruby-toggle-between-integration-test-and-source)
	    
	    (local-set-key (kbd "C-j C-p") 'jep:ruby-puts-current-word)
	    (local-set-key (kbd "M-j M-p") 'jep:ruby-puts-current-word)
	    
	    (local-set-key (kbd "C-j C-l") 'jep:ruby-puts-current-word)
	    (local-set-key (kbd "M-j M-l") 'jep:ruby-puts-current-word)
	    
	    (local-set-key (kbd "C-j C-i") 'jep:ruby-info-current-word)
	    (local-set-key (kbd "M-j M-i") 'jep:ruby-info-current-word)

	    (local-set-key (kbd "C-j @") 'jep:ruby-assign-current-variable)
	    (local-set-key (kbd "M-j @") 'jep:ruby-assign-current-variable)))

(add-to-list 'auto-mode-alist '("\.rb$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"  . ruby-mode))

(provide 'jep-ruby)
