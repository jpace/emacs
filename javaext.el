;;; Java extensions
;;; author: jeugenpace at gmail dot com

(message "Loading Java extensions...")

(defun jep:java-insert-print-break-line (char)
  "Inserts a line that prints 55 of the same characters. For debugging Java code."
  (interactive "cCharacter: ")
  (insert char)
  )

(defun jep:java-insert-print-intro-line (char)
  "Inserts a line that prints 55 of the same characters. For debugging Java code."
  (interactive "cCharacter: ")
  (insert "System.out.println(\"")
  (insert char)
  )

(defun jep:java-insert-log-variable (str)
  "Inserts a line that prints a variable to the log."
  (interactive "sString: ")
  (insert "tr.Ace.log(\"")
  (insert str)
  (insert ": \" + ")
  (insert str)
  (insert ");\n")
  )

(defun jep:java-insert-get-set (type name)
  "Inserts getFoo and setFoo functions for foo."
  (interactive "sType: \nsName: ")
  (let* ((cname (capitalize name)))
    (progn
      (insert "    public " type " get" cname "()\n")
      (insert "    {\n")
      (insert "        return _" name ";\n")
      (insert "    }\n")
      (insert "    \n")
      (insert "    public " "void" " set" cname "(" type " " name ")\n")
      (insert "    {\n")
      (insert "        _" name " = " name ";\n")
      (insert "    }\n")
      (insert "    \n")
      )))

(defun jep:java-new-class-without-main ()
  "Creates a new class, with a no-args constructor, and without a main function."
  (let ((name (jep:file-basename)))
    (insert "package xxx;\n"
	    "\n"
	    "import java.util.*;\n"
	    "\n"
	    "public class " name " {\n"
	    "    public " name "() {\n"
	    "    }\n"
	    "\n"
	    "}\n")))

(defun jep:java-new-class-with-main ()
  "Creates a new class, with a no-args constructor and a main function."
  (let ((name (jep:file-basename)))
    (insert "import java.io.*;\n"
	    "import java.util.*;\n"
	    "\n"
	    "public class " name " {\n"
	    "    public " name "() {\n"
	    "    }\n"
	    "\n"
	    "    public static void main(String[] args) {\n"
	    "    }\n"
	    "\n"
	    "}\n")))


(defun jep:java-new-junit-class ()
  "Creates a new class, with a no-args constructor and a main function."
  (let ((name (jep:file-basename)))
    (insert "package xxx;\n"
	    "\n"
	    "import java.io.*;\n"
	    "import java.util.*;\n"
	    "import junit.framework.TestCase;\n"
	    "\n"
	    "public class " name " extends TestCase {\n"
	    "\n"
	    "    public " name "(String name) {\n"
	    "        super(name);\n"
	    "    }\n"
	    "    \n"
	    "}\n")))

(defun jep:java-new-file ()
  "Creates a new class. User is prompted for the following:"
  "    standard - Does not contain a main function."
  "    application - Contains a main function. The default."
  "    JUnit - Derived from junit.framework.TestCase. Does not contain a main function."
  (interactive)
  (let (type)
    (setq type (read-from-minibuffer "Type [s, a, j]: "))
    (if (or (= (length type) 0)
	    (string-match type "a"))
	(jep:java-new-class-with-main)
      (if (string-match type "s")
	  (jep:java-new-class-without-main)
	(if (string-match type "j")
	    (jep:java-new-junit-class))))))

(defun jep:java-get-file-source-companion (file)
  "If the file is a test file, returns the source file. Otherwise returns nil."

  (let* 
      (pre post testfile srcfile srcdir
	   (re "^\\(.*/\\)test/\\(.*\\)Test.java\\([^<]*\\)")
	   )
    
    (if (not (and (string-match re file)
		  (match-end 1) (match-end 2)))
	nil
      (setq pre  (substring file (match-beginning 1) (match-end 1)))
      (setq post (substring file (match-beginning 2) (match-end 2)))
      (message "2pre: %s post: %s" pre post)
      (setq testfile (concat pre post ".java"))
      (message "1testfile: %s" testfile)
      testfile
      )))

(defvar jep:java-test-to-source-substitutions 23
  "*Substitutions between test and source files")

(defun jep:java-get-file-test-companion (file)
  "Returns the test file for the given source. Returns nil if the path"
  "is of the form test/source or test/src."

  (let* 
      (pre post srcdir testfile
	   (re "^\\(.*/\\)\\(source\\|src\\)\\(/.*\\).java\\([^<]*\\)")
	   )
    
    (if (not (and (string-match re file)
		  (match-end 1) (match-end 2)))
	(message "2file: %s" file)
      (setq pre    (substring file (match-beginning 1) (match-end 1)))
      (setq srcdir (substring file (match-beginning 2) (match-end 2)))
      (setq post   (substring file (match-beginning 3) (match-end 3)))

      ;; make sure that pre doesn't end with /test:
      (if (string-match "^.*/test/$" pre)
	  nil
	(setq testfile (concat pre "test/" srcdir post "Test.java"))
	(message "1testfile: %s" testfile)
	testfile
	)
      )
    )
  )

(defun jep:java-get-file-companion (file)
  "Returns the companion test/source file of the given file."
  (interactive)

  (let* ((companion (jep:java-get-file-test-companion file)))
    
    (message "1companion: %s" companion)
    (if (null companion)
	(setq companion (jep:java-get-file-source-companion file)))
    companion
    )
  )

(defun jep:java-get-file-companion-orig (file)
  "Returns the companion test/source file of the given file."
  (let* ((companion (jep:java-get-file-test-companion file)))

    (if (companion)
	companion
      (progn
	(setq companion (jep:java-get-file-source-companion file))
	(if (companion)
	    (progn 
	      (message "1companion: %s", companion)
	      )
	  (progn
	    (message "1no companion")
	    nil
	)
	  )
	)
      nil
      )
    )
  )

(defun jep:java-basename ()
  "Returns the file name, minus the directory and suffix."
  (let* ((bn (buffer-name))
         (namelist (jep:file-split bn))
         fn
         )

    (if (or (null namelist) (= 1 (length namelist)))
        nil
      
      ;; first in the name list is the file name; second is the extension
      (setq fn  (nth 0 namelist))
      fn)))

(defun jep:java-toggle-test-file ()
  "Switches from the current .java file to its unit test, if any."
  (interactive)
  (jep:java-switch-or-create-file t))

(defvar jep:java-test-source-patterns 
  '(("^\\(/Depot/work/project/trunk\\)/tests/junit\\(.*\\)Test.java" "\\1\\2.java")
    ("^\\(/Depot/work/project/trunk\\)/\\(.*\\).java"                "\\1/tests/junit/\\2Test.java")
    ("^\\(.*/\\)\\(source\\|src\\)"                                  "\\1test/\\2")
    ("^\\(.*/\\)\\(test\\)/"                                         "\\1")))

(defun jep:java-get-counterpart (fname patterns)
  (let (pattern lhs rhs)
    (progn 
      (if (null patterns)
	  nil
	(setq pattern (car patterns))
	(setq lhs     (nth 0 pattern))
	(setq rhs     (nth 1 pattern))
	(or (and (string-match lhs fname) (replace-regexp-in-string lhs rhs fname))
	    (jep:java-get-counterpart fname (cdr patterns)))))))

(defun jep:java-find-counterpart (fname)
  "*Toggles between a test and source Java file."
  
  (interactive)
  (jep:java-get-counterpart fname jep:java-test-source-patterns))
       
(defun jep:java-toggle-between-test-and-source ()
  "*Toggles between a test and source Java file."
  
  (interactive)
  (let ((other (jep:java-find-counterpart (buffer-file-name))))
    (if (null other)
	(message("no companion file"))
      (if (setq buf (get-buffer other))
	  (switch-to-buffer buf)
	(switch-to-buffer (find-file-noselect other))))))

(defun jep:java-switch-or-create-file (create)
  "Switches from the current .java file to its unit test, if any."
  (interactive)
  (let* ((bfn (buffer-file-name))
         (companion (jep:java-get-file-companion bfn))
         fn
         )

    ;; if we got a file to be loaded
    (if (null companion) 
	;; (message "no companion file")
	()
      
      ;; switch to the buffer if it's already loaded
      (if (setq bp (get-buffer companion))
	  ;; go to that file, companioning it if needed
	  (switch-to-buffer bp)
	
	(if (or create (file-exists-p companion))
	    ;; load it and switch to it
	    (switch-to-buffer (find-file-noselect companion))
	  
	  ;; doh!
	  (message (concat "File " companion " does not exist."))
	  )))))

(defun jep:java-variable-to-constant (var)
  "*Converts the variable from camel case to a constant (uppercase and underscores)"
  
  (let ((case-replace t)
	(case-fold-search nil))
    (upcase (replace-regexp-in-string "\\(\\(?:[A-Z]\\|[0-9]+\\)\\)" "_\\1" var t nil))))

(defun jep:java-constant-to-variable (const)
  "*Converts the const (uppercase and underscores) to camel case"
  
  (let ((case-replace t)
	(case-fold-search nil))
    (replace-regexp-in-string "_\\([a-z0-9]\\)" "\\U\\1" (downcase const) t nil)))

(defun jep:java-toggle-variable-and-constant ()
  "*Toggles between a variable and a constant.
"
  (interactive)
  (re-search-backward "[^A-Za-z0-9_]")
  (forward-char-command 1)
  (let ((start (point))
	(case-fold-search nil))
    (re-search-forward "[^A-Za-z0-9_]")
    (backward-char-command 1)
    (kill-region start (point))
    (setq 
     var  (current-kill 0)
     repl (if (string-match "^[A-Z0-9_]+$" var)
	      (jep:java-constant-to-variable var)
	    (jep:java-variable-to-constant var)))
    (insert repl)))

(defun jep:xxx-java-if-stmt-add-braces ()
  "*Goes to next if statement and adds braces.
"
  (interactive)
  (re-search-forward "\\(if\\|for\\) *(.*) *$")
  (insert " {")
  (re-search-forward ";")
  (insert "\n}")
  (c-indent-line-or-region)
  )

(message "Java extensions loaded.")

(provide 'jep:javaext)
