;;; jep-java.el --- Java configuration

;; Copyright (C) 2013  Jeff

;; Author: Jeff <jpace@eddie>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(message "Loading Java extensions...")

(add-hook 'java-mode-hook
	  '(lambda()
	     (setq tab-width 4)))

(defun jep:java-insert-print-break-line (char)
  "Inserts a line that prints 55 of the same characters. For debugging Java code."
  (interactive "cCharacter: ")
  (insert char))

(defun jep:java-insert-print-intro-line (char)
  "Inserts a line that prints 55 of the same characters. For debugging Java code."
  (interactive "cCharacter: ")
  (insert "System.out.println(\"")
  (insert char))

(defun jep:java-insert-log-variable (str)
  "Inserts a line that prints a variable to the log."
  (interactive "sString: ")
  (insert "tr.Ace.log(\"")
  (insert str)
  (insert ": \" + ")
  (insert str)
  (insert ");\n"))

(defun jep:java-add-log-current-word ()
  "Inserts a line that prints a variable to the log."
  (interactive)
  (let ((word (current-word)))
    (move-end-of-line nil)
    (newline)
    (c-indent-line-or-region)
    (insert "tr.Ace.log(\"" word "\", " word ");")
    (c-indent-line-or-region)))

(defun jep:java-new-class-without-main ()
  "Creates a new class, with a no-args constructor, and without a main function."
  (let ((name (jep:file-basename)))
    (insert "package ")
    (insert (jep:java-current-buffer-to-package-name) ";\n\n")
    (insert "import java.util.*;\n"
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
    (insert "package ")
    (insert (jep:java-current-buffer-to-package-name) ";\n\n")
    (insert "import java.io.*;\n"
	    "import java.util.*;\n"
	    "import junit.framework.TestCase;\n"
	    "\n"
	    "public class " name " extends TestCase {\n"
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
	   (re "^\\(.*/\\)test/\\(.*\\)Test.java\\([^<]*\\)"))
    
    (if (not (and (string-match re file)
		  (match-end 1) (match-end 2)))
	nil
      (setq pre  (substring file (match-beginning 1) (match-end 1)))
      (setq post (substring file (match-beginning 2) (match-end 2)))
      (message "2pre: %s post: %s" pre post)
      (setq testfile (concat pre post ".java"))
      (message "1testfile: %s" testfile)
      testfile)))

(defvar jep:java-test-to-source-substitutions 23
  "*Substitutions between test and source files")

(defun jep:java-get-file-test-companion (file)
  "Returns the test file for the given source. Returns nil if the path"
  "is of the form test/source or test/src."

  (let* 
      (pre post srcdir testfile
	   (re "^\\(.*/\\)\\(source\\|src\\)\\(/.*\\).java\\([^<]*\\)"))
    
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
	testfile))))

(defun jep:java-get-file-companion (file)
  "Returns the companion test/source file of the given file."
  (interactive)

  (let* ((companion (jep:java-get-file-test-companion file)))
    (message "1companion: %s" companion)
    (if (null companion)
	(setq companion (jep:java-get-file-source-companion file)))
    companion))

(defun jep:java-basename ()
  "Returns the file name, minus the directory and suffix."
  (let* ((bn (buffer-name))
         (namelist (jep:file-split bn))
         fn)

    (if (or (null namelist) (= 1 (length namelist)))
        nil
      
      ;; first in the name list is the file name; second is the extension
      (setq fn  (nth 0 namelist))
      fn)))

(defvar jep:java-test-source-patterns 
  '(("^\\(/Depot/work/project/trunk\\)/tests/junit\\(.*\\)Test.java" "\\1\\2.java")
    ("^\\(/Depot/work/project/trunk\\)/\\(.*\\).java"                "\\1/tests/junit/\\2Test.java")

    ; maven laytout, src/main/java/.../Foo.java <=> src/test/java/.../TestFoo.java:
    ("^\\(.*/src/\\)main\\(/java/.*\\)/\\(\\w+.java\\)$"             "\\1test\\2/Test\\3")
    ("^\\(.*/src/\\)test\\(/java/.*\\)/Test\\(\\w+.java\\)$"         "\\1main\\2/\\3")))

(defun jep:java-find-counterpart (fname)
  "*Toggles between a test and source Java file."
    (interactive)
  (jep:file-get-counterpart fname jep:java-test-source-patterns))

(defun jep:java-show-counterpart ()
  "*Toggles between a test and source Java file."
  (interactive)
  (let ((other (jep:java-find-counterpart (buffer-file-name))))
    (message (concat "other file " other))))

(defun jep:java-toggle-between-test-and-source ()
  "*Toggles between a test and source Java file."
  (interactive)
  (jep:file-toggle-files jep:java-test-source-patterns))

(defun jep:java-variable-to-constant (var)
  "*Converts the variable from camel case to a constant (uppercase and underscores)"
  (let ((case-replace t)
	(case-fold-search nil))
    (upcase (replace-regexp-in-string "\\(\\(?:[A-Z]\\|[0-9]+\\)\\)" "_\\1" var t nil))))

(defun jep:java-constant-to-variable (const)
  "*Converts the const (uppercase and underscores) to camel case"
  (let ((case-replace t)
	(case-fold-search nil))
    (replace-regexp-in-string "_\\([a-z0-9]\\)" 'capitalize (downcase const) t nil)))

(defun jep:java-toggle-variable-and-constant ()
  "*Toggles between a variable and a constant."
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

(defun jep:java-if-stmt-add-braces ()
  "*Goes to next if statement and adds braces."
  (interactive)
  (re-search-forward "\\(if\\|for\\) *(.*) *$")
  (insert " {")
  (re-search-forward ";")
  (insert "\n}")
  (c-indent-line-or-region))

;; from https://raw.github.com/dacap/home/master/.emacs
(defun jep:java-sort-imports ()
  "* Sorts the imports in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^import " nil t)
      (set-mark (line-beginning-position))
      (forward-line)
      (while (re-search-forward "^import " (line-end-position) t)
	(forward-line))
      (sort-lines nil (mark) (point)))))

(add-hook 'java-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-j t") 'jep:java-toggle-between-test-and-source)
	    (local-set-key (kbd "M-j t") 'jep:java-toggle-between-test-and-source)

	    (local-set-key (kbd "C-j l") 'jep:java-if-stmt-add-braces)

	    (local-set-key (kbd "C-j C-l") 'jep:java-add-log-current-word)

	    (local-set-key (kbd "C-j C-i") 'jep:java-sort-imports)))

(defun jep:java-path-to-package-name (path)
  (replace-regexp-in-string "/" "." 
			    (replace-regexp-in-string "/\\w+\.java" "" 
						      (replace-regexp-in-string "^.*?\\(source\\|src/main/java\\|src/integTest/java\\|src/test/java\\)/" "" path))))

(defun jep:java-current-buffer-to-package-name ()
  (jep:java-path-to-package-name (buffer-file-name)))

(defun jep:java-insert-package-name ()
  "Adds the package statement, based on the file name."
  (interactive)
  (insert "package ")
  (insert (jep:java-current-buffer-to-package-name) ";\n"))

(add-to-list 'auto-mode-alist '("\.java$" . java-mode))

(message "Java extensions loaded.")

;; (require 'jep-eclim)

(provide 'jep-java)
;;; jep-java.el ends here
