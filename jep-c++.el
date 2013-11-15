;;; C++ extensions
;;; functions in support of C++ code
;;; author: jeugenepace at gmail dot com

(require 'jep-file)

(message "Loading C++ extensions...")

;;; ----------------------------------------------------------------------
(defun jep:c++-insert-include-project (file)
  "Adds the guards and include statement for a file, eg.,
#ifndef Foo_h
#include \"Foo.h\"
#endif
"
  (interactive "sHeader name (eg: \"Foo\"): ")
  (insert (concat "#ifndef " file "_h\n"))
  (insert (concat "#include \"" file ".h\"\n"))
  (insert "#endif\n")
  (insert "\n")
  )

;;; ----------------------------------------------------------------------
(defun jep:c++-insert-include-system-without-h (file prefix)
  "Adds the guards and include statement for a system file without a .h suffix, eg.,
#ifndef proj_Foo
#define proj_Foo
#include <Foo>
#endif
"
  (interactive "sHeader name (eg: \"vector\"): \nsProject prefix (eg: \"proj\"): ")
  (insert (concat "#ifndef " prefix "_" file "\n"))
  (insert (concat "#define " prefix "_" file "\n"))
  (insert (concat "#include <" file ">\n"))
  (insert "#endif\n")
  (insert "\n")
  )


;;; ----------------------------------------------------------------------
(defun jep:c++-insert-include-system-with-h (file prefix)
  "Adds the guards and include statement for a system file with a .h suffix, eg.,
#ifndef proj_Foo_h
#define proj_Foo_h
#include <Foo.h>
#endif
"
  (interactive "sHeader name (eg: \"syscall\"): \nsProject prefix (eg: \"proj\"): ")
  (insert (concat "#ifndef " prefix "_" file "_h\n"))
  (insert (concat "#define " prefix "_" file "_h\n"))
  (insert (concat "#include <" file ".h>\n"))
  (insert "#endif\n")
  (insert "\n")
  )

;;; ----------------------------------------------------------------------
(defun jep:c++-insert-include-self-guards ()
  "Inserts the appropriate guards based on the file (buffer) name, eg.,
#ifndef Foo_h
#define Foo_h

... file contents ...

#endif
"
  (interactive)
  (let* ((bn (buffer-name))
         (namelist (jep:file-split bn))
         fn ext load dir bp
         )

    (if (or (null namelist) (= 1 (length namelist)))
        nil
      
      ;; first in the name list is the file name; second is the extension
      (setq fn  (nth 0 namelist) 
	    ext (nth 1 namelist))

      (cond
       ;; must be a header file
       ((equal ext "h")
	(save-excursion
	  (beginning-of-buffer)
	  (insert (concat "#ifndef " fn "_" ext "\n"))
	  (insert (concat "#define " fn "_" ext "\n\n"))
	  
	  (end-of-buffer)
	  (insert (concat "\n#endif //! " fn "_" ext "\n"))
	  ))
       (t
	(message (concat "Extension " ext " is not that of a known C++ header file")))
       )
    )))



;;; ----------------------------------------------------------------------
(defun jep:c++-switch-or-create-file (fromh fromcpp create)
  "Switches the current buffer from X.h or X.cpp file to X.FROMH or X.FROMCPP,
respectively. Loads the target file if is is not already loaded, and assumes
that it is in the same directory as the current file. The create parameter
denotes whether to create the file, if it does not exist."
  (interactive)
  (let* ((bn (buffer-name))
         (namelist (jep:file-split bn))
         fn ext load dir bp
         )
    
    ;; return nothing if the current file doesn't have an extension
    (if (or (null namelist) (= 1 (length namelist)))
        nil

      ;; first in the name list is the file name; second is the extension
      (setq fn  (nth 0 namelist) 
	    ext (nth 1 namelist))
      (cond
       ;; X.cpp => X.FROMCPP
       ((equal ext "cpp")
        (setq load (concat fn "." fromcpp)))
       ;; X.h => X.FROMH
       ((equal ext "h")
        (setq load (concat fn "." fromh)))
       ;; else, we couldn't figure out the extension
       (t
        (message (concat "Extension " ext " is not known")))
       ))

    ;; if we got a file to be loaded
    (if (null load) nil
      
      ;; switch to the buffer if it's already loaded
      (if (setq bp (get-buffer load))
	  ;; go to that file, loading it if needed
          (switch-to-buffer bp)
	
        ;; otherwise, maybe it really doesn't exist, in which case we might or
        ;; might not't create an empty buffer
        (if (or create (file-exists-p load))
	    ;; load it and switch to it
	    (switch-to-buffer (find-file-noselect (concat (file-name-directory bn) load)))
	  
	  ;; doh!
	  (message (concat "File " load " does not exist."))
	  )))
    ))

;;; ----------------------------------------------------------------------
(defun jep:c++-get-companion-file ()
  "Switches from X.cpp to X.h, and vice versa, creating the other file if it doesn't exist."
  (interactive)
  ;; create it if it doesn't exist --------/
  (jep:c++-switch-or-create-file "cpp" "h" t)
  )

;;; ----------------------------------------------------------------------
(defun jep:c++-get-companion-file-nocreate ()
  "Switches from X.cpp to X.h, and vice versa. Does not create file if it doesn't exist."
  (interactive)
  (jep:c++-switch-or-create-file "cpp" "h" nil)
  )

;;; ----------------------------------------------------------------------
(defun jep:c++-get-test-file ()
  "Jumps from either X.cpp or X.h to X.t.cpp, creating it if it doesn't exist."
  (interactive)
  (jep:c++-switch-or-create-file "t.cpp" "t.cpp" t)
  )

(defun jep:c++-insert-template-declaration ()
  "Creates a minimal C++ declaration."
  (interactive)
  (let ((name (jep:file-basename)))
    (insert "class " name " {\n"
	    "public:\n"
	    "    " name "() {\n"
	    "    \n"
	    "    }\n"
	    "\n"
	    "};\n")))

(defun jep:c++-insert-template-implementation ()
  (interactive)
  (let ((name (jep:file-basename)))
    (insert "using namespace std;\n"
	    "using namespace xxx;\n"
	    "\n"
	    name "::" name "()\n"
	    "{\n"
	    "}\n"
	    "\n"
	    name"::~" name "()\n"
	    "{\n"
	    "}\n"
	    "\n"
	    "\n"
	    )))

(defun jep:c++-insert-companion-include ()
  "Include the header file for this (.cpp) file."
  (interactive)
  (let ((name (jep:file-basename)))
    (insert "#ifndef " name "_h\n"
	    "#include \"" name ".h\"\n"
	    "#endif\n"
	    "\n")))

(defun jep:c++-insert-standard-headers ()
  "Includes the standard headers, iostream and string."
  (interactive)
  ;; eventually, read this from a list:
  (insert "include <iostream>\n"
	  "include <string>\n"
	  "\n"))

(defun jep:c++-insert-main ()
  (interactive)
  (let ((name (jep:file-basename)))
    (insert "int main(int argc, char** argv)\n"
	    "{\n"
	    "    \n"
	    "    return 0;\n"
	    "}\n"
	    "\n")))

;;; ----------------------------------------------------------------------
(defun jep:c++-new-header-file ()
  "Creates a new C++ header, including its guards, and a minimal declaration."
  (interactive)
  (jep:c++-insert-include-self-guards))

(defun jep:c++-new-source-file ()
  "Creates a new C++ source file. The following are valid options:"
  "    standard (s) - This is a standard source file, i.e., the .cpp for some .h."
  "    includes (n) - Includes standard header files."
  "    declaration (d) - The declaration."
  "    implementation (I) - The implementation is added by default; this negates it.."
  "    main (m) - A main function."
  (interactive)
  (let (options)
    (setq options (read-from-minibuffer "Options [sndIm]: "))
    (if (string-match "s" options)
	(jep:c++-insert-companion-include))
    (if (string-match "n" options)
	(jep:c++-insert-standard-headers))
    (if (string-match "d" options)
	(jep:c++-insert-template-declaration))
    (if (or (= (length options) 0)
	    (not (string-match "I" options)))
	(jep:c++-insert-template-implementation))
    (if (string-match options "m")
	(jep:c++-insert-main))))

(defun jep:c++-new-file ()
  "Creates a new C++ file."
  (interactive)
  (let ((ext (file-name-extension (buffer-name))))
    (if (equal ext "h")
	(jep:c++-new-header-file)
      (jep:c++-new-source-file))))

(message "C++ extensions loaded.")

(provide 'jep-c++)
