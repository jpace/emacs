;;; Ruby extensions


;; Ruby mode
(add-hook 'ruby-mode-hook 
	  '(lambda()
	     (setq 
	      indent-tabs-mode   nil
	      )))

(defun jep:ruby-new-file ()
  (interactive)
  (insert "#!/usr/bin/ruby -w\n"
	  "# -*- ruby -*-\n"
	  "\n"
	  "require 'rubygems'\n"
	  "require 'riel'\n"
	  "\n"))

(provide 'jep:ruby)
