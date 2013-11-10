;;; Perl extensions
;;; author: jeugenpace at gmail dot com

;; perl mode for complex data structures (Emacs chokes on the format)
(defun perl-ds-mode ()
  "Emacs doesn't understand Perl very well == fundamental mode with tabs set to 4."
  (interactive)
  (fundamental-mode)
  (setq tab-width 4
	indent-tabs-mode nil
	mode-name "Emacs doesn't understand Perl very well"))
;;
;;** Perl mode
;; regular perl mode
(add-hook 'perl-mode-hook 
	  '(lambda()
	     (setq 
	      tab-width          4
	      indent-tabs-mode   nil
	      paragraph-separate "[ 	\f]*$" ; Perl mode also mucks this up.
	      )))
;;
;;** CPerl mode
;; regular cperl mode (not sure which I'm keeping)
(add-hook 'cperl-mode-hook 
	  '(lambda()
	     (setq 
	      tab-width          4
	      indent-tabs-mode   nil
	      paragraph-separate "[ 	\f]*$" ; Perl mode also mucks this up.
	      cperl-indent-level 4	; who uses 2?
	      cperl-invalid-face nil
	      )))

(defun jep:perl-new-file ()
  (interactive)
  (insert "#!/usr/bin/perl -w\n"
	  "# -*- perl -*-\n"
	  "\n"
	  "use strict;\n"
	  "\n"))

(add-to-list 'auto-mode-alist '("\.p[lm]$" . perl-mode))

(provide 'jep-perl)
