;;* Jeff Pace's tweaks for ibuffer.
;; originally stolen from http://emacs-fu.blogspot.com/2010/02/dealing-with-many-buffers-ibuffer.html

(require 'ibuffer) 
(setq ibuffer-saved-filter-groups
      (quote (("default"      
	       ("dired" (mode . dired-mode))
	       ("Org" ;; all org-related buffers
		(mode . org-mode))  
	       ;; ("Mail"
	       ;;   (or  ;; mail-related buffers
	       ;;    (mode . message-mode)
	       ;;    (mode . mail-mode)
	       ;;    ;; etc.; all your mail related modes
	       ;;    ))
	       ("pvn"
		(filename . "org/incava/pvn"))
	       ("diffj"
		(filename . "org/incava/diffj"))
	       ("ijdk"
		(filename . "org/incava/ijdkproj"))
	       ("riel"
		(filename . "org/incava/riel"))
	       ("emacs"
		(filename . "System/Emacs"))
	       ("zsh"
		(filename . "System/Zsh"))
	       ("jdk"
		(filename . "com/sun/java"))
	       ("MyProject2"
		(filename . "src/myproject2/"))
	       ("Programming" ;; prog stuff not already in MyProjectX
		(or
		 (mode . c-mode)
		 (mode . java-mode)
		 (mode . perl-mode)
		 (mode . python-mode)
		 (mode . ruby-mode)
		 (mode . emacs-lisp-mode)
		 ;; etc
		 )) 
	       ("ERC"   (mode . erc-mode))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

