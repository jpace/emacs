;;* Jeff Pace's tweaks for ibuffer.
;; originally stolen from http://emacs-fu.blogspot.com/2010/02/dealing-with-many-buffers-ibuffer.html

(require 'ibuffer) 
(setq ibuffer-saved-filter-groups
      (quote (("default"      
	       ;; ("dired" (mode . dired-mode))
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
	       ("glark"
		(filename . "org/incava/glark"))
	       ("doctorj"
		(filename . "org/incava/doctorj"))
	       ("emacs"
		(or
		 (filename . ".emacs.d/lisp")
		 (filename . "System/Emacs")))
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

;; Enable ibuffer-filter-by-filename to filter on directory names too.
(eval-after-load "ibuf-ext"
  '(define-ibuffer-filter filename
     "Toggle current view to buffers with file or directory name matching QUALIFIER."
     (:description "filename"
		   :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
     (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
			(buffer-local-value 'dired-directory buf))
		    (string-match qualifier it))))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)
