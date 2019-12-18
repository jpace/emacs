;;; jep-ibuffer.el --- Configuration for ibuffer

;; Copyright (C) 2013  Jeff Pace

;; originally stolen from http://emacs-fu.blogspot.com/2010/02/dealing-with-many-buffers-ibuffer.html

(require 'ibuffer) 
(require 'str)

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

(defvar jep:projects-directory "/opt/incava")
(defvar jep:projects-incava-directory (concat jep:projects-directory "/opt/incava/"))

(defvar jep:filename-subs 
  '(("/home/jpace" . "~")
    ("/opt/sag/is" . "~is")
    ("/opt/incava" . "~incava")
    ("/home/jpace/.emacs.d/lisp" . "~lisp")
    ("/home/jpace/.config/zsh" . "~zsh")
    ("/$" . "")))

(define-ibuffer-column dirname
  (:name "Directory"
	 :inline nil)
  (if (buffer-file-name buffer)
      (str-replace-all (file-name-directory (buffer-file-name buffer)) jep:filename-subs)
    (or dired-directory
	"")))

;; this is in incubation state; it somewhat does the same as the 'dirname'
;; column above, replacing the patterns, but it does not strip the filename.
(setq ibuffer-directory-abbrev-alist
      '(
	("/opt/sag" . "~sag")
	("/opt/sag/dcx" . "~dcx")
	("/opt/sag/dcc2" . "~dcc2")
	("/opt/sag/grace" . "~grace")
	("/opt/sag/sown" . "~sown")
	("/opt/sag/yenkins" . "~yenkins")
	("/opt/sag/yira" . "~yira")
	("/opt/incava" . "~incava")
	("/opt/incava/attest" . "~attest")
	("/opt/incava/blog" . "~blog")
	("/opt/incava/diffj" . "~diffj")
	("/opt/incava/doctorj" . "~doctorj")
	("/opt/incava/glark" . "~glark")
	("/opt/incava/ijdk" . "~ijdk")
	("/opt/incava/joda-schedule" . "~joda")
	("/opt/incava/pmdx" . "~pmdx")
	("/opt/incava/qualog" . "~qualog")
	))

(setq ibuffer-formats
      '((mark modified read-only " "
	      (name 30 30 :left :elide)
	      " "
	      (size 9 -1 :right)
	      " " dirname)
	(mark modified read-only " "
	      (name 30 30 :left :elide)
	      " "
	      (size 9 -1 :right)
	      " " filename)
	(mark modified read-only " "
	      (name 30 30 :left :elide)
	      " "
	      (size 9 -1 :right)
	      " " filename-and-process)
	(mark " "
	      (name 30 30 :left :elide)
	      " " filename-and-process)))

(define-ibuffer-sorter filename-or-dired
  "Sort the buffers by their pathname."
  (:description "filenames plus dired")
  (string-lessp 
   (with-current-buffer (car a)
     (or buffer-file-name
	 (if (eq major-mode 'dired-mode)
	     (expand-file-name dired-directory))
	 ;; so that all non pathnames are at the end
	 "~"))
   (with-current-buffer (car b)
     (or buffer-file-name
	 (if (eq major-mode 'dired-mode)
	     (expand-file-name dired-directory))
	 ;; so that all non pathnames are at the end
	 "~"))))
(define-key ibuffer-mode-map (kbd "s p")     'ibuffer-do-sort-by-filename-or-dired)
  
(global-set-key (kbd "C-l") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-b") 'ibuffer)
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "M-RET") 'quick-switch-buffer)

(provide 'jep-ibuffer)
;;; jep-ibuffer.el ends here
