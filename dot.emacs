;;* Jeff Pace's Vainglorious .emacs file;;

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/vendor")

(setq
 enable-local-eval             t
 indent-tabs-mode              nil	; not using tabs
 default-tab-width             8	; tab == 8 spaces
 tab-width                     8	; ditto
 visible-bell                  nil	; turn off the annoying bell
 inhibit-startup-message       t	; ???
 line-number-mode              t	; show line numbers
 sentence-end-double-space     nil	; treat ". " as a sentence separator
 sentence-end                  "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*"
 scroll-step                   1	; scroll 1 line at a time
 auto-save-interval            2000	; save every 2000 operations
 auto-save-default             nil	; no autosaving
 gc-cons-threshold             500000	; garbage collection threshold
 default-fill-column           80
 default-major-node            'indented-text-mode
 font-lock-maximum-size        2000000	; fontify up to this size buffer
 column-number-mode            t	; show column numbers
 completion-ignored-extensions		; don't do file name completion on
 (append 
  (list "~" "\\.class" "\\.obj" "\\.o")	; backup files, .class files (Java), and object files (C/C++)
  completion-ignored-extensions)
 next-line-add-newlines        nil	; Disables down-arrow and C-n at the end
					; of a buffer from adding a new line to
					; that buffer.

 next-screen-context-lines     3	; Overlap between window-fulls when
					; scrolling by pages.

 apropos-do-all                t	; Thorough and slow.
 show-paren-mode               t	; makes parentheses match in color.
 mouse-avoidance-mode          'banish	; move mouse ptr out while typing.
 display-time-24hr-format      t	; military time
 frame-title-format            "%b - Emacs" ; full name (dir + file)
 icon-title-format             "%f - Emacs" ; short name (file only)
 paragraph-separate            "[ 	\f]*$" ; CC mode mucks this up to actual blank lines (no chars)
 transient-mark-mode           t	; show regions as they are highlighted.
 find-file-compare-truenames   t	; Use the truename of the file, not the base name.
 )
;;
;;* INITIALIZATION
;;** Emacs Version
;; Figure out which flavor of emacs we are running
(setq jep:this-is-xemacs (not (null (string-match "XEmacs" (emacs-version)))))
(setq jep:this-is-gnuemacs (string-match "GNU Emacs" (emacs-version)))

(setq special-display-buffer-names	; treat the compilation buffer as special
      (append `("*compilation*")))
;;
;;** my code
(load "jep-file")
(load "jep-java")
(load "jep-c++")
(load "jep-c")
(load "jep-perl")
(load "jep-ruby")
(load "jep-debug")			; for inserting debugging statements.
(load "work-env" t)			; environment at work
(load "jep-ccmode")			; customization for C* and Java files
(load "jep-text")
(load "jep-ibuffer")

;; there are some useful functions in ergoemacs, but I don't use all of the
;; keybindings
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us")
(load "ergoemacs-keybindings-5.3.9/ergoemacs-mode")
;; (ergoemacs-mode nil)
(cua-mode)				; ctrl-C (copy), -X (cut), -V (paste), -Z (undo)
;; but not ctrl-O (open) or ctrl-S (save)

(menu-bar-enable-clipboard)

;; my keybindings override those unset in ergoemacs, so it has to load afterward.
(load "keys")

(load "color-theme")			; in living color!
(load "color-theme-soren")		; my theme
(load "modeline")			; my modeline

(color-theme-soren)

;; (defalias 'list-buffers 'listbuf)

;; For multiple buffers with the same basename, instead of, for example
;; "Foo.txt<2>", this displays "Foo.txt<bar>".
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;
;;** Code from others
;;*** Buffer switching
;; This allows c-tab and c-s-tab switching between buffers, cycling around
;; Author: Igor Boukanov <boukanov@fi.uib.no>
(require 'pc-bufsw)
(cond (jep:this-is-gnuemacs
       (pc-bufsw::bind-keys 
	[C-tab]				; cycle to previous in buffer list
	[C-S-iso-lefttab]		; cycle to next in buffer list
	))
      (jep:this-is-xemacs
       (pc-bufsw::bind-keys 
	[(control tab)]
	[(control shift iso-lefttab)]
	)
       (turn-on-font-lock)
       ))

;;*** find-file advice
;; Prompt for a file that does not exist.
(defadvice find-file (around confirm-new-file)
  "Prompts to create the file, if it does not exist and there is no prefix"
  "argument to find-file. Thus, C-u C-x C-f will skip the prompt."
  (let ((file (ad-get-arg 0)))
    (if (or (file-exists-p file)	; load it if it does exist, of course
	    (or current-prefix-arg	; prefix argument bypasses the test
		(or (not (interactive-p)) ; otherwise, ask to create it
		    (yes-or-no-p
		     (format "`%s' does not exist, create buffer? " file))
		    )))
	ad-do-it
      )))

(ad-activate 'find-file)

;;
;;** New files
;;*** Automatic insertions
(add-hook 'find-file-hooks 'auto-insert)
(load-library "autoinsert")
(setq auto-insert-alist
      (append '(((java-mode . "Java Mode") . jep:java-new-file))
	      '(((c++-mode  . "C++ Mode")  . jep:c++-new-file))
	      '(((perl-mode . "Perl Mode") . jep:perl-new-file))
	      '(((ruby-mode . "Ruby Mode") . jep:ruby-new-file))
	      auto-insert-alist))

;; (add-hook 'find-file-not-found-hooks 'jep:new-file-hook)

;; Set tab width explicitly for text mode
(add-hook 'text-mode-hook
	  '(lambda()
	     (setq tab-width 8
		   indent-tabs-mode t
		   )))

;; Sets execute permission for saved files with '#!' in the first line.
(add-hook 'after-save-hook
          '(lambda ()
             (progn
               (and (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (save-match-data
                          (looking-at "^#!"))))
                    (shell-command (concat "chmod u+x " buffer-file-name))
                    (message (concat "Saved as script: " buffer-file-name))
                    ))))

;;*** Use crypt++ for automatic switching between Unix and DOS files
(require 'crypt++)

;;** Confirm close
;; Add Emacs close confirmation, since ctrl-x ctrl-c is too easy to hit
;; accidentally
(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really kill Emacs? "))
	    kill-emacs-query-functions))

;;** Java mode
;; Set tab width explicitly for text mode
(add-hook 'java-mode-hook
	  '(lambda()
	     (setq tab-width 4)))

;; Make java mode support Java 1.5 annotations.
;(require 'java-mode-indent-annotations)
;(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)

(cond (jep:this-is-gnuemacs
       (display-time)))			; Display current time in mode line

;; these are normally disabled
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'eval-expression  'disabled nil)
(put 'narrow-to-region 'disabled nil)

(cond (jep:this-is-gnuemacs
       (shell)
       (process-kill-without-query (get-process "shell"))))

(add-hook 'text-mode-hook 'turn-on-auto-fill nil)

(add-hook 'xml-mode-hook 
          (lambda () (auto-fill-mode -1)))

(add-hook 'html-mode-hook 'turn-on-auto-fill nil)

;;$$$ todo: add erb-mode, inheriting text-mode, without auto-fill

;;*** HTML mode
;; Just to keep from getting prompted every time I invoke HTML:
(setq html-helper-address-string "<a href=\"mailto:jpace *at* incava *.* org\">Jeffrey E. Pace</a>")

;; Filename extensions for modes
(setq auto-mode-alist
      (append '(("\\.txt$"	. text-mode)

		;; C
		("\\.c$"	. c-mode)

		;; C++
		("\\.h$"	. c++-mode) ; let's assume C++ .h files

		("\\.l$"	. c++-mode) ; lex
		("\\.y$"	. c++-mode) ; yacc
		("\\.idl$"	. c++-mode) ; IDL

		("\\.C$"	. c++-mode)
		("\\.H$"	. c++-mode)
		("\\.cc$"	. c++-mode)
		("\\.cpp$"      . c++-mode)
		("\\.hpp$"      . c++-mode)
		("\\.hh$"	. c++-mode)
		("\\.inl$"	. c++-mode) ; inline files (templates)

		;; emacs/lisp
		(".emacs"	. emacs-lisp-mode)
		("\\.el"	. emacs-lisp-mode)
		("\\.lisp$"	. lisp-mode)

		;; TeX
		("\\.tex$"	. latex-mode)
		("\\.sty$"	. latex-mode) ; TeX style files

		;; perl
		("\\.p[lm]?$"   . perl-mode)

		("[Mm]akefile"  . makefile-mode)
		("\\.mak$"      . makefile-mode)
		("\\.mk$"       . makefile-mode)
		("\\.gmk$"      . makefile-mode) ; Suffix in Sun files for GNU makefiles

		("\\.awk$"      . awk-mode)
		
		("\\.tcl$"      . tcl-mode)

		("\\.java$"     . java-mode)
		("\\.jpp$"      . java-mode)

		("\\.py$"       . python-mode)

		("\\.bat$"      . sh-mode)
		("\\.cmd$"      . sh-mode)
		("\\.ini$"      . sh-mode)
		("\\.sh$"       . sh-mode)
		("rc$"          . sh-mode)
		("^dot\\."      . sh-mode)
		("^dot\\."      . sh-mode)

		("\\..?html?$"  . html-mode)
		("\\.dsp$"      . html-mode) ; server pages
		("\\.php$"      . html-mode)

		("\\.xml?$"     . xml-mode) ; XML

		("\\.sql"       . sql-mode)  ; SQL

		("\\.quotes$"   . quip-mode) ; quip

		("\\.rb$"       . ruby-mode) ; Ruby
		("Rakefile$"    . ruby-mode) ; Ruby

		("\\.gradle$"   . groovy-mode) ; Gradle

		(""		. text-mode) ; default
		) auto-mode-alist))

(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)

(add-to-list 'auto-mode-alist        '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy"    . groovy-mode))

;;** font-lock
;; Font-locking faces set-up
;; Switch on font-lock for every mode which supports it.
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))

;; there seems to be inconsistency with the way colors are set in version 20,
;; which is what the following is for:
(cond ((fboundp 'global-font-lock-mode)
       ;; Customize face attributes
       (set-face-foreground 'font-lock-comment-face       "pink1")
       ;;(set-face-background 'font-lock-comment-face       "LightGray")
       
       ;; Load the font-lock package.
       (require 'font-lock)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)))

(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size
      ;; I'm using files that are too big, apparently
      ;; (if font-lock-maximum-decoration (* 70 1024) (* 150 1024)))
      (if font-lock-maximum-decoration (* 70 2048) (* 150 2048)))
;;
;;** Electric Buffer Menu mode customization.
(defun my-ebuf-stuff ()
  "My own Electric Buffer Menu stuff.  Currently binds some
convenience keys."
  (define-key electric-buffer-menu-mode-map [up]       'previous-line)
  (define-key electric-buffer-menu-mode-map [down]     'next-line)
  (define-key electric-buffer-menu-mode-map [next]     'scroll-up)
  (define-key electric-buffer-menu-mode-map [previous] 'scroll-down)
  (define-key electric-buffer-menu-mode-map [left]     'scroll-right)
  (define-key electric-buffer-menu-mode-map [right]    'scroll-left))
(add-hook 'electric-buffer-menu-mode-hook 'my-ebuf-stuff)

;; This fontifies the compilation buffer when compilation exits.
(defun my-compilation-finish-function (buf msg)
  "This is called after the compilation exits.  Currently just
highlights the compilation messages."
  (save-excursion
    (set-buffer buf)
    (font-lock-fontify-buffer)))
(setq compilation-finish-function 'my-compilation-finish-function)

(defun eliminate-all-tabs ()
  "Convert all tabs to spaces in the entire buffer."
  (interactive "*")
  (untabify (point-min) (point-max))
  (message "TABS converted to SPACES"))

;;; This installs the `saveplace' package and defines where the places
;;; in visited files are saved between sessions.
(condition-case err
    (require 'saveplace)
  (error
   (message "Cannot save places %s" (cdr err))))
(setq-default save-place t)		; save places in all files
(setq save-place-file	 "~/.emacsloc")

;; automatically advance one entry (day) in my journal, skipping to
;; the next month (and year) if necessary, including making the
;; appropriate directories.
(defun get-todays-journal-entry ()
  "Gets the file for today in the journal directory hierarchy."
  (interactive)
  (setq name (string-to-int (file-name-nondirectory (buffer-file-name))))
  (setq dir  (file-name-directory (buffer-file-name)))
  (setq dttm (decode-time))
  (yes-or-no-p
   (format "Journal entry: %d in %s <time %s>" name dir dttm)))

(cond (jep:this-is-xemacs
       (defun jep:toggle-menubar ()
	 (interactive)
	 (set-specifier menubar-visible-p
			(not (specifier-instance menubar-visible-p))))
       (defun jep:toggle-modeline ()
	 (interactive)
	 (set-specifier has-modeline-p
			(not (specifier-instance has-modeline-p))))
       (defun jep:toggle-toolbar ()
	 (interactive)
	 (set-specifier default-toolbar-visible-p
			(not (specifier-instance default-toolbar-visible-p))))
       (global-set-key [(meta f12)] 'jep:toggle-modeline)
       (global-set-key [(meta f11)] 'jep:toggle-menubar)
       (global-set-key [(meta f10)] 'jep:toggle-toolbar)
       (global-set-key [(control button3)] 'popup-menubar-menu)
       (set-specifier default-toolbar-visible-p nil)
       (set-specifier menubar-visible-p nil)))

;;** jep:find-file-from-list
;;============================================================
(defun jep:find-file-from-list ()
  (interactive)
  (let* ((bol (or (beginning-of-line) (point)))
	 (eol (or (end-of-line) (point)))
	 (line (buffer-substring bol eol))
	 (file (or (and (string-match "^\\([^:]+\\):" line)
			(substring line (match-beginning 1) (match-end 1)))
		   line)))
    (next-line 1)
    (beginning-of-line)
    (if (and file
	     (file-readable-p file))
	(find-file file))))
;;
;;** jep:insert-time-stamp
;;============================================================
(defun jep:insert-time-stamp ()
  (interactive)
  (insert (current-time-stamp)))
;;
;;** jep:toggle-read-only
;;============================================================
(defun jep:toggle-read-only ()
  (interactive)
  (if buffer-read-only
      (shell-command (concat "chmod u+w " (buffer-file-name)))
    (shell-command (concat "chmod u-x " (buffer-file-name))))
  (message "")
  (toggle-read-only))
;;
;;** jep:toggle-case-fold-search
(defun jep:toggle-case-fold-search ()
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (if case-fold-search
      (message "case sensitive search OFF")
    (message "case sensitive search ON")))
;;
;;** jep:point-to-top
(defun jep:point-to-top ()
  (interactive)
  (recenter 0))
;;
;;** jep:point-to-bottom
(defun jep:point-to-bottom ()
  (interactive)
  (recenter -1))
;;
;;** jep:move-up
(defun jep:move-up ()
  (interactive)
  (scroll-up 1))
;;
;;** jep:move-up
(defun jep:move-up ()
  (interactive)
  (scroll-up 1))
;;
;;** jep:move-down
(defun jep:move-down ()
  (interactive)
  (scroll-down 1))

;;
;;** jep:toggle-tab-width
(defun jep:toggle-tab-width ()
  (interactive)
  (if (= tab-width 4)
      (setq tab-width 8)
      (setq tab-width 4)
      ))
;;
;;** jep:insert-data-format
(defvar jep:insert-date-format "%A, %d %B %Y"
  "*Format for \\[jep:insert-date].")

;;
;;** jep:insert-date
(defun jep:insert-date ()
  "Insert the current date, using jep:insert-date."
  (interactive "*")
  (insert (format-time-string jep:insert-date-format (current-time))))

(defconst Electric-buffer-menu-mode-font-lock-keywords
   (purecopy
    (list
     '("^ MR Buffer.*"                 . font-lock-preprocessor-face) ;hdr 1
	'("^ -- ------.*"              . font-lock-preprocessor-face) ;hdr 2
	'("^\\(....Man: .*\\)"         1 font-lock-variable-name-face t) ;Manpg (new)
	'("^[. ][*][^%].[^*].*"        . font-lock-comment-face)	;Mod x temp
	'("^....[*]Buffer List[*].*"   . font-lock-doc-string-face) ;Buffer list
	'("^\\(....[*]shell.*\\)"      1 font-lock-reference-face t) ;shell buff
	'("^....[*].*"                 . font-lock-string-face) ;Temp buffer
	'("^....[+].*"                 . font-lock-keyword-face) ;Mail buffer
	'("^....[A-Za-z0-9/]*[-][+].*" . font-lock-keyword-face) ;Mail buffer
	'(".*Dired.*"                  . font-lock-function-name-face)
	'("^.*.java$"                  . ebuf-java-file)
	'("^.*.rb$"                    . ebuf-ruby-file)
	'("^.*.xml$"                   . ebuf-xml-file)
	'("^.*.build.*.xml$"           . ebuf-ant-build-file)
	'("^.*.text$"                  . ebuf-text-file)
	'("^.*.sh$"                    . ebuf-shell-file)
	'("^.*.el$"                    . ebuf-lisp-file)
	)))
 ; This hook run after buffer formatted, so it is necessary to re-fontify it...
 (add-hook 'electric-buffer-menu-mode-hook
	   '(lambda ()
	      (font-lock-mode 1)
	      (font-lock-fontify-buffer)))

(load "desktop")
;; no warnings for missing files:
(setq-default desktop-missing-file-warning nil)
(desktop-save-mode 1)

;;;; end of .emacs
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#040408" :foreground "#E8E3E3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
;;
;;* LOCAL VARIABLES
;; ============================
;; Local Variables:
;; mode: Emacs-Lisp
;; outline-regexp: ";;\\*+"
;; End:
;; ============================

(setq minibuffer-max-depth nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(font-lock-maximum-size 256000)
 '(load-home-init-file t t)
 '(show-paren-mode t))

(add-hook 'dired-load-hook
  (lambda ()
    (set-variable 'dired-use-ls-dired
      (and (string-match "gnu" system-configuration)
           ;; Only supported for XEmacs >= 21.5 and GNU Emacs >= 21.4 (I think)
           (if (featurep 'xemacs)
               (and
		(fboundp 'emacs-version>=)
		(emacs-version>= 21 5))
             (and (boundp 'emacs-major-version)
                  (boundp 'emacs-minor-version)
                  (or (> emacs-major-version 21)
                      (and (= emacs-major-version 21)
                           (>= emacs-minor-version 4)))))))))

(add-hook 'dired-load-hook
  (lambda ()
    (set-variable 'dired-use-ls-dired nil)))

;(set-face-font 'default  "-adobe-courier-medium-r-normal--12-*-*-*-*-*-*-*")
;(set-face-font 'modeline "-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1")

(add-to-list 'load-path "~/.emacs.d/lisp/vendor/yasnippet")
(setq yas/snippet-dirs (list "~/.emacs.d/lisp/yasnippet/snippets" "~/.emacs.d/lisp/vendor/yasnippet/snippets"))
(require 'yasnippet) ;; not yasnippet-bundle
(yas/global-mode 1)

(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))

;; (yas/initialize)

;; yasnippet complains if ~/.emacs.d/snippets does not exist

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(require 'ido)
(ido-mode 'both)

(load (system-name) 'noerror)