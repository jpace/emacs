;;* Jeff Pace's vainglorious .emacs file;;

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/vendor")

;;
;;* INITIALIZATION
;;** Emacs Version
(setq jep:this-is-xemacs (not (null (string-match "XEmacs" (emacs-version)))))
(setq jep:this-is-gnuemacs (string-match "GNU Emacs" (emacs-version)))

(if jep:this-is-xemacs
    (load "xemacs-config")
  (load "emacs-config"))

(load "cua-config")
(load "compilation-config")

(load "tabs-config")
(load "font-config")

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

;; my keybindings override those unset in ergoemacs, so it has to load afterward.
(load "keys")

(load "modeline-config")		; my modeline

;; (load "color-theme")			; in living color!
;; (load "color-theme-soren")		; my theme
;; (color-theme-soren)

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

;; these are normally disabled
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'eval-expression  'disabled nil)
(put 'narrow-to-region 'disabled nil)

(cond (jep:this-is-gnuemacs
       (shell)
       (process-kill-without-query (get-process "shell"))))

(load "xml-config")
(load "html-config")

;;$$$ todo: add erb-mode, inheriting text-mode, without auto-fill

;; Filename extensions for modes
(setq auto-mode-alist
      (append auto-mode-alist
	      '(
		;; TeX
		("\\.tex$"	. latex-mode)
		("\\.sty$"	. latex-mode) ; TeX style files

		("[Mm]akefile"  . makefile-mode)
		("\\.mak$"      . makefile-mode)
		("\\.mk$"       . makefile-mode)
		("\\.gmk$"      . makefile-mode) ; Suffix in Sun files for GNU makefiles

		("\\.awk$"      . awk-mode)
		
		("\\.tcl$"      . tcl-mode)

		("\\.py$"       . python-mode)

		("\\.bat$"      . sh-mode)
		("\\.cmd$"      . sh-mode)
		("\\.ini$"      . sh-mode)
		("\\.sh$"       . sh-mode)
		("rc$"          . sh-mode)
		("^dot\\."      . sh-mode)
		("^dot\\."      . sh-mode)

		("\\.sql"       . sql-mode)  ; SQL

		("\\.quotes$"   . quip-mode) ; quip

		(""		. text-mode) ; default
		)))

(load "groovy-config")

(load "electric-buffer-config")

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
  (insert (time-stamp-string)))
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

(load "nav-config")

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

(load "desktop-config")

(global-auto-revert-mode 1)

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
 '(cua-mode t nil (cua-base))
 '(display-time-mode t)
 '(load-home-init-file t t)
 '(show-paren-mode t))

(load "dired-config")
(load "snippet-config")

(require 'ido)
(ido-mode 'both)
(setq confirm-nonexistent-file-or-buffer nil)

(load (system-name) 'noerror)

(if window-system
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:inherit nil :stipple nil :background "#040408" :foreground "#E8E3E3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))))
