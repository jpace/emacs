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

;; my keybindings override those unset in ergoemacs (cua), so it must load first:
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

;; my keybindings override those unset in ergoemacs (cua), so it has to load afterward.
(load "keys")

(load "modeline-config")		; my modeline

;; not working with Emacs 24.x:
;; (load "color-theme")			; in living color!
;; (load "color-theme-soren")		; my theme
;; (color-theme-soren)

;;
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

;; these are normally disabled
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'eval-expression  'disabled nil)
(put 'narrow-to-region 'disabled nil)

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

;;
;;** jep:insert-time-stamp
;;============================================================
(defun jep:insert-time-stamp ()
  (interactive)
  (insert (time-stamp-string)))
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
