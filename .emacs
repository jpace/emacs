;;* Jeff Pace's vainglorious .emacs file;;

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/vendor")

(setq jep:this-is-xemacs (not (null (string-match "XEmacs" (emacs-version)))))
(setq jep:this-is-gnuemacs (string-match "GNU Emacs" (emacs-version)))

(load (if jep:this-is-xemacs
	  "jep-xemacs"
	"jep-emacs"))

;; my keybindings override those unset in ergoemacs (cua), so it must load first:
(load "jep-cua")
(load "jep-keymap")
(load "jep-compilation")
(load "jep-tabs")
(load "jep-font")
(load "jep-file")
(load "jep-java")
(load "jep-c++")
(load "jep-c")
(load "jep-perl")
(load "jep-ruby")
(load "jep-debug")
(load "jep-ccmode")
(load "jep-text")
(load "jep-ibuffer")
(load "jep-modeline")
(load "jep-buffer")
(load "jep-xml")
(load "jep-html")

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

		(""		. text-mode) ; default -- this must be last.
		)))

(load "jep-groovy")
(load "jep-electric-buffer")
(load "jep-search")
(load "jep-nav")
(load "jep-desktop")

(setq minibuffer-max-depth nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(load-home-init-file t t))

(load "jep-dired")
(load "jep-snippet")

(require 'ido)
(ido-mode 'both)
(setq confirm-nonexistent-file-or-buffer nil)

(load (system-name) 'noerror)
(load (concat system-name "-" user-real-login-name) 'noerror)

;; not working with Emacs 24.x:
;; (load "color-theme")			; in living color!
;; (load "color-theme-soren")		; my theme
;; (color-theme-soren)

(if window-system
    (custom-set-faces
     '(default ((t (:inherit nil :stipple nil :background "#040412" :foreground "#E8E3E3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))))

;;
;;* LOCAL VARIABLES
;; ============================
;; Local Variables:
;; mode: Emacs-Lisp
;; outline-regexp: ";;\\*+"
;; End:
;; ============================

