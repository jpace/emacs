;;* Jeff Pace's vainglorious .emacs file;;

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/vendor")

(setq jep:this-is-xemacs (not (null (string-match "XEmacs" (emacs-version)))))
(setq jep:this-is-gnuemacs (string-match "GNU Emacs" (emacs-version)))

(load (if jep:this-is-xemacs
	  "jep-xemacs"
	"jep-emacs"))

(require 'str)
(require 'appearance)

;; my keybindings override those unset in ergoemacs (cua), so it must load first:
(require 'jep-cua)

(require 'jep-keymap)
(require 'jep-compilation)

(require 'jep-tabs)
(require 'jep-font)
(require 'jep-file)

(require 'jep-java)

(require 'jep-c++)
(require 'jep-c)
(require 'jep-perl)
(require 'jep-ruby)
(require 'jep-debug)
(require 'jep-ccmode)
(require 'jep-text)
(require 'jep-ibuffer)
(require 'jep-modeline)
(require 'jep-buffer)
(require 'jep-xml)
(require 'jep-html)
(require 'jep-sh)

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
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(display-time-mode t)
 '(load-home-init-file t t)
 '(show-paren-mode t))

(load "jep-dired")
(load "jep-snippet")

(require 'ido)
(ido-mode 'both)

(load (system-name) 'noerror)
(load (concat system-name "-" user-real-login-name) 'noerror)
(load (user-real-login-name) 'noerror)

(if window-system
    (custom-set-faces
     '(default ((t (:inherit nil :stipple nil :background "#040412" :foreground "#E8E3E3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))))

;;
;;* LOCAL VARIABLES
;; ============================
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#040404" :foreground "#E8E3E3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
;; Local Variables:
;; mode: Emacs-Lisp
;; outline-regexp: ";;\\*+"
;; End:
;; ============================

(set-cursor-color "#fafa33")
