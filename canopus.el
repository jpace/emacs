;;; canopus.el --- configuration for home machine

;; Copyright (C) 2013  Jeff Pace

;; Author: Jeff Pace <jpace@canopus>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(setq ibuffer-saved-filter-groups
      (quote (("default"      
	       ;; ("dired" (mode . dired-mode))
	       ;; ("Org" ;; all org-related buffers
	       ;;  (mode . org-mode))  
	       ;; ("Mail"
	       ;;   (or  ;; mail-related buffers
	       ;;    (mode . message-mode)
	       ;;    (mode . mail-mode)
	       ;;    ;; etc.; all your mail related modes
	       ;;    ))
	       ("asciitable"
		(filename . "org/incava/asciitable"))
	       ("bin"
		(filename . "home/jpace/bin"))
	       ("blog"
		(filename . "org/incava/blog"))
	       ("diffj"
		(filename . "org/incava/diffj"))
	       ("doctorj"
		(filename . "org/incava/doctorj"))
	       ("emacs"
		(or
		 (filename . ".emacs.d/lisp")
		 (filename . "System/Emacs")))
	       ("glark"
		(filename . "org/incava/glark"))
	       ("ijdk"
		(filename . "org/incava/ijdkproj"))
	       ("jagol"
		(filename . "org/incava/jagolproj"))
	       ("jdk"
		(filename . "com/sun/java"))
	       ("logue"
		(filename . "org/incava/logue"))
	       ("pvn"
		(filename . "org/incava/pvn"))
	       ("ragol"
		(filename . "org/incava/ragol"))
	       ("rails"
		(filename . "org/incava/rails"))
	       ("riel"
		(filename . "org/incava/riel"))
	       ("svnx"
		(filename . "org/incava/svnx"))
	       ("synoption"
		(filename . "org/incava/synoption"))
	       ("zsh"
		(filename . "System/Zsh"))
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

;; scala
(add-to-list 'load-path "~/.emacs.d/lisp/scala-mode")
(add-to-list 'load-path "~/.emacs.d/lisp/ensime/elisp")

(require 'scala-mode-auto)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'canopus)
;;; canopus.el ends here
