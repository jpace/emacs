;;; jep-emacs.el --- General configuration for Emacs

;; Copyright (C) 2013  Jeff Pace

;; Author: Jeff Pace <jpace@canopus>
;; Keywords: extensions

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

(setq
 enable-local-eval             t
 sentence-end-double-space     nil	; treat ". " as a sentence separator
 sentence-end                  "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*"
 auto-save-interval            2000	; save every 2000 operations
 auto-save-default             nil	; no autosaving
 gc-cons-threshold             500000	; garbage collection threshold
 default-fill-column           100
 default-major-node            'indented-text-mode
 apropos-do-all                t	; Thorough and slow.
 paragraph-separate            "[ 	\f]*$" ; CC mode mucks this up to actual blank lines (no chars)
 transient-mark-mode           t	; show regions as they are highlighted.
 find-file-compare-truenames   t	; Use the truename of the file, not the base name.
 )

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-enable-clipboard)

(setq auto-mode-alist
      (append auto-mode-alist
	      '(("\.emacs" . emacs-lisp-mode)
		("\.el"    . emacs-lisp-mode)
		("\.lisp$" . lisp-mode))))

;;** Confirm close
;; Add Emacs close confirmation, since ctrl-x ctrl-c is too easy to hit
;; accidentally
(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really kill Emacs? "))
	    kill-emacs-query-functions))

;; reload files when they change on disk (e.g., git checkout HEAD ...):
(global-auto-revert-mode 1)

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

;; these are normally disabled
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'eval-expression  'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defun restart-server ()
  "Stops and starts the server."
  (server-force-delete)
  (server-start))

(restart-server)

(provide 'jep-emacs)
;;; jep-emacs.el ends here
