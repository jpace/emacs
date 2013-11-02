;;; jep-electric-buffer.el --- electric buffer config

;; Copyright (C) 2013  Jeff

;; Author: Jeff <jpace@eddie>
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

(provide 'jep-electric-buffer)
;;; jep-electric-buffer.el ends here
