;;; jep-groovy.el --- Configuration for Groovy

;; Copyright (C) 2013  Jeff Pace

;; Author: Jeff Pace <jeugenepace@gmail.com>
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

;;; turn on syntax highlighting
(global-font-lock-mode 1)

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(add-to-list 'load-path "~/.emacs.d/lisp/vendor/groovy-mode")
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(defcustom groovy-electric-expand-delimiters-list '(?\$)
  "*List of contexts where matching delimiter should be inserted.
The word 'all' will do all insertions."
  :type '(set :extra-offset 8
	      (const :tag "Everything" all )
	      (const :tag "Curly brace" ?\{ )
	      (const :tag "Square brace" ?\[ )
	      (const :tag "Round brace" ?\( )
	      (const :tag "Quote" ?\' )
	      (const :tag "Double quote" ?\" )
	      (const :tag "Dollar in GStrings" ?\$ ))
  :group 'groovy-electric)

(defun jep:groovy-println-current-word ()
  "Inserts a line that writes the word to standard output."
  (interactive)
  (let ((word (current-word)))
    (move-end-of-line nil)
    (newline)
    (c-indent-line-or-region)
    (insert "println \"" word "\: ${" word "}\"")
    (c-indent-line-or-region)))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-j C-l") 'jep:groovy-println-current-word)))

(provide 'jep-groovy)
;;; jep-groovy.el ends here
