;;; text-functions.el --- extensions for processing text files

;; Copyright (C) 2011  Jeffrey E. Pace

;; Author: Jeffrey E. Pace <jpace@incava.org>
;; Keywords: files, wp

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

;; fills the current paragraph and moves down

(defun jep:fill-and-forward ()
  (interactive)
  (fill-paragraph nil)
  (forward-paragraph))			; used to be 'forward-block-of-lines'

(defun jep:reindent-buffer ()
  "*Reindent each paragraph"
  (interactive)
  (while (not (eobp))
    (jep:fill-and-forward)))

(defun previous-window (arg)
  "Switch to previous window."
  (interactive "p")
  (other-window (- arg)))

(defun jep:space-indent ()
  (interactive)
  (beginning-of-line-text)
  (insert "    ")
  (end-of-line)
  (forward-char)
  )

(defun jep:space-indent-and-forward ()
  (interactive)
  (beginning-of-line-text)
  (insert "    ")
  )

(defun jep:match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun jep:upcase-char ()
  "Capitalizes the current character and moves right."

  (interactive)
  (let ((start (point)))
    (forward-char 1)
    (upcase-region start (point))))

(defun jep:text-camel-to-snake-case (var)
  "*Converts the variable from camelCase to snake_case"
  
  (let ((case-replace t)
	(case-fold-search nil))
    (downcase (replace-regexp-in-string "\\(\\(?:[A-Z]\\|[0-9]+\\)\\)" "_\\1" var t nil))))

(defun jep:text-snake-to-camel-case (const)
  "*Converts the variable from snake_case to camelCase"
  
  (let ((case-replace t)
	(case-fold-search nil))
    (replace-regexp-in-string "_\\([a-z0-9]\\)" "\\U\\1" (downcase const) t nil)))

(defun jep:text-toggle-camel-and-snake-case ()
  "*Toggles between camelCase and snake_case.
"
  (interactive)
  (re-search-backward "[^A-Za-z0-9_]")
  (forward-char 1)
  (let ((start (point))
	(case-fold-search nil))
    (re-search-forward "[^A-Za-z0-9_]")
    (backward-char 1)
    (kill-region start (point))
    (setq 
     var  (current-kill 0)
     repl (if (string-match "[A-Z]" var)
	      (jep:text-camel-to-snake-case var)
	    (jep:text-snake-to-camel-case var)))
    (insert repl)))

(provide 'text-functions)
;;; text-functions.el ends here
