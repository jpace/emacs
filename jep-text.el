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

(provide 'text-functions)
;;; text-functions.el ends here
