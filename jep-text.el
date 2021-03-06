;;; jep-text.el --- extensions for processing text files

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

(defun jep:text-fill-and-forward ()
  "Fill the current paragraph and move to the next one."
  (interactive)
  (fill-paragraph nil)
  (forward-paragraph))

(defun jep:text-refill-buffer ()
  "*Reindent each paragraph."
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (while (not (eobp))
      (jep:text-fill-and-forward))))

(defun jep:text-space-indent ()
  (interactive)
  (beginning-of-line-text)
  (insert "    ")
  (end-of-line)
  (forward-char))

(defun jep:text-space-indent-and-forward ()
  (interactive)
  (beginning-of-line-text)
  (insert "    "))

(defun jep:match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun jep:text-upcase-char ()
  "Capitalizes the current character and moves right."
  (interactive)
  (let ((start (point)))
    (forward-char 1)
    (upcase-region start (point))))

(defun jep:text-downcase-char ()
  "Downcases the current character and moves right."
  (interactive)
  (let ((start (point)))
    (forward-char 1)
    (downcase-region start (point))))

(defun jep:text-camel-to-snake-case (var)
  "*Converts the variable from camelCase to snake_case"
  (let ((case-replace t)
	(case-fold-search nil))
    (downcase (replace-regexp-in-string "\\(\\(?:[A-Z]\\|[0-9]+\\)\\)" "_\\1" var t nil))))

(defun jep:text-snake-to-camel-case (const)
  "*Converts the variable from snake_case to camelCase"
  (let ((case-replace t)
	(case-fold-search nil))
    (replace-regexp-in-string "_" "" 
			      (replace-regexp-in-string "[^_]\\([a-z0-9]\\)" 'capitalize (downcase const) t nil))))

(defun jep:text-toggle-camel-and-snake-case ()
  "*Toggles between camelCase and snake_case."
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

;; from https://www.emacswiki.org/emacs/DeletingWhitespace:
(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

;; from https://www.emacswiki.org/emacs/ZapUpToChar
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
  you are deleting forward, the CHAR is replaced and the point is
  put before CHAR"
  (insert char)
  (if (< 0 arg) (forward-char -1)))

(defun jep:text-align-comma ()
  "Aligns the current region on commas."
  (interactive)
  (align-regexp (region-beginning) (region-end) ",\\(\\s-*\\)" 1 1 t))

(defun jep:text-align-equals ()
  "Aligns the current region on equals."
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=" 1 1 t))

(defun jep:text-align-comment ()
  "Aligns the current region on comment."
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)//" 1 1 t))

(defun jep:text-align-colon ()
  "Aligns the current region on colons."
  (interactive)
  (align-regexp (region-beginning) (region-end) ":\\(\\s-*\\)" 1 1 t))

(defun jep:text-align-before (arg)
  "Aligns the current region up to the argument."
  (interactive "sString: ")
  (align-regexp (region-beginning) (region-end) (concat arg "\\(\\s-*\\)") 1 1 t))

(defun jep:text-align-after (arg)
  "Aligns the current region starting with the argument."
  (interactive "sString: ")
  (align-regexp (region-beginning) (region-end) (concat "\\(\\s-*\\)" arg) 1 1 t))

(add-hook 'text-mode-hook 'turn-on-auto-fill nil)
(add-hook 'text-mode-hook 
	  (lambda () 
	    (local-set-key (kbd "M-I") 'jep:text-refill-buffer)
	    (local-set-key (kbd "M-j M-t") 'jep:text-fill-and-forward)
	    (set (make-local-variable 'require-final-newline) nil)))

(add-to-list 'auto-mode-alist '("\.txt$" . text-mode))

(define-key jep:keymap "\C-y" 'jep:text-toggle-camel-and-snake-case)
(define-key jep:keymap "\M-y" 'jep:text-toggle-camel-and-snake-case)
(define-key jep:keymap "s"    'sort-lines)

(global-set-key "\M-m"     'capitalize-word)
(global-set-key "\M-M"     'upcase-word)
(global-set-key "\M-U"     'jep:text-upcase-char)
(global-set-key "\M-L"     'jep:text-downcase-char)
(global-set-key "\M-/"     'hippie-expand)

(global-set-key [(control delete)]   'backward-kill-word)
(global-set-key [(meta delete)]      'kill-word)
(define-key global-map [(control backspace)] 'undo)

(define-key jep:keymap "c"    'comment-region)
(define-key jep:keymap "u"    'uncomment-region)
(define-key jep:keymap "\M-f" 'auto-fill-mode)
(define-key jep:keymap "\C-w" 'whack-whitespace)

(define-key jep:keymap "," 'jep:text-align-comma)
(define-key jep:keymap "=" 'jep:text-align-equals)
(define-key jep:keymap "/" 'jep:text-align-comment)
(define-key jep:keymap ":" 'jep:text-align-colon)

(provide 'jep-text)
;;; text-functions.el ends here
