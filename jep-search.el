;;; jep-search.el --- Configuration for searching

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

(defun jep:toggle-case-fold-search ()
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (if case-fold-search
      (message "case sensitive search OFF")
    (message "case sensitive search ON")))

(define-key jep:keymap "\C-f" 're-search-forward)
(define-key jep:keymap "\C-q" 'query-replace-regexp)
(define-key jep:keymap "\C-r" 'replace-regexp)
(define-key jep:keymap "b"    're-search-backward)
(define-key jep:keymap "q"    'query-replace)
(define-key jep:keymap "r"    'replace-string)
(define-key jep:keymap "<"    'isearch-backward-regexp)
(define-key jep:keymap ">"    'isearch-forward-regexp)

(provide 'jep-search)
;;; jep-search.el ends here
