;;; upcase-char.el --- upcase character

;; Copyright (C) 2011  Jeffrey E. Pace

;; Author: Jeffrey E. Pace <jeugenepace at gmail dot com>
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

;; capitalizes the current character and moves right.
(defun jep:upcase-char ()
  (interactive)
  (let ((start (point)))
    (forward-char 1)
    (upcase-region start (point))))

(provide 'upcase-char)
;;; upcase-char.el ends here
