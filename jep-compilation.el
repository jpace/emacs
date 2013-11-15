;;; jep-compilation.el --- configuration for compilation

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

(setq special-display-buffer-names	; treat the compilation buffer as special
      (append `("*compilation*")))

;; This fontifies the compilation buffer when compilation exits.
(defun my-compilation-finish-function (buf msg)
  "This is called after the compilation exits.  Currently just
highlights the compilation messages."
  (save-excursion
    (set-buffer buf)
    (font-lock-fontify-buffer)))
(setq compilation-finish-function 'my-compilation-finish-function)

(global-set-key [f5] 'compile)
(global-set-key [f6] 'previous-error)
(global-set-key [f7] 'next-error)

(define-key jep:keymap (kbd "C-k") 'compile)

(provide 'jep-compilation)
;;; jep-compilation.el ends here
