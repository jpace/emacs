;;; jep-keymap.el --- Configuration for my keymap.

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

(defvar jep:keymap (make-sparse-keymap)
  "Keymap for all personal key bindings")

;; For ease, both ctrl-y and alt-y use this keymap.

(define-key global-map (kbd "M-j") jep:keymap)
(define-key global-map (kbd "C-j") jep:keymap)

;; CUA settings muck up ctrl-x, so use an alternative, alt-j alt-j:
(define-key jep:keymap (kbd "M-j") 'repeat-complex-command)

(provide 'jep-keymap)
;;; jep-keymap.el ends here
