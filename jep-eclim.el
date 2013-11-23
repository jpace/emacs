;;; jep-eclim.el --- Configuration for Eclim

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

(add-to-list 'load-path "~/.emacs.d/lisp/vendor/emacs-eclim")
(add-to-list 'load-path "~/.emacs.d/lisp/vendor/auto-complete")
(add-to-list 'load-path "~/.emacs.d/lisp/vendor/auto-complete/lib")
(add-to-list 'load-path "~/.emacs.d/lisp/vendor/auto-complete/lib/popup")

(load "tkj-java")

(provide 'jep-eclim)
;;; jep-eclim.el ends here
