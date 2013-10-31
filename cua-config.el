;;; cua-config.el --- cua-config

;; Copyright (C) 2013  Jeff Pace

;; Author: Jeff <jpace@incava.org>
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

;;; Code:

;; there are some useful functions in ergoemacs, but I don't use all of the
;; keybindings
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us")
(load "ergoemacs-keybindings-5.3.9/ergoemacs-mode")
;; (ergoemacs-mode nil)
(cua-mode)				; ctrl-C (copy), -X (cut), -V (paste), -Z (undo)
;; but not ctrl-O (open) or ctrl-S (save)

(provide 'cua-config)
;;; cua.el ends here
