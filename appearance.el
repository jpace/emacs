;;; appearance.el --- Configuration for appearance

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

(setq visible-bell                  nil
      inhibit-startup-message       t)

(show-paren-mode t)

;; not working with Emacs 24.x:
;; (load "color-theme")			; in living color!
;; (load "color-theme-soren")		; my theme
;; (color-theme-soren)

(tool-bar-mode -1)

(defun jep:reset-mode-line ()
  "Resets the mode line to the original colors."
  (interactive)
  (set-face-attribute 'mode-line nil
		    :background "gray75"
		    :foreground "black"))

(jep:reset-mode-line)

(provide 'appearance)
;;; appearance.el ends here
