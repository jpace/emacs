;;; jep-xemacs.el --- configuration for xemacs

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

(defun jep:toggle-menubar ()
  (interactive)
  (set-specifier menubar-visible-p
		 (not (specifier-instance menubar-visible-p))))
(defun jep:toggle-modeline ()
  (interactive)
  (set-specifier has-modeline-p
		 (not (specifier-instance has-modeline-p))))
(defun jep:toggle-toolbar ()
  (interactive)
  (set-specifier default-toolbar-visible-p
		 (not (specifier-instance default-toolbar-visible-p))))
(global-set-key [(meta f12)] 'jep:toggle-modeline)
(global-set-key [(meta f11)] 'jep:toggle-menubar)
(global-set-key [(meta f10)] 'jep:toggle-toolbar)
(global-set-key [(control button3)] 'popup-menubar-menu)
(set-specifier default-toolbar-visible-p nil)
(set-specifier menubar-visible-p nil)

(provide 'jep-xemacs)
;;; jep-xemacs.el ends here
