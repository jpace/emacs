;;; tabs-config.el --- tabs configuration

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

(setq
 indent-tabs-mode              nil	; not using tabs
 default-tab-width             8	; tab == 8 spaces
 tab-width                     8	; ditto
 )

;; Set tab width explicitly for text mode
(add-hook 'text-mode-hook
	  '(lambda()
	     (setq tab-width 8
		   indent-tabs-mode t
		   )))

;;
;;** jep:toggle-tab-width
(defun jep:toggle-tab-width ()
  (interactive)
  (if (= tab-width 4)
      (setq tab-width 8)
      (setq tab-width 4)
      ))

(defun eliminate-all-tabs ()
  "Convert all tabs to spaces in the entire buffer."
  (interactive "*")
  (untabify (point-min) (point-max))
  (message "TABS converted to SPACES"))

(provide 'tabs-config)
;;; tabs-config.el ends here
