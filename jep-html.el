;;; jep-html.el --- html configuration

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

(add-hook 'html-mode-hook
	  (lambda ()
	    (auto-fill-mode -1)
	    (setq indent-tabs-mode nil)))

;; Just to keep from getting prompted every time I invoke HTML:
(setq html-helper-address-string "<a href=\"mailto:jpace *at* incava *.* org\">Jeffrey E. Pace</a>")

(add-to-list 'auto-mode-alist '("\.html?$" . html-mode))
(add-to-list 'auto-mode-alist '("\.dsp$" . html-mode))
(add-to-list 'auto-mode-alist '("\.php$" . html-mode))

(provide 'jep-html)
;;; jep-html.el ends here
