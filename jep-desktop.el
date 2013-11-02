;;; jep-desktop.el --- configuration for desktop

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

;;; Code:

(load "desktop")

;; no warnings for missing files:
(setq-default desktop-missing-file-warning nil)
(desktop-save-mode 1)

;;; This installs the `saveplace' package and defines where the places
;;; in visited files are saved between sessions.
(condition-case err
    (require 'saveplace)
  (error
   (message "Cannot save places %s" (cdr err))))
(setq-default save-place t)		; save places in all files
(setq save-place-file	 "~/.emacsloc")

(provide 'jep-desktop)
;;; jep-desktop.el ends here
