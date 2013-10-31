;;; nav-config.el --- navigation configuration

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
 scroll-step                   1	; scroll 1 line at a time
 next-screen-context-lines     3	; Overlap between window-fulls when
					; scrolling by pages.
 mouse-avoidance-mode          'banish	; move mouse ptr out while typing.
 next-line-add-newlines        nil	; Disables down-arrow and C-n at the end
					; of a buffer from adding a new line to
					; that buffer.
 )

;;
;;** jep:point-to-top
(defun jep:point-to-top ()
  (interactive)
  (recenter 0))
;;
;;** jep:point-to-bottom
(defun jep:point-to-bottom ()
  (interactive)
  (recenter -1))
;;
;;** jep:move-up
(defun jep:move-up ()
  (interactive)
  (scroll-up 1))
;;
;;** jep:move-up
(defun jep:move-up ()
  (interactive)
  (scroll-up 1))
;;
;;** jep:move-down
(defun jep:move-down ()
  (interactive)
  (scroll-down 1))

(provide 'nav-config)
;;; nav-config.el ends here
