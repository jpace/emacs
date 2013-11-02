;;; jep-nav.el --- navigation configuration

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

(setq
 scroll-step                   1	; scroll 1 line at a time
 next-screen-context-lines     3	; Overlap between window-fulls when
					; scrolling by pages.
 mouse-avoidance-mode          'banish	; move mouse ptr out while typing.
 next-line-add-newlines        nil	; Disables down-arrow and C-n at the end
					; of a buffer from adding a new line to
					; that buffer.
 )

(defun jep:point-to-top ()
  "Put the current position at the top of the displayed window"
  (interactive)
  (recenter 0))

(defun jep:point-to-bottom ()
  "Put the current position at the bottom of the displayed window"
  (interactive)
  (recenter -1))

(defun jep:scroll-up-one-line ()
  "Scroll up one line"
  (interactive)
  (scroll-up 1))

(defun jep:scroll-down-one-line ()
  "Scroll down one line"
  (interactive)
  (scroll-down 1))

(global-set-key "\M-=" 'goto-line)
(global-set-key "\M-g" 'goto-line)

(global-set-key [home]     'beginning-of-buffer)
(global-set-key [end]      'end-of-buffer)

;; Putty sends odd characters, so these map them to the same commands:
(global-set-key (kbd "ESC <home>")  'beginning-of-buffer)
(global-set-key (kbd "ESC <select>")  'end-of-buffer)
(global-set-key (kbd "<select>")  'end-of-buffer)

(global-set-key [(meta left)]     'beginning-of-line)
(global-set-key [(meta right)]    'end-of-line)

(global-set-key "\M->" 'forward-sentence)
(global-set-key "\M-<" 'backward-sentence)

(global-set-key [(meta up)]   'jep:scroll-up-one-line)
(global-set-key [(meta down)] 'jep:scroll-down-one-line)

(global-set-key [(control down)]   'forward-paragraph)
(global-set-key [(control up)]     'backward-paragraph)

(provide 'jep-nav)
;;; jep-nav.el ends here
