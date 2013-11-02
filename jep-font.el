;;; jep-font.el --- font configuration

;; Copyright (C) 2013  Jeff

;; Author: Jeff <jpace@eddie>
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

;; 

;;; Code:

;;** font-lock
;; Font-locking faces set-up
;; Switch on font-lock for every mode which supports it.
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))

;; there seems to be inconsistency with the way colors are set in version 20,
;; which is what the following is for:
(cond ((fboundp 'global-font-lock-mode)
       ;; Customize face attributes
       (set-face-foreground 'font-lock-comment-face       "pink1")
       ;;(set-face-background 'font-lock-comment-face       "LightGray")
       
       ;; Load the font-lock package.
       (require 'font-lock)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)))

(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size
      ;; I'm using files that are too big, apparently
      (if font-lock-maximum-decoration (* 70 2048) (* 150 2048)))

(provide 'jep-font)
;;; jep-font.el ends here
