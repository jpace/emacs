;;; jep-buffer.el --- Configuration for buffers

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

;; This allows c-tab and c-s-tab switching between buffers, cycling around
;; Author: Igor Boukanov <boukanov@fi.uib.no>
(require 'pc-bufsw)
(cond (jep:this-is-gnuemacs
       (pc-bufsw::bind-keys 
	[C-tab]				; cycle to previous in buffer list
	[C-S-iso-lefttab]		; cycle to next in buffer list
	))
      (jep:this-is-xemacs
       (pc-bufsw::bind-keys 
	[(control tab)]
	[(control shift iso-lefttab)]
	)
       (turn-on-font-lock)
       ))

(setq confirm-nonexistent-file-or-buffer nil)

(define-key jep:keymap "R" 'revert-buffer)

(define-key jep:keymap [(tab)] 'jep:reindent-buffer)
(define-key jep:keymap "\t" 'jep:reindent-buffer)

(provide 'jep-buffer)
;;; jep-buffer.el ends here
