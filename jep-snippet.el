;;; jep-snippet.el --- yasnippet configuration

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

(add-to-list 'load-path "~/.emacs.d/lisp/vendor/yasnippet")
(setq yas/snippet-dirs (list "~/.emacs.d/lisp/yasnippet/snippets" "~/.emacs.d/lisp/vendor/yasnippet/snippets"))
(require 'yasnippet) ;; not yasnippet-bundle
(yas/global-mode 1)

(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

(provide 'jep-snippet)
;;; jep-snippet.el ends here