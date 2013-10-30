;;; dired-config.el --- configuration for dired

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

(add-hook 'dired-load-hook
  (lambda ()
    (set-variable 'dired-use-ls-dired
      (and (string-match "gnu" system-configuration)
           ;; Only supported for XEmacs >= 21.5 and GNU Emacs >= 21.4 (I think)
           (if (featurep 'xemacs)
               (and
		(fboundp 'emacs-version>=)
		(emacs-version>= 21 5))
             (and (boundp 'emacs-major-version)
                  (boundp 'emacs-minor-version)
                  (or (> emacs-major-version 21)
                      (and (= emacs-major-version 21)
                           (>= emacs-minor-version 4)))))))))

(add-hook 'dired-load-hook
  (lambda ()
    (set-variable 'dired-use-ls-dired nil)))

(provide 'dired-config)
;;; dired-config.el ends here
