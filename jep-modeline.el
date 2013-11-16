;;; jep-modeline.el --- Configuration for Modeline

;; Copyright (C) 2013  Jeff Pace

;; Author: Jeff <jeugenepace@gmail.com>
;; Keywords: extensions

;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html

(require 'str)

(setq mode-line-system-identification  
  (substring (system-name) 0
             (string-match "\\..+" (system-name))))

(defvar jep:modeline-subs
  '(("/home/jpace/" . "~/")
    (".*/Projects/com/softwareag/is/" . "~is/")
    ("/proj/org/incava/" . "~incava/")
    ("/$" . "")
    ))

(defun jep:modeline-dir-abbrev ()
  (str-replace-all default-directory jep:modeline-subs))

(setq default-mode-line-format
      (list ""
            'mode-line-modified
            "%25b--"
            " ["
            '(:eval (jep:modeline-dir-abbrev))
            "] "
            "%[(" 
            'mode-name 
            "%n" 
            'mode-line-process  
            ")%]--" 
             "L%l--"
             "C%c--"
            '(-3 . "%P")
            "-%-"))

(setq
 mode-line-format default-mode-line-format
 display-time-24hr-format t	; 24-hour time
 frame-title-format       "%b - Emacs" ; full name (dir + file)
 icon-title-format        "%f - Emacs" ; short name (file only)
 column-number-mode       t
 display-time-mode        t
 )

;;; modeline fiddling
;; (setq default-mode-line-formatdddd
;;       (setq-default
;;        mode-line-format
;;        (list
;;         ""
;;         "%C"
;;         (cons mode-line-modified-extent 'mode-line-modified)
;;         (cons mode-line-buffer-id-extent (list 'line-number-mode "L%l "))
;;         (cons mode-line-buffer-id-extent (list 'column-number-mode "C%c "))
;;         (cons mode-line-buffer-id-extent (cons -3 "%p"))
;;         " "
;;         (cons mode-line-minor-mode-extent " %70b ")
;;         ;; (cons mode-line-minor-mode-extent (cons -50 'mode-line-buffer-identification))
;; 	" ["
;;         'default-directory
;; 	"] "
;;         'global-mode-string
;;         "   %[("
;;         (cons mode-line-minor-mode-extent
;;               (list "" 'mode-name 'minor-mode-alist))
;;         (cons mode-line-narrowed-extent "%n")
;;         'mode-line-process
;;         ")%]----"
;;         "-%-")))

;; (line-number-mode 1)
;; (column-number-mode 1)

(provide 'jep-modeline)
;;; jep-modeline.el ends here
