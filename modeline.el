;;; modeline.el --- Controls the mode line.

;; Copyright (C) 2007 by incava.org.

;; Author: Jeff Pace <jeugenepace at gmail dot com>

;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html

(setq mode-line-system-identification  
  (substring (system-name) 0
             (string-match "\\..+" (system-name))))

(setq default-mode-line-format
      (list ""
            'mode-line-modified
            "%25b--"
	    (system-name)
	    ":"
            " ["
            'default-directory
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

;; Start with new default.
(setq mode-line-format default-mode-line-format)

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

(provide 'modeline)

;;; modeline.el ends here
