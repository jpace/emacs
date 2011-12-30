;;; debug-statements.el --- Code for inserting debugging statements into files.

;; Copyright (C) 2011 by Free Software Foundation, Inc.

;; Author: Jeff Pace <jeugenepace at gmail dot com>

;;; Commentary:

;; 

;;; Code:
;;; ----------------------------------------------------------------------

(defvar jep:debug-stmt-format nil
  "Debug statement format associated with each buffer.")

(make-variable-buffer-local 'jep:debug-stmt-format)

(defun jep:debug-stmt-get-format ()
  "*Returns the statement format for the current file/buffer."
  (interactive)
  (or jep:debug-stmt-format
      (let ((ext (file-name-extension (buffer-file-name))))
	(cond ((equal ext "java") "tr.Ace.log(\"%s\", %s);")
	      ((equal ext "rb")   "puts \"%s: #{%s}\"")
	      (t (error (format "Extension not handled: %s" ext)))))))

(defun jep:debug-stmt-get-format-msg ()
  "*Returns the statement format for the current file/buffer."
  (interactive)
  (message "format: %s" (jep:debug-stmt-get-format)))

(defun jep:debug-stmt-get-format-debug ()
  "*Returns the statement format for the current file/buffer."
  (interactive)
  (let ((ext (file-name-extension (buffer-name))))
    (message "format: %s" jep:debug-stmt-format)))

(defun jep:debug-stmt-insert ()
  "*Inserts a debug statement for the current variable.
"
  (interactive)
  (re-search-backward "[^A-Za-z0-9:\\-_]")
  (forward-char 1)
  (let ((start (point))
	(startcol (current-column))
	(stmt  (jep:debug-stmt-get-format)))
   (re-search-forward "[^A-Za-z0-9:\\-_]")
    (backward-char 1)
    (kill-ring-save start (point))

    (c-end-of-statement)
    (end-of-line)
    (insert "\n")
    (indent-for-tab-command)
    (setq 
     var       (current-kill 0)
     formatted (format stmt var var))
    (insert formatted)
    (next-line 1)
    (move-to-column startcol)
    (message "formatted: %s" formatted)))

(defun jep:debug-stmt-set (stmt)
  "*Sets the statement/format for the current buffer.
"
  (interactive "sStatement: ")
  (message "statement: %s" stmt)
  (setq jep:debug-stmt-format stmt))

(provide 'debug-statements)

;;; debug-statements.el ends here
