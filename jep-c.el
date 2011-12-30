;;; C extensions
;;; author: jeugenepace at gmail dot com

(defun jep:c-comment-out ()
  "Comments out the currently highlighted region. Comment characters are for
the C family of languages (C, C++, Java)."
  (interactive "*")			; "*" == abort if buffer is read only
  
  ;; stick "/*" at the beginning of the current region, and "*/" at the end of
  ;; the current region. Do nothing if there's no current region.
  
  (let (min-pt max-pt))
  (setq min-pt (region-beginning))

  ;; +3 because of the added "/* " at the region beginning
  (setq max-pt (+ 3 (region-end)))
   
  (goto-char min-pt)
  (insert "/* ")

  (goto-char max-pt)
  (insert " */")
  )

;;; ----------------------------------------------------------------------
;;; keys settings

;; ESC-ALT-/ (for /* ... */)
(global-set-key "\e\M-/"  'jep:c-comment-out)

(setq auto-mode-alist
      (append '(
		;; C
		("\\.c$"	. c-mode)
		) auto-mode-alist))

(provide 'jep:c)
