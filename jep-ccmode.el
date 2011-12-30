;;; ccmode_settings -- settings for C, C++, Objective-C, CORBA IDL, and Java
;;; files, for usage with CC Mode.
;;; author: jeugenepace at gmail dot com

(defconst jep-c-style
  '((c-basic-offset                    . 4)
    (c-recognize-knr-p                 . t)
    (c-hanging-comment-ender-p         . nil)
    (c-tab-always-indent               . t)
    (c-comment-only-line-offset        . (0 . 0))
    (c-indent-comments-syntactically-p . t)
    (c-hanging-braces-alist            . ((substatement-open after)
					  (brace-list-open)))
    (c-hanging-colons-alist            . ((member-init-intro before)
					  (inher-intro)
					  (case-label after)
					  (label after)
					  (access-label after)))
    (c-cleanup-list                    . (scope-operator
					  empty-defun-braces
					  defun-close-semi))
    (c-offsets-alist                   . ((arglist-close     . c-lineup-arglist)
					  (substatement-open . 0)
                                          (inline-open       . 0)
					  (case-label        . +)
					  (block-open        . 0)
					  (knr-argdecl-intro . 5) ; not that I ever do K&R args any more
					  ))
    (c-echo-syntactic-information-p . t) ; Do I care what it thinks?
    )
  "JPace C Programming Style")

(defun jep-c-mode-common-hook ()
  ;; Add my personal style and set it for the current buffer.  Give it a nice
  ;; egotistical name instead of something nonoffensive.
  (c-add-style "JPACE" jep-c-style t)
  ;; Offset customizations not in jep-c-style
  (c-set-offset 'stream-op 'c-lineup-streamop) ; for << alignment with cout, cerr
  (c-set-offset 'member-init-intro '++)	; huh?

  (setq tab-width        8		; 8 spaces for every tab
	indent-tabs-mode nil		; use spaces instead of tabs:
	paragraph-separate "[ 	\f]*$"	; CC mode mucks this up to actual blank lines.
	)

;  (c-toggle-auto-hungry-state 0)
  (c-toggle-auto-state   -1)		; turn on auto-newlining until I get sick of it again
  (c-toggle-hungry-state 1)		; it's Hungry Jack hungry
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  )
(add-hook 'c-mode-common-hook 'jep-c-mode-common-hook)

;;;; end of jep-ccmode.el
