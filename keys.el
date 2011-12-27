;;;; keys.el -- global key bindings

;; meta and control keys
(global-set-key "\M-="     'goto-line)
(global-set-key "\C-l"     'electric-buffer-list)
(global-set-key "\e#"      'query-replace-regexp)
(global-set-key "\M-*"     'query-replace-regexp)
(global-set-key "\M-\C-f"  'font-lock-fontify-buffer)
(global-set-key "\C-c#"    'comment-region)

(global-set-key "\e!"      'jep:c++-get-companion-file)
(global-set-key "\e`"      'jep:c++-get-companion-file-nocreate)
(global-set-key "\e^"      'jep:c++-get-test-file)

(global-set-key "\M-w"     'clipboard-kill-ring-save)
(global-set-key "\C-w"     'clipboard-kill-region)

(global-set-key "\M-m"     'capitalize-word) ; 
(global-set-key "\M-M"     'upcase-word)
(global-set-key "\M->"     'forward-sentence)
(global-set-key "\M-<"     'backward-sentence)
(global-set-key "\M-,"     'downcase-word)

;; Not relying on the function keys as much, since they're tending to collide
;; with various window managers.

;; (read-kbd-macro "ESC q <C-down>"))
(global-set-key [f3]       'jep:fill-and-forward)
(global-set-key [(control f3)]  'jep:reindent-buffer)
(global-set-key [f4]       'call-last-kbd-macro)
(global-set-key [f5]       'compile)
(global-set-key [f6]       'previous-error)
(global-set-key [f7]       'next-error)

(global-set-key [f8]           'jep:space-indent-and-forward)
(global-set-key [(control f8)] 'jep:space-indent)

(global-set-key [home]     'beginning-of-line)
(global-set-key [end]      'end-of-line)

(global-set-key [(control home)]  'beginning-of-buffer)
(global-set-key [(control end)]   'end-of-buffer)

(global-set-key [(meta left)]     'beginning-of-line)
(global-set-key [(meta right)]    'end-of-line)

;; prior and next mean page up and page down
(global-set-key [(control prior)] 'enlarge-window)
(global-set-key [(control next)]  'shrink-window)

(global-set-key [(meta up)]       'jep:move-up)
(global-set-key [(meta down)]     'jep:move-down)

(define-key global-map [(control backspace)] 'undo)
(define-key global-map [(control =)]         'goto-line)

(global-set-key [(control delete)]   'backward-kill-word)
(global-set-key [(meta delete)]      'kill-word)

(global-set-key [(control down)]   'forward-paragraph)
(global-set-key [(control up)]     'backward-paragraph)

(defvar jep:keymap (make-sparse-keymap)
  "Keymap for all personal key bindings")

;; For ease, both ctrl-y and alt-y use this keymap.

(define-key global-map "\M-y" jep:keymap)
(define-key global-map "\C-y" jep:keymap)

(define-key jep:keymap "2"    'make-frame)
(define-key jep:keymap "O"    'jep:toggle-read-only)
(define-key jep:keymap "R"    'revert-buffer)

(define-key jep:keymap "\C-a" 'iso-accents-mode)
(define-key jep:keymap "\M-b" 'jep:point-to-bottom)
(define-key jep:keymap "\M-c" 'compare-windows)
(define-key jep:keymap "\C-f" 're-search-forward)
(define-key jep:keymap "\C-k" 'compile)
(define-key jep:keymap "\C-l" 'jep:find-file-from-list)
(define-key jep:keymap "\C-q" 'query-replace-regexp)
(define-key jep:keymap "\C-r" 'replace-regexp)
(define-key jep:keymap "\C-t" 'jep:debug-stmt-insert)
(define-key jep:keymap "\C-^" 'enlarge-window)

(define-key jep:keymap "b"    're-search-backward)
(define-key jep:keymap "c"    'comment-region)
(define-key jep:keymap "d"    'ediff-buffers)
(define-key jep:keymap "f"    'igrep-find)
(define-key jep:keymap "g"    'igrep)
(define-key jep:keymap "i"    'jep:java-insert-print-intro-line)
(define-key jep:keymap "5"    'jep:java-insert-print-break-line)
(define-key jep:keymap "\C-v" 'jep:java-toggle-variable-and-constant)

(define-key jep:keymap "k"    'delete-frame)
(define-key jep:keymap "l"    'list-matching-lines)
(define-key jep:keymap "o"    'jep:other-frame)
(define-key jep:keymap "q"    'query-replace)
(define-key jep:keymap "r"    'replace-string)
(define-key jep:keymap "s"    'sort-lines)
(define-key jep:keymap "t"    'jep:java-toggle-between-test-and-source)
(define-key jep:keymap "u"    'jep:upcase-char)

(define-key jep:keymap "<"    'isearch-backward-regexp)
(define-key jep:keymap ">"    'isearch-forward-regexp)

(define-key jep:keymap "\M-f" 'auto-fill-mode)

(define-key jep:keymap "\M-g" 'jep:c++-insert-include-self-guards)
(define-key jep:keymap "\M-h" 'jep:c++-insert-include-system-with-h)
(define-key jep:keymap "\M-i" 'jep:c++-insert-include-project)
(define-key jep:keymap "\M-s" 'jep:c++-insert-include-system-without-h)

(define-key jep:keymap [(control tab)]  'jep:toggle-tab-width)
(define-key jep:keymap [(meta tab)]     'jep:toggle-tab-width)
(define-key jep:keymap [(tab)]          'jep:toggle-tab-width)

(define-key jep:keymap "B"    'jep:file-insert-basename)

;;;; end of keys.el
