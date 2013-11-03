;;;; keys.el -- global key bindings

(global-set-key "\M-w"     'clipboard-kill-ring-save)
(global-set-key "\C-w"     'clipboard-kill-region)

(global-set-key "\M-V"     'yank-pop)

;; Not relying on the function keys as much, since they're tending to collide
;; with various window managers.

;; prior and next mean page up and page down
(global-set-key [(control prior)] 'enlarge-window)
(global-set-key [(control next)]  'shrink-window)

(defvar jep:keymap (make-sparse-keymap)
  "Keymap for all personal key bindings")

;; For ease, both ctrl-y and alt-y use this keymap.

(define-key global-map "\M-j" jep:keymap)
(define-key global-map "\C-j" jep:keymap)

(define-key jep:keymap "\M-c" 'compare-windows)
(define-key jep:keymap "\C-^" 'enlarge-window)

(define-key jep:keymap "k"    'delete-frame)
(define-key jep:keymap "o"    'jep:other-frame)

;;;; end of keys.el
