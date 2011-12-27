(defun color-theme-soren ()
  "Color theme by Jeffrey E. Pace, created 2005-05-16.
Mostly fairly mid-saturation yet bright colors on a dark gray background."
  (interactive)
  (color-theme-install
   '(color-theme-soren
     ;; ((background-color . "#0b1016")	;180 degree hue
     
     ;; '(color-theme-soren
     ;; ((background-color . "#2F4F4F")	;180 degree hue
     ((background-color . "#040408")	;180 degree hue
      (background-mode . dark)
      (background-toolbar-color . "#2F4F4F")
      (border-color . "#000000")
      (border-color . "#000000")
      (bottom-toolbar-shadow-color . "#09131c")
      (cursor-color . "#E05252")	;0 degree
      (foreground-color . "#E8E3E3")	;0 degree
      (mouse-color . "yellow")
      (top-toolbar-shadow-color . "#132639"))

;     ((buffers-tab-face . buffers-tab)
;      (cperl-here-face . font-lock-string-face)
;      (cperl-pod-face . font-lock-comment-face)
;      (cperl-pod-head-face . font-lock-variable-name-face)
;      (vc-annotate-very-old-color . "#0046FF")
;      (vc-mode-face . highlight))
     (default ((t (nil))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#abc" :foreground "#123"))))
     (cperl-array-face ((t (:bold nil :foreground "#F7E6D4"))))
     (cperl-hash-face ((t (:bold nil :foreground "#FF9999"))))
     (cperl-nonoverridable-face ((t (nil))))
     (cursor ((t (:background "green" :foreground "black"))))
     (dired-face-boring ((t (:foreground "#dddddd"))))
     (dired-face-directory ((t (:foreground "#9bdddd"))))
     (dired-face-executable ((t (:foreground "#d1ca7d"))))
     (dired-face-flagged ((t (:background "#1a8484"))))
     (dired-face-marked ((t (:background "red" :foreground "deepskyblue"))))
     (dired-face-permissions ((t (:background "#432671" :foreground "#e8f4dc"))))
     (dired-face-setuid ((t (:foreground "#f26085"))))
     (dired-face-socket ((t (:foreground "#efbfcb"))))
     (dired-face-symlink ((t (:foreground "#aaa299"))))
     (font-lock-builtin-face ((t (:foreground "#D6E6F5"))))
     (font-lock-comment-face ((t (:foreground "#E3E3E8"))))
     (font-lock-constant-face ((t (:foreground "#D6F5F5"))))
     (font-lock-doc-string-face ((t (:foreground "#FFE6CC"))))
     (font-lock-function-name-face ((t (:foreground "#14B8B8"))))
     (font-lock-keyword-face ((t (:foreground "#E6B3B3"))))
     (font-lock-preprocessor-face ((t (:foreground "#C2B2A3"))))
     (font-lock-reference-face ((t (:foreground "#94E1E1")))) ;210

     (ebuf-java-file      ((t (:foreground "#f5f0db")))) ;cream and coffee
     (ebuf-ruby-file      ((t (:foreground "#b4cece"))))
     (ebuf-xml-file       ((t (:foreground "#80b2a6"))))
     (ebuf-ant-build-file ((t (:foreground "#c58bc5"))))
     (ebuf-text-file      ((t (:foreground "#66e0c2"))))
     (ebuf-shell-file     ((t (:foreground "#b9caff"))))
     (ebuf-lisp-file      ((t (:foreground "#b4c7a2"))))

     (font-lock-string-face ((t (:foreground "#F0CCA8")))) ;30 degree
     (font-lock-type-face ((t (nil))))
     (font-lock-variable-name-face ((t (:foreground "#ADEBEB"))))
     (font-lock-warning-face ((t (:foreground "#FAD1D1"))))
     (green ((t (:foreground "green"))))
     (gui-button-face ((t (:background "grey75" :foreground "black"))))
     (gui-element ((t (:background "Gray80" :foreground "black"))))
     (highlight ((t (:background "#75F0F0" :foreground "#053361"))))
     (html-helper-bold-face ((t (:bold t))))
     (html-helper-italic-face ((t (nil))))
     (html-helper-link-face ((t (:foreground "#5C99D6")))) ;210 degree
     (html-helper-significant-tag-face ((t (:foreground "#AD8585")))) ;30 degree
     (html-helper-strikethrough-face ((t (:strikethru t))))
     (html-helper-underline-face ((t (:underline t))))
     (hyper-apropos-documentation ((t (:foreground "white"))))
     (hyper-apropos-heading ((t (:bold t))))
     (hyper-apropos-hyperlink ((t (:foreground "sky blue"))))
     (hyper-apropos-major-heading ((t (:bold t))))
     (hyper-apropos-section-heading ((t (:bold t))))
     (hyper-apropos-warning ((t (:bold t :foreground "red"))))
     (isearch ((t (:background "#CCDD55" :foreground "#223300"))))
     (isearch-secondary ((t (:foreground "#EEFF77"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (list-mode-item-selected ((t (:background "gray68"))))
     (modeline ((t (:background "#DEEDED" :foreground "#333300"))))
     (modeline-inactive ((t (:background "#aaaaaa" :foreground "#dd0000"))))
     (modeline-buffer-id ((t (:foreground "#993333"))))
     (modeline-mousable ((t (:foreground "#8CD9D9"))))
     (modeline-mousable-minor-mode ((t (:foreground "#C99C9C"))))
     (p4-depot-added-face ((t (:foreground "blue"))))
     (p4-depot-deleted-face ((t (:foreground "red"))))
     (p4-depot-unmapped-face ((t (:foreground "grey30"))))
     (p4-diff-change-face ((t (:foreground "dark green"))))
     (p4-diff-del-face ((t (:foreground "red"))))
     (p4-diff-file-face ((t (:background "gray90"))))
     (p4-diff-head-face ((t (:background "gray95"))))
     (p4-diff-ins-face ((t (:foreground "blue"))))
     (paren-mismatch-face ((t (:background "DeepPink"))))
     (paren-no-match-face ((t (:background "yellow"))))
     (pointer ((t (:foreground "Blue"))))
     (primary-selection ((t (:bold nil :background "gray65" :foreground "#94253d"))))
     (red ((t (:foreground "red"))))
     (region ((t (:background "blue"))))
     (right-margin ((t (nil))))
     (scroll-bar ((t (:background "cyan" :foreground "red"))))
     (secondary-selection ((t (nil))))
     (toolbar ((t (:background "Gray80" :foreground "black"))))
     (underline ((t (:underline t))))
     (widget ((t (:background "Gray80" :foreground "black"))))
     (widget-button-face ((t (nil))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "lime green"))))
     (widget-field-face ((t (:background "dim gray"))))
     (widget-inactive-face ((t (:foreground "light gray"))))
     (widget-single-line-field-face ((t (:background "dim gray"))))
     (yellow ((t (:foreground "yellow"))))
     (zmacs-region ((t (:background "#C7D1D1" :foreground "#B81414")))))))
