;;; pc-bufsw.el -- Support for a quick switch between Emacs buffers.

;; Author: Igor Bukanov <boukanov@fi.uib.no>
;; Maintainer: Igor Bukanov
;; Version 1.2
;; Keywords: buffer

;;
;;  THIS SOFTWARE IS NOT COPYRIGHTED
;;
;;  This source code is offered for use in the public domain. You may
;;  use, modify or distribute it freely.
;;
;;  This code is distributed in the hope that it will be useful but
;;  WITHOUT ANY WARRANTY. ALL WARRANTIES, EXPRESS OR IMPLIED ARE HEREBY
;;  DISCLAMED. This includes but is not limited to warranties of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:
;; This is an implementation of quick switch for Emacs buffer that is similar 
;; to one that is widely used on PC. The main idea here is that a user chooses
;; two keys (most often C-tab and S-C-tab) that switch between buffers 
;; according to most or least recently order. After the final choice is made 
;; the last selected buffer becomes most recently used one. 

(provide 'pc-bufsw)

(defconst pc-bufsw::quite-time 3
  "Quite time to automaticaly terminate buffer switch mode. 
If there is no input during quite-time seconds makes the last choosen 
buffer current." ) 

; Variable to store data vector during buffers change.
; It has the following structure:
;   element at index 2*i - buffer to select  after i'th switch
;   element at index 2*i+1 - window that initially shows buffer 2*i or nil
; The vector may contain any buffer refference several times if that was shown
; initially in several windows. It is supposed that buffers in the vector are 
; odered according to most recently used oder.
(defvar pc-bufsw::walk-vector nil)

; Index of currently selected buffer in pc-bufsw::walk-vector. Always even.
(defvar pc-bufsw::cur-index 0)

; The initial buffer list. It is used to construct 
; pc-bufsw::walk-vector. When a user stops the selection, 
; the new buffer order much pc-bufsw::start-buf-list except the selected buffer 
; that is moved on the top.
(defvar pc-bufsw::start-buf-list nil)

(defun pc-bufsw::previous () 
  (interactive) 
  (pc-bufsw::walk 1))

(defun pc-bufsw::lru () 
  (interactive) 
  (pc-bufsw::walk -1))

;;;###autoload
(defun pc-bufsw::bind-keys (key1 key2)
  "Bind key1 and key2 to switch buffers in most or least recently used oder.
Pressing key1 or key2 would switch to most or least recently used buffer and
enter switch mode. In this mode subsequent pressing of key1 or key2 would go 
father in buffer list shown in echo area. 

Pressing any other key or no input during the period indicated 
by 'pc-bufsw::quite-time' variable closes the mode and makes the last selected buffer current. 
If newly selected buffer is shown in some window that would be used to show
the buffer. Otherwise it will be displayed in the initial window.

Typical usage in .emacs file:
(require 'pc-bufsw)
(pc-bufsw::bind-keys [C-tab] [C-S-tab])
ot if you use XEmacs, do it like
(as suggested by Friedrich Dominicus <Friedrich.Dominicus@inka.de>)
(require 'pc-bufsw)   
(pc-bufsw::bind-keys [(control tab)] [ (control shift tab) ])
"
  (global-set-key key1 'pc-bufsw::previous) 
  (global-set-key key2 'pc-bufsw::lru)) 


; Main loop. It does 4 things. First, select new buffer and/or windows 
; according to user input. Second, it selects the newly choosen buffer/windows/frame.
; Third, it draw in the echo area line with buffer names. Forth, it waits for a timeout
; to terminate the switching.
(defun pc-bufsw::walk (direction) 
  (catch 'cancel
    (if (null pc-bufsw::walk-vector) 
	(pc-bufsw::start))
    (pc-bufsw::choose-next-index direction)
    (pc-bufsw::choose-buf-and-wnd)
    (pc-bufsw::show-buffers-names)
    (if (sit-for pc-bufsw::quite-time) (pc-bufsw::finish))))

(defun pc-bufsw::start() 
  (setq pc-bufsw::start-buf-list (buffer-list))
  (setq pc-bufsw::cur-index 0)
  (setq pc-bufsw::walk-vector (pc-bufsw::get-walk-vector))
  (add-hook 'pre-command-hook 'pc-bufsw::switch-hook))

;; Hook to access next input from user.
(defun pc-bufsw::switch-hook () 
  (if (not (or (eq 'pc-bufsw::lru this-command)
	       (eq 'pc-bufsw::previous this-command)))
      (pc-bufsw::finish))) 

;; Construct main buffer/window vector. Throw 'cancel if the current buffer 
;  is minibuffer. 
(defun pc-bufsw::get-walk-vector()
  (let ((wnd (selected-window)))
    (if (window-minibuffer-p wnd) (throw 'cancel nil))
    (let* ((pair-list (pc-bufsw::get-walk-buffer-window-pair-list 
		       wnd pc-bufsw::start-buf-list))
	   (len (* 2 (1+ (length pair-list))))
	   (v (make-vector len nil)))
      (while pair-list
	(let ((pair (car pair-list)))
	  (setq len (1- len))
	  (aset v len (cdr pair))
	  (setq len (1- len))
	  (aset v len (car pair)))
	(setq pair-list (cdr pair-list)))
      (aset v 1 wnd)
      (aset v 0 (window-buffer wnd))
      v)))

;;Return nill if buffer is not sutable for switch
(defun pc-bufsw::can-work-buffer (buffer)
  (let ((name (buffer-name buffer)))
    (not (char-equal ?\  (aref name 0)))))
  
;;Create list of buffer/window pairs that can be visited by buffer switch.
;;Pair (buffer of start-wnd)/start-wnd is not included.
;;The list will be returned in reverse order!
(defun pc-bufsw::get-walk-buffer-window-pair-list(start-wnd buf-list)
  (let ((pair-list nil))
    (while buf-list
      (let ((buffer (car buf-list)))
	(if (pc-bufsw::can-work-buffer buffer)
	    (let ((wnd-list (get-buffer-window-list buffer 0 t)))
	      (if (null wnd-list)
		  (setq pair-list (cons (cons buffer start-wnd) pair-list))
		(while wnd-list
		  (let ((wnd (car wnd-list)))
		    (if (not (eq wnd start-wnd))
			(setq pair-list (cons (cons buffer wnd) pair-list))))
		  (setq wnd-list (cdr wnd-list)))))))
      (setq buf-list (cdr buf-list)))
    pair-list))

;; Echo buffer list. Current buffer marked by <>.
(defun pc-bufsw::show-buffers-names()
  (let* ((width (frame-width))
	 (n (pc-bufsw::find-first-visible width))
	 (str (pc-bufsw::make-show-str n width)))
    (message "%s" str)))

(defun pc-bufsw::find-first-visible(width)
  (let ((first-visible 0)
	(i 2)
	(visible-length (pc-bufsw::show-name-len 0 t)))
    (while (<= i pc-bufsw::cur-index)
      (let ((cur-length (pc-bufsw::show-name-len i (= first-visible i))))
	(setq visible-length (+ visible-length cur-length))
	(if (> visible-length width)
	    (progn
	      (setq first-visible i)
	      (setq visible-length cur-length))))
      (setq i (+ 2 i)))
    first-visible))

(defun pc-bufsw::show-name-len(i at-left-edge)
  (+ (if at-left-edge 2 3) 
     (length (buffer-name (aref pc-bufsw::walk-vector i)))))

(defun pc-bufsw::make-show-str (first-visible width)
  (let* ((i (+ 2 first-visible))
	 (count (length pc-bufsw::walk-vector))
	 (str (pc-bufsw::show-name first-visible t))
	 (visible-length (length str))
	 (continue-loop (not (= i count))))
    (while continue-loop
      (let* ((name (pc-bufsw::show-name i nil))
	     (name-len (length name)))
	(setq visible-length (+ visible-length name-len))
	(if (> visible-length width) (setq continue-loop nil)
	  (setq str (concat str name))
	  (setq i (+ 2 i))
	  (if (= i count) 
	      (setq continue-loop nil)))))
    str))

(defun pc-bufsw::show-name(i at-left-edge)
  (let ((name (buffer-name (aref pc-bufsw::walk-vector i))))
    (cond ((= i pc-bufsw::cur-index) (concat (if at-left-edge "<" " <") 
						 name ">"))
	  (at-left-edge (concat " " name " "))
	  (t (concat "  " name " ")))))

(defun pc-bufsw::choose-next-index (direction)
  (setq pc-bufsw::cur-index
	(mod (+ pc-bufsw::cur-index (* 2 direction)) 
	     (length pc-bufsw::walk-vector))))

(defun pc-bufsw::choose-buf-and-wnd () 
  (let ((buf (aref pc-bufsw::walk-vector pc-bufsw::cur-index))
	(wnd (aref pc-bufsw::walk-vector (1+ pc-bufsw::cur-index)))
	(cur-wnd (selected-window)))
    (if (null wnd) 
	(setq wnd (aref pc-bufsw::walk-vector 1)))
    (if (not (eq cur-wnd wnd))
	(progn 
	  (if (eq cur-wnd (aref pc-bufsw::walk-vector 1))
	      (set-window-buffer cur-wnd (aref pc-bufsw::walk-vector 0)))
	  (let ((wnd-frame (window-frame wnd))
		(cur-frame (selected-frame)))
	    (if (not (eq wnd-frame cur-frame))
		(progn 
		  ;(select-frame wnd-frame)
		  (raise-frame wnd-frame)
		  ;(redirect-frame-focus cur-frame wnd-frame)
		  ;(handle-switch-frame (list 'switch-frame wnd-frame))
		  )))
	  (select-window wnd)))
    (switch-to-buffer buf t)))

;; Called on switch mode close
(defun pc-bufsw::finish()
  (pc-bufsw::restore-order (aref pc-bufsw::walk-vector 
				 pc-bufsw::cur-index) 
			   pc-bufsw::start-buf-list)
  (remove-hook 'pre-command-hook 'pc-bufsw::switch-hook)
  (setq pc-bufsw::walk-vector nil)
  (setq pc-bufsw::cur-index 0)
  (setq pc-bufsw::start-buf-list nil)
  (message nil))


;; Put buffers in Emacs buffer list according to oder indicated by list
;  except put chosen-buffer to the first place.
(defun pc-bufsw::restore-order(chosen-buffer list)
  (while list 
    (let ((buffer (car list)))
      (if (not (eq buffer chosen-buffer)) (bury-buffer buffer)))
    (setq list (cdr list))))


















