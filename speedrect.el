;;; speedrect.el ---  Fast modal rectangle commands -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: JD Smith <jdtsmith+elpa@gmail.com>
;; Created: 2023
;; Version: 0.7
;; Package-Requires: ((emacs "29.1") (compat "30"))
;; Homepage: https://github.com/jdtsmith/speedrect
;; Keywords: convenience

;; speedrect is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; speedrect is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package adds convenient modal keybindings and additional
;; functionality when rectangle-mark-mode is active (typically on C-x
;; SPC).  Hit ? for help.

;;; Code:
;;;; Requires
(require 'rect)
(require 'calc)
(require 'subr-x)
(require 'compat)
(require 'cl-lib)

(defgroup speedrect ()
  "Fast modal rectangle commands."
  :group 'rectangle)

;;;; Customization
(defcustom speedrect-continue t
  "Stay in speedrect until quit."
  :type 'boolean)

(defun speedrect-linecol ()
  "Return line and column as list."
  (list (line-number-at-pos) (current-column)))

;;;; Variables
(defvar-local speedrect-last nil
  "Last rectangle position.
Stored as (POINT-LINE POINT-COL MARK-LINE MARK-COL), where ....")

(defun speedrect-recall-last ()
  "Restore last saved rectangle position."
  (interactive)
  (unless rectangle-mark-mode
    (rectangle-mark-mode 1))
  (pcase speedrect-last
    (`((,point . ,mark) ,point-crutches ,mark-crutches)
     (set-mark (marker-position mark))
     (goto-char point)
     (setf (window-parameter nil 'rectangle--point-crutches) point-crutches)
     (setq-local rectangle--mark-crutches mark-crutches)
     (when (called-interactively-p 'interactive)
       (message "Restored last rectangle %d %d"
		(marker-position point) (marker-position mark))))
    (_ (message "No stored rectangle position"))))

(defun speedrect-stash ()
  "Stash the line and column of point and mark."
  (when rectangle-mark-mode
    (let ((pm (car speedrect-last)))
      (if pm
	  (progn
	    (move-marker (car pm) (point))
	    (move-marker (cdr pm) (mark)))
	(setq pm (cons (point-marker) (copy-marker (mark-marker)))))
      (setq speedrect-last
	    (list pm
		  (window-parameter nil 'rectangle--point-crutches)
		  rectangle--mark-crutches)))))

(defun speedrect-restart ()
  "Start a new rectangle, setting mark at the current position."
  (interactive)
  (set-mark (point)))

;;;; Rectangle Movement
(defsubst speedrect-right-char (columns)
  "Move COLUMNS right unless COLUMNS<0 and at left edge."
  (unless (and (eq (current-column) 0) (< columns 0))
    (rectangle-right-char columns)))

(defun speedrect-shift-right (columns)
  "Shift the current speedrect by COLUMNS (negative to the left, default 1).
Note that point and mark will not move beyond the end of text on their lines."
  (interactive "P")
  (let ((columns (or columns 1)))
    (dotimes (_ 2)
      (speedrect-right-char columns)
      ;; we bind this-command=nil to avoid the repeat logic of
      ;; rectangle-exchange-point-and-mark
      (let ((this-command nil)) (rectangle-exchange-point-and-mark)))))

(defun speedrect-shift-right-fast (columns)
  "Shift the current speedrect left by COLUMNS (default 5)."
  (interactive "P")
  (speedrect-shift-right (or columns 5)))

(defun speedrect-shift-left (columns)
  "Shift the current speedrect left by COLUMNS (default 1)."
  (interactive "P")
  (speedrect-shift-right (- (or columns 1))))

(defun speedrect-shift-left-fast (columns)
  "Shift the current speedrect left by COLUMNS (default 5)."
  (interactive "P")
  (speedrect-shift-right (- (or columns 5))))

(defun speedrect-shift-down (lines)
  "Shift rectangle down by LINES."
  (interactive "P")
  (let ((p (point))
	(lines (or lines 1)))
    (goto-char (mark))
    (rectangle-next-line lines)
    (set-mark (point))
    (goto-char p)
    (rectangle-next-line lines)))

(defun speedrect-shift-down-fast (lines)
  "Shift the current speedrect down by LINES (default 5)."
  (interactive "P")
  (speedrect-shift-down (or lines 5)))

(defun speedrect-shift-up (lines)
  "Shift the current speedrect up by LINES (default 1)."
  (interactive "P")
  (speedrect-shift-down (- (or lines 1))))

(defun speedrect-shift-up-fast (lines)
  "Shift the current speedrect up by LINES (default 5)."
  (interactive "P")
  (speedrect-shift-down (- (or lines 5))))

;;;; Manipulation
(defun speedrect-undo ()
  "Undo, temporarily disabling mark."
  (interactive)
  (let ((mark-active nil)) (undo)))

(defun speedrect-kill-rectangle (start end &optional preserve-width)
  "Kill rectangle between START and END.
If PRESERVE-WIDTH is non-nil, preserves width."
  (interactive "*r\nP")
  (if preserve-width
      (progn
	(copy-rectangle-as-kill start end)
	(clear-rectangle start end))
    (kill-rectangle start end)))

(defun speedrect-yank-rectangle-dwim ()
  "Yank rectangle, but first swap mark and point if needed.
Any selected rectangle is first deleted."
  (interactive)
  (if (< (mark) (point)) (exchange-point-and-mark))
  (delete-rectangle (point) (mark))
  (call-interactively #'yank-rectangle))

(defun speedrect-copy-rectangle-dwim ()
  "Copy rectangle, but first swap mark and point if needed."
  (interactive)
  (if (< (mark) (point)) (exchange-point-and-mark))
  (call-interactively #'copy-rectangle-as-kill))

(defun speedrect-delete-rest (start end)
  "Keep rectangle between START and END, deleting the rest of the affected lines."
  (interactive "r")
  (let ((rect (extract-rectangle start end)))
    (delete-region (progn (goto-char start) (line-beginning-position))
		   (progn (goto-char end) (line-end-position)))
    (insert (string-join rect "\n"))))

(defun speedrect--replace-with-rect (startcol endcol lines width
					      &optional from to)
  "Insert LINES' 1st element, replacing text between STARTCOL and ENDCOL.
Each string in LINES is space-padded to occupy at least WIDTH
characters.  The element is removed from LINES by side effect.
FROM and TO are optional substring positions to insert.  WIDTH is
used to insert spaces if the LINES list is not long enough to
full the full rectangle."
  (delete-rectangle-line startcol endcol nil)
  (let ((el (car lines))
	(w (- width (or from 0) (- (or to 0)))))
    (move-to-column startcol t)
    (if el
	(progn
	  (insert (substring el from (+ width to)))
	  (setcar lines (cadr lines))
	  (setcdr lines (cddr lines)))
      (insert (make-string w ?\s)))))

(defun speedrect--lr-space (rect)
  "Compute and return the minimum number of flanking spaces.
Returns a cons of the minimum number of space characters
appearing on the left- and right-hand ends of all strings in
RECT (a list of strings)."
  (cl-loop for s in rect
	   if (string-match "^\\( *\\)[^ ].*?\\( *\\)$" s)
	   minimize (match-end 1) into left
	   minimize (- (length s) (match-beginning 2)) into right
	   finally return (cons left right)))

(defun speedrect--delete-ws-line (start end right)
  "Delete left whitespace on line between columns START and END, preserving width.
If RIGHT is non-nil, delete from right."
  (let* ((delete-from (if right end start))
	 (pad-at (if right start end))
	 (guard (if right (line-beginning-position) (line-end-position)))
	 (skip-fn (if right 'skip-syntax-backward 'skip-syntax-forward)))
    (when (and (= (move-to-column delete-from t) delete-from) (/= (point) guard))
      (let* ((beg (point))
	     (end (progn (funcall skip-fn " " guard) (point)))
	     (trim-len (abs (- beg end)))
	     (pad-col (if right pad-at (- pad-at trim-len))))
	(message "Trimming from %d:%d %d->%d [%d]"
		 delete-from pad-at beg end trim-len)
	(delete-region beg end)
	(when (and (> trim-len 0) (= (move-to-column pad-col t) pad-col))
	  (insert (make-string trim-len ?\s)))))))

(defun speedrect--delete-ws (start end &optional right)
  "Delete left whitespace within rectangle with corners START and END.
With prefix or non-nil RIGHT, delete whitespace from right-hand
side instead."
  (interactive "*r\nP")
  (apply-on-rectangle #'speedrect--delete-ws-line start end right))

;;;; Numerical/calc integration
(defun speedrect--increment-first-number (startcol endcol increment)
  "Increment the first number in the rectangle line by INCREMENT.
The rectangle line spans columns from STARTCOL to ENDCOL."
  (save-excursion
    (let* ((width (- endcol startcol))
	   (end (progn (move-to-column startcol)
		       (min (+ (point) width) (line-end-position)))))
      (cond
       ((re-search-forward (rx (* space) "0x" (group (+ xdigit))) end t)
	(let ((fmt (format "0x%%0%dx" (- (match-end 1) (match-beginning 1))))
	      (num (string-to-number (match-string-no-properties 1) 16)))
	  (replace-match (format fmt (+ num increment)))))
       ((re-search-forward (rx (* space)
			       (group (? (any ?- ?+)) (+ (any "0-9")))
			       (group (* space)))
			   end t)
        (let* ((len (length (match-string 0)))
	       (num-txt (match-string-no-properties 1))
               (prefix (if (= (aref num-txt 0) ?0) "0" ""))
               (num (string-to-number num-txt 10))
               (fmt (format "%%%sd" prefix))
	       (replace (format fmt (+ num increment)))
	       (pad-avail (max 0 (- len (length replace))))
	       (rear-pad (min (length (match-string 2)) pad-avail))
	       (front-pad (- pad-avail rear-pad)))
          (replace-match (concat (make-string front-pad ?\s)
				 replace
				 (make-string rear-pad ?\s)))))))))

(defun speedrect-increment-first-number (start end &optional increment)
  "Increment the first integer or hexadecimal number in the rectangle.
The rectangle spans from START to END.  Increment is 1 by
default, but can be set with INCREMENT."
  (interactive "r\np")
  (apply-on-rectangle #'speedrect--increment-first-number
		      start end (or increment 1)))

(defun speedrect-decrement-first-number (start end &optional decrement)
  "Decrement the first integer or hexadecimal number in the rectangle.
The rectangle spans from START to END.  Decrement is 1 by
default, but can be set with DECREMENT."
  (interactive "r\np")
  (speedrect-increment-first-number start end (- (or decrement 1))))

(defun speedrect-yank-from-calc (start end)
  "Yank matrix from top of calc stack, overwriting the marked rectangle.
START and END are the interactively-defined region beginning and
end.  The (matrix) value at the top of the calc stack is used,
and must have the same number of rows as the height of the marked
rectangle.  Brackets are not copied and short vectors are
expanded.  To avoid copying commas, `v ,' may be used in calc,
and `d n' with a prefix arg changes the displayed precision.  A
minimum of one padding space is preserved on each side of the
inserted text."
  (interactive "r")
  (if-let* ((rectangle-mark-mode)
	    (buf (get-buffer "*Calculator*")))
      (let* ((lines (with-current-buffer buf
		      (string-lines
		       (let ((calc-vector-brackets nil)
			     (calc-full-vectors t))
			 (math-format-value (calc-top))))))
	     (b (region-beginning))
	     (e (region-end))
	     (rect-height (+ (count-lines b e) (if (eq (char-before e) ?\n) 1 0))))
	(unless (= (length lines) rect-height)
	  (warn "Row count of calc matrix (%d) does not match rectangle height (%d), %s"
		(length lines) rect-height
		(if (> rect-height (length lines)) "inserting blanks" "truncating"))
	  (when (< rect-height (length lines))
	    (setq lines (cl-subseq lines 0 rect-height))))
	(let* ((lr (speedrect--lr-space lines))
	       (wdth (length (car lines))) ; note: last line may differ
	       (low (max 0 (1- (car lr))))
	       (high (min 0 (- (1- (cdr lr))))))
	  (apply-on-rectangle #'speedrect--replace-with-rect
			      start end lines wdth low high)))
    (user-error "Calc rectangle yank not possible here")))

;;* 
;;;; Multiple Cursors
(defun speedrect--mc-restart ()
  "Restart speedrect with stored rectangle.
Also removes itself from `multiple-cursors-mode-hook'."
  (unless multiple-cursors-mode
    (remove-hook 'multiple-cursors-mode-hook
		 #'speedrect--mc-restart t) 
    (speedrect-recall-last)))

(declare-function mc/edit-lines "mc-edit-lines")
(defun speedrect-multiple-cursors ()
  "Add multiple cursors on each line at the current column."
  (interactive)
  (condition-case nil
      (require 'multiple-cursors)
    (error (user-error "Multiple-cursors not found"))
    (:success
     (let ((col (current-column)))
       (speedrect-quit)
       (add-hook 'multiple-cursors-mode-hook
		 #'speedrect--mc-restart nil t)
       (exchange-point-and-mark)
       (move-to-column col)
       (mc/edit-lines)))))

;;;; Rectangles as Text
(defun speedrect-copy-rectangle-as-text ()
  "Copy the current rectangle to the kill ring as normal text."
  (interactive)
  (let ((rect (extract-rectangle (region-beginning) (region-end))))
    (kill-new (string-join rect "\n"))
    (message "Copied rectangle as %d lines" (length rect))))

(defun speedrect-fill-text (start end &optional width)
  "Fill text in the selected rectangle START..END to the given WIDTH.
WIDTH defaults to the selected rectangle's width, but can be set
with numeric prefix arg.  If the marked rectangle is entirely
blank, yank and fill the last killed rectangle instead.
Whitespace is not preserved."
  (interactive "*r")
  (unless width
    (setq width (cond ((null current-prefix-arg)
		       (car (rectangle-dimensions start end)))
		      ((consp current-prefix-arg)
		       (read-number "Fill width: "))
		      (t (prefix-numeric-value current-prefix-arg)))))
  (when (<= width 0) (user-error "Fill width must be >0"))
  (with-undo-amalgamate
    (let* ((height (cdr (rectangle-dimensions start end)))
	   (rect (extract-rectangle start end))
	   (blankp (cl-every
		    (lambda (l) (string-match-p (rx bos (* space) eos) l))
		    rect))
	   (formatter (apply-partially #'format (format "%%-%ds" width)))
	   lines)
      (with-temp-buffer
	(if blankp
	    (yank-rectangle)
	  (insert (string-trim (string-join rect " "))))
	(let ((fill-column width))	; fill@width
	  (fill-region (point-min) (point-max)))
	(setq lines (mapcar formatter (string-lines (buffer-string)))))
      (when (< height (length lines))
	(save-excursion
	  (when (> end start)
	    (goto-char end))
	  (end-of-line)
	  (open-line (- (length lines) height))))
      (delete-rectangle start end t)
      (when (> (point) (mark)) (exchange-point-and-mark))
      (insert-rectangle lines))))

;;;; Help
(defun speedrect-transient-map-info ()
  "Documentation window for speedrect."
  (interactive)
  (with-help-window "SpeedRect Command Key Help"
    (princ "SpeedRect Rectangle Mark Mode Commands
============================================================================\n
Insertion:\n
  [o] open      open rectangle with tabs/spaces, shifting text right
  [t] string    replace rectangle with string\n
Killing:\n
  [k] kill      kill and save rectangle for yanking (keep space, with prefix)
  [d] delete    kill rectangle without saving
  [SPC] del-ws  delete all whitespace from left (right, with prefix)
  [c] clear     clear rectangle area by overwriting with spaces
  [r] rest      delete the rest of the columns, keeping the marked rectangle\n
Copy/Yank:\n
  [w] copy      copy rectangle for future rectangle yanking
  [W] copy-text copy rectangle to kill ring as normal lines of text
  [y] yank      yank rectangle, inserting at point\n
Shift Rectangle (can use numeric prefixes):\n
  [S-left]      move the rectangle left
  [S-right]     move the rectangle right
  [S-up]        move the rectangle up
  [S-down]      move the rectangle down
  [M-S-left]    move the rectangle left 5 columns
  [M-S-right]   move the rectangle right 5 columns
  [M-S-up]      move the rectangle up 5 lines
  [M-S-down]    move the rectangle down 5 lines\n
Change Rectangle:\n
  [x] corners   move point around corners of the rectangle
  [n] new       start a new rectangle from this location
  [l] last      restore the last used rectangle, if possible\n
Numerical:\n
  [N] numbers   fill the rectangle with numeric range (prefix to set start)
  [+/=] increment increment the first number (prefix to set increment)
  [-] decrement decrement the first number (prefix to set decrement)
  [#] grab      grab the rectangle as a matrix in calc
  [_] across    sum across rows and grab result in calc as a vector
  [:] down      sum down the columns and grab result in calc
  [m] yank-mat  yank matrix from top of calc stack, overwriting selection\n
Etc:\n
  [f] fill      fill text within rectangle (prefix to prompt fill width)
  [M] multiple-cursors  add cursors at current column
  [u] undo      undo last edit (and restart)
  [?] help      view this Help buffer
  [q] quit      exit rectangle-mark-mode")))

;;;; Bindings and mode
(defun speedrect-quit ()
  "Quit speedrect."
  (interactive)
  (speedrect-stash)
  (deactivate-mark))

(defun speedrect--wrap-command (command &optional after)
  "Wrap an interactive COMMAND to store rect and (posibly) reenter.
Many/most rectangle commands deactivate mark and exit
`rectangle-mark-mode'.  This stashes the rectangle before such
commands, and, if custom option `speedrect-continue' is non-nil,
restarts with the same rectangle.  If AFTER is non-nil, stash the
rectangle after the command runs, otherwise, stash it before."
  (let ((func
	 (lambda ()
	   (interactive)
	   (unless after (speedrect-stash))
	   (call-interactively command)
	   (when after (speedrect-stash))
	   (when speedrect-continue
	     (run-at-time 0 nil
			  (lambda (buf)
			    (with-current-buffer buf
			      (activate-mark)
			      (rectangle-mark-mode 1)
			      (speedrect-recall-last)))
			  (current-buffer)))))
	(name (intern (concat (symbol-name command) "--speedrect_wrap"))))
    (defalias name func)
    name))

(defun speedrect-create-bindings ()
  "Create the bindings for speedrect."
  (cl-loop
   for (key def wrap) in
   '(;; Rectangle basics
     ("k" speedrect-kill-rectangle t)
     ("t" string-rectangle after)
     ("o" open-rectangle t)
     ("w" copy-rectangle-as-kill t)
     ("W" speedrect-copy-rectangle-as-text)
     ("y" speedrect-yank-rectangle-dwim t)
     ("c" clear-rectangle t)
     ("d" delete-rectangle after)
     ("N" rectangle-number-lines t)
     ("r" speedrect-delete-rest after)
     ("SPC" speedrect--delete-ws after)
     ("x" rectangle-exchange-point-and-mark)
     ;; Shift rect
     ("S-<right>" speedrect-shift-right)
     ("S-<left>" speedrect-shift-left)
     ("M-S-<right>" speedrect-shift-right-fast)
     ("M-S-<left>" speedrect-shift-left-fast)
     ("S-<up>" speedrect-shift-up)
     ("S-<down>" speedrect-shift-down)
     ("M-S-<up>" speedrect-shift-up-fast)
     ("M-S-<down>" speedrect-shift-down-fast)
     ;; Numerical/Calc commands
     ("-" speedrect-decrement-first-number after)
     ("+" speedrect-increment-first-number after)
     ("=" speedrect-increment-first-number after)
     ("_" calc-grab-sum-across)
     (":" calc-grab-sum-down)
     ("#" calc-grab-rectangle)
     ("m" speedrect-yank-from-calc after)
     ;; Special
     ("n" speedrect-restart)
     ("l" speedrect-recall-last)
     ("f" speedrect-fill-text after)
     ("u" speedrect-undo t)
     ("M" speedrect-multiple-cursors)
     ("?" speedrect-transient-map-info)
     ("q" speedrect-quit))
   for bind = (if wrap (speedrect--wrap-command def (eq wrap 'after)) def)
   do (define-key rectangle-mark-mode-map (kbd key) bind))
  (put 'rectangle-mark-mode-map 'speedrect t))

(defun speedrect-hook ()
  "Setup speedrect for `rectangle-mark-mode'."
  (when rectangle-mark-mode
    (speedrect-create-bindings)
    (message "%s: [?] for help%s"
	     (propertize "SpeedRect" 'face 'success)
	     (if speedrect-last
		 (format "  %s:%d->%d"
			 (propertize "last-rect" 'face 'bold)
			 (marker-position (caar speedrect-last))
			 (marker-position (cdar speedrect-last)))
	       ""))))

;;;###autoload
(define-minor-mode speedrect-mode
  "Enable rectangular modal editing."
  :global t
  (if speedrect-mode
      (add-hook 'rectangle-mark-mode-hook #'speedrect-hook)
    (remove-hook 'rectangle-mark-mode-hook #'speedrect-hook)))

(provide 'speedrect)
;;; speedrect.el ends here
