;;; speedrect.el ---  Fast modal rectangle commands -*- lexical-binding: t -*-
;; Copyright (C) 2023 J.D. Smith

;; Author: JD Smith
;; Created: 2023
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1") (compat "29.1.4.0"))
;; Homepage: https://github.com/jdtsmith/speedrect
;; Keywords: convenience
;; Prefix: speedrect
;; Separator: -

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

;; This package adds convenient modal keybinding when
;; rectangle-mark-mode is active (typically on C-x SPC).  Hit ? for
;; help.

;;; Code:
(require 'rect)
(require 'calc)
(require 'subr-x)
(require 'compat)
(eval-when-compile (require 'cl-lib))

(defun speedrect-linecol ()
  "Return line and column as list."
  (list (line-number-at-pos) (current-column)))

(defvar-local speedrect-last nil
  "Last rectangle position.
Stored as (point-line point-col mark-line mark-col)")

(defun speedrect-stash (&rest _r)
  "Stash the line and column of point and mark.
Used as :before advice for commands which operate on the marked
rect and exit `rectangle-mark-mode'."
  (if rectangle-mark-mode
      (setq speedrect-last
	    (append (speedrect-linecol)
		    (save-excursion
		      (goto-char (mark))
		      (speedrect-linecol))))))

(defun speedrect-recall-last ()
  "Restore last saved rectangle position."
  (interactive)
  (pcase speedrect-last
    (`(,pl ,pc ,ml ,mc)
     (goto-char (point-min))
     (forward-line (1- ml))
     (move-to-column mc)
     (set-mark (point))
     (forward-line (- pl ml))
     (move-to-column pc)
     (message "Restored last rectangle %d %d, %d %d" ml mc pl pc))
    (_ (message "No stored rectangle position"))))

(defun speedrect-restart ()
  "Start a new rectangle, setting mark at the current position."
  (interactive)
  (set-mark (point)))

(defun speedrect-shift-right (columns)
  "Shift the current speedrect by COLUMNS (negative to the left, default 1).
Note that point and mark will not move beyond the end of text on their lines."
  (interactive "P")
  (let ((p (point))
	(pcol (current-column))
	(columns (or columns 1))
	(mcol (progn (goto-char (mark)) (current-column))))
    (move-to-column (max 0 (+ mcol columns)))
    (set-mark (point))
    (goto-char p)
    (move-to-column (max 0 (+ pcol columns)))))

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
	(pcol (current-column))
	(lines (or lines 1))
	(mcol (progn (goto-char (mark)) (current-column))))
    (forward-line lines)
    (move-to-column mcol)
    (set-mark (point))
    (goto-char p)
    (forward-line lines)
    (move-to-column pcol)))

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

(defun speedrect-delete-rest (start end)
  "Keep rectangle between START and END, deleting the rest of the affected lines."
  (interactive "r")
  (speedrect-stash)
  (let ((rect (extract-rectangle start end)))
    (delete-region (progn (goto-char start) (line-beginning-position))
		   (progn (goto-char end) (line-end-position)))
    (insert (string-join rect "\n"))))

(defun speedrect--replace-with-rect (startcol endcol rect &optional from to)
  "Insert RECT's 2nd element, replacing text between STARTCOL and ENDCOL.
The element is removed from RECT by side effect.  FROM and TO are
optional substring positions to insert."
  (delete-rectangle-line startcol endcol nil)
  (insert (substring (pop (cdr rect)) from to)))

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

(defun speedrect-yank-from-calc (start end)
  "Yank matrix from top of calc stack, overwriting the marked rectangle.
START and END are the interactively-defined region beginning and
end.  The (matrix) value at the top of the calc stack is used,
and must have the same number of rows as the height of the marked
rectangle.  To avoid copying brackets and commas, v [ and v , may
be used in calc.  A minimum of one padding space is preserved on
each side of the inserted text."
  (interactive "r")
  (if-let ((rectangle-mark-mode)
	   (buf (get-buffer "*Calculator*")))
      (let* ((b (region-beginning))
	     (e (region-end))
	     (height (+ (count-lines b e)
			(if (eq (char-before e) ?\n) 1 0)))
	     crect lr)
	(save-excursion
	  (with-current-buffer buf
	    (let* ((cstart (progn (calc-cursor-stack-index 1)
				  (if (and calc-line-numbering (looking-at "[0-9]+: "))
				      (match-end 0)
				    (point))))
		   (cend (progn (calc-cursor-stack-index 0) (line-end-position 0))))
	      (setq crect (extract-rectangle cstart cend))))
	  (if (eq (length crect) height)
	      (progn
		(setq lr (speedrect--lr-space crect))
		(push nil crect) ; dummy, for consuming in apply-on-rectangle
		(apply-on-rectangle 'speedrect--replace-with-rect
				    start end crect
				    (max 0 (1- (car lr)))
				    (min 0 (- (1- (cdr lr)))))
		(speedrect-stash))
	    (user-error "Row count of calc matrix (%d) does not match rectangle height (%d)"
			(length crect) height))))
    (user-error "Calc rectangle yank not possible here")))

(defun speedrect-transient-map-info ()
  "Documentation window for speedrect."
  (interactive)
  (with-help-window "SpeedRect Command Key Help"
    (dolist
	(l '("SpeedRect Rectangle Mark Mode Commands\n"
	     "============================================================================\n\n"
	     "Insertion:\n\n"
	     "  [o] open      fill rectangle with tabs/spaces, moving adjacent text right\n"
	     "  [t] string    replace rectangle with prompt string\n\n"
	     "Killing:\n\n"
	     "  [k] kill      kill and save rectangle for yanking\n"
	     "  [d] delete    kill rectangle without saving\n"
	     "  [SPC] del-ws  delete all whitespace, starting from left column\n"
	     "  [c] clear     clear rectangle area by overwriting with spaces\n"
	     "  [r] rest      delete the rest of the columns, keeping the marked rectangle\n\n"
	     "Change Rectangle:\n\n"
	     "  [n] new       start a new rectangle from this location\n"
	     "  [l] last      restore the last used rectangle position, if possible\n\n"
	     "Shift Rectangle (can use numeric prefixes):\n\n"
	     "  [S-left]      move the rectangle left\n"
	     "  [S-right]     move the rectangle right\n"
	     "  [S-up]        move the rectangle up\n"
	     "  [S-down]      move the rectangle down\n"
	     "  [M-S-left]    move the rectangle left 5 columns\n"
	     "  [M-S-right]   move the rectangle right 5 columns\n"
	     "  [M-S-up]      move the rectangle up 5 columns\n"
	     "  [M-S-down]    move the rectangle down 5 lines\n\n"
	     "Copy/Yank:\n\n"
	     "  [w] copy      copy rectangle for future yanking\n"
	     "  [y] yank      yank rectangle, inserting at point\n\n"
	     "Numerical:\n\n"
	     "  [N] numbers   fill the rectangle with numbers (prefix to set start)\n"
	     "  [#] grab      grab the rectangle as a matrix in calc\n"
	     "  [_] across    sum across rows and grab result in calc as a vector\n"
	     "  [:] down      sum down the columns and grab result in calc\n"
	     "  [m] yank-mat  yank matrix from top of calc stack, overwriting selected rect\n\n"
	     "Etc:\n\n"
	     "  [?] help      view this Help buffer\n"
	     "  [q] quit      exit rectangle-mark-mode"))
      (princ l))))

(defun speedrect-quit ()
  "Quit speedrect."
  (interactive)
  (deactivate-mark))

(defun speedrect-create-bindings ()
  "Create the bindings for speedrect.
Also adds :before advice to rectangle commands to stash the rect
prior to deactivating mark."
  (cl-loop
   for (key def) in
   '(;; Rectangle basics
     ("k" kill-rectangle)   	 ("t" string-rectangle)
     ("o" open-rectangle)   	 ("w" copy-rectangle-as-kill)
     ("y" yank-rectangle)   	 ("c" clear-rectangle)
     ("d" delete-rectangle) 	 ("N" rectangle-number-lines)
     ("r" speedrect-delete-rest) ("SPC" delete-whitespace-rectangle)
     ;; Shift rect
     ("S-<right>" speedrect-shift-right)
     ("S-<left>" speedrect-shift-left)
     ("M-S-<right>" speedrect-shift-right-fast)
     ("M-S-<left>" speedrect-shift-left-fast)
     ("S-<up>" speedrect-shift-up)
     ("S-<down>" speedrect-shift-down)
     ("M-S-<up>" speedrect-shift-up-fast)
     ("M-S-<down>" speedrect-shift-down-fast)
     ;; Calc commands
     ("_" calc-grab-sum-across) (":" calc-grab-sum-down) ("#" calc-grab-rectangle)
     ("m" speedrect-yank-from-calc)
     ;; Special
     ("n" speedrect-restart) ("l" speedrect-recall-last)
     ("?" speedrect-transient-map-info) ("q" speedrect-quit))
   for dname = (symbol-name def)
   do
   (define-key rectangle-mark-mode-map (kbd key) def)
   (unless (seq-some (lambda (name)
		       (string-prefix-p name dname))
		     '("speedrect" "calc"))
     (advice-add def :before #'speedrect-stash)))
  (put 'rectangle-mark-mode-map 'speedrect t))

(defun speedrect-hook ()
  "Setup speedrect for `rectangle-mark-mode'."
  (when rectangle-mark-mode
    (unless (get 'rectangle-mark-mode-map 'speedrect)
      (speedrect-create-bindings))
    (message "%s: [?] for help%s"
	     (propertize "SpeedRect" 'face 'success)
	     (if speedrect-last
		 (format "  %s:%S"
			 (propertize "last-rect" 'face 'bold)
			 speedrect-last)
	       ""))))

;;; autoload
(add-hook 'rectangle-mark-mode-hook #'speedrect-hook)

(provide 'speedrect)
;;; speedrect.el ends here
