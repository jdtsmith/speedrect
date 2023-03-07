;;; speedrect.el ---  Fast modal rectangle commands -*- lexical-binding: t -*-
;; Copyright (C) 2023 J.D. Smith

;; Author: JD Smith
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.0"))
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

(defun speedrect-shift (columns)
  "Shift the current speedrect by COLUMNS (negative to the left, default 1).
Note that point and mark will not move beyond the end of text on their lines."
  (interactive "P")
  (let ((p (point))
	(pcol (current-column))
	(columns (or columns 1))
	(mcol (progn (goto-char (mark)) (current-column))))
    (move-to-column (+ mcol columns))
    (set-mark (point))
    (goto-char p)
    (move-to-column (+ pcol columns))))

(defun speedrect-shift-fast (columns)
  "Shift the current speedrect left by COLUMNS (default 5)."
  (interactive "P")
  (speedrect-shift (or columns 5)))

(defun speedrect-shift-left (columns)
  "Shift the current speedrect left by COLUMNS (default 1)."
  (interactive "P")
  (speedrect-shift (- (or columns 1))))

(defun speedrect-shift-left-fast (columns)
  "Shift the current speedrect left by COLUMNS (default 5)."
  (interactive "P")
  (speedrect-shift (- (or columns 5))))

(defun speedrect-transient-map-info ()
  "Documentation window for speedrect."
  (interactive)
  (with-help-window "Rectangle Mark Command Help"
    (dolist
	(l '("SpeedRect Rectangle Mark Mode Commands\n"
	     "============================================\n\n"
	     "Insertion:\n\n"
	     "  [o] open      fill rectangle with spaces, moving adjacent text right\n"
	     "  [t] string    replace rectangle with prompt string\n\n"
	     "Killing:\n\n"
	     "  [k] kill      kill and save rectangle for yanking\n"
	     "  [d] delete    kill rectangle without saving\n"
	     "  [SPC] del-ws  delete all whitespace, starting from left column\n"
	     "  [c] clear     clear rectangle area by overwriting with spaces\n\n"
	     "Change Rectangle:\n\n"
	     "  [n] new       start a new rectangle from this location\n"
	     "  [l] last      restore the last used rectangle position, if possible\n\n"
	     "Shift Rectangle (can use numeric prefixes)\n\n"
	     "  [S-left]      move the rectangle left\n"
	     "  [S-right]     move the rectangle right\n"
	     "  [M-S-left]    move the rectangle left 5 columns\n"
	     "  [M-S-right]   move the rectangle right 5 columns\n\n"
	     "Copy/Yank:\n\n"
	     "  [w] copy      copy rectangle for future yanking\n"
	     "  [y] yank      yank rectangle, inserting at point\n\n"
	     "Calc:\n\n"
	     "  [#] grab      grab the rectangle as a matrix in calc\n"
	     "  [=] across    sum across rows and grab result in calc\n"
	     "  [+] down      sum down the columns and grab result in calc\n\n"
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
     ("k" kill-rectangle)   	("t" string-rectangle)
     ("o" open-rectangle)   	("w" copy-rectangle-as-kill)
     ("y" yank-rectangle)   	("c" clear-rectangle)
     ("d" delete-rectangle) 	("N" rectangle-number-lines)
     ("SPC" delete-whitespace-rectangle)
     ;; Shift rect
     ("S-<right>" speedrect-shift)
     ("S-<left>" speedrect-shift-left)
     ("M-S-<right>" speedrect-shift-fast)
     ("M-S-<left>" speedrect-shift-left-fast)
     ;; Calc commands
     ("=" calc-grab-sum-across) ("+" calc-grab-sum-down) ("#" calc-grab-rectangle)
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
