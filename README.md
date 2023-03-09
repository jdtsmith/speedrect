# SpeedRect
<img src="https://user-images.githubusercontent.com/93749/223541236-cd77d56c-d1d0-40cd-8d69-f6c12dfe3d3a.png" width=423 align="right">


Quick key bindings and other tools for Emacs' `rectangle-mark-mode`.

`SpeedRect` is a small Emacs package that provides convenient "modal" key bindings and other tools while in `rectangle-mark-mode` (`C-x SPC`, by default).

## Features

- [Bindings](#key-listing) for all the basic rectangle functions: open, insert string, kill, delete, clear, copy, yank.
- Useful, but not default-bound rectangle command to delete all whitespace right.
- Additional command to _invert the deletion_, i.e. delete the _unmarked columns_, keeping the marked rectangle.
- Save and restore the last rectangle's position.
- Start a new rectangle from point. 
- Two-way interaction with Calc: send data or sums into calc, and yank matrix data into the buffer from the calc stack.
- Column shift: slide the rectangle mark left or right, 1 or 5 columns at a time (or any number of columns, with a numerical prefix). 
- A help page (hit `?`). 

## Installation

Just download, add to path, and arrange to `(require 'speedrect)`.  For users of `use-package`:

```elisp
(use-package speedrect
  :load-path "~/code/emacs/speedrect") ; or wherever
```

Or, with `use-package` and `straight`:

```elisp
(use-package speedrect
  :straight (speedrect :type git :host github :repo "jdtsmith/speedrect"))
```
## Usage

Start `rectangle-mark-mode` as usual (`C-x SPC`, by default).  Hit `?` to summon a help buffer of available key bindings.


## Hints

A rectangle is just a _region_ (point and mark), specially interpreted.  While marking rectangles, you can `C-x C-x` to switch point and mark to make changes to the top/bottom of the selected region (hit it yet again to switch to the other diagonal).

Use calc, it's super-powerful:

### Using Calc

[Calc](https://www.gnu.org/software/emacs/manual/html_mono/calc.html) is an ancient and powerful calculator in emacs with many capabilities, including operating on [matrix data](https://www.gnu.org/software/emacs/manual/html_node/calc/Matrix-Tutorial.html).  In addition to simple sums, `SpeedRect` offers powerful two-way communication with calc for sending in and yanking out columns of numerical data:

1. It can send columns of numbers to calc as a _matrix_ (2D array of numbers).  Once in calc, you can operate on those numbers using a wide array of operations.  Many things "just work" on matrices (e.g. `1 +` will add one to all the numbers).  Others can easily be mapped over matrix elements (try `v m`). You can combine columns, change their order, and much more.
2. Once you have something you're happy with at the top of calc's *stack* (the entry numbered `1:`), you can:
    - hit `q` to return to your original buffer (where `rectangle-mark-mode` will still be active),
    - adjust the position of your rectangle if needed (`S-left/right` is useful for this; a zero-width rectangle is fine), and
    - hit `m` to yank the matrix from calc into the buffer (if it has the right number of rows), replacing the marked rectangle.

You don't have to be in the same `mark-rectangle-mode` session to yank a matrix from calc.  As long as the height of your rectangle matches the number of matrix rows, it will just work.  So you can start in one buffer, accumulate a matrix, manipulate it, switch to another buffer, and yank it there.

**Note**: what you see is what you get in calc.  The matrix will be yanked _exactly_ as it appears.  `v [` and `v ,` will remove the brackets and commas.  `v >` will right align numbers.  While `v .` is convenient for shortening long entries, undo it before yanking.  `d f` will let you set the number of digits after the decimal.  And many more options.  

## Key Listing

```
SpeedRect Rectangle Mark Mode Commands
============================================================================
Insertion:
  [o] open      fill rectangle with tabs/spaces, moving adjacent text right
  [t] string    replace rectangle with prompt string
Killing:
  [k] kill      kill and save rectangle for yanking
  [d] delete    kill rectangle without saving
  [SPC] del-ws  delete all whitespace, starting from left column
  [c] clear     clear rectangle area by overwriting with spaces
  [r] rest      delete the rest of the columns, keeping the marked rectangle
Change Rectangle:
  [n] new       start a new rectangle from this location
  [l] last      restore the last used rectangle position, if possible
Shift Rectangle (can use numeric prefixes):
  [S-left]      move the rectangle left
  [S-right]     move the rectangle right
  [M-S-left]    move the rectangle left 5 columns
  [M-S-right]   move the rectangle right 5 columns
Copy/Yank:
  [w] copy      copy rectangle for future yanking
  [y] yank      yank rectangle, inserting at point
Numerical:
  [N] numbers   fill the rectangle with numbers (prefix to set start)
  [#] grab      grab the rectangle as a matrix in calc
  [=] across    sum across rows and grab result in calc as a vector
  [+] down      sum down the columns and grab result in calc
  [m] yank-mat  yank matrix from top of calc stack, overwriting selected rect
Etc:
  [?] help      view this Help buffer
  [q] quit      exit rectangle-mark-mode
```

## Thanks
 Contributors of ideas from [this discussion](https://www.reddit.com/r/emacs/comments/11k9u73/a_tiny_modal_rectanglemarkmode/)
