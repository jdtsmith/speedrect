

# SpeedRect
<a href="https://elpa.gnu.org/packages/speedrect.html"><img alt="GNU ELPA" src="https://elpa.gnu.org/packages/speedrect.svg"/></a> 

Quick key bindings and other tools for Emacs' `rectangle-mark-mode` 

  
`SpeedRect` is a small Emacs package that automatically provides convenient "modal" key bindings and other tools while in `rectangle-mark-mode` (`C-x SPC`, or `C-M-mouse1` drag, by default).



## Features
<img width="625" alt="image" align="right" src="https://github.com/user-attachments/assets/adcea492-cd51-4123-b5aa-7f0f31eb1029" />

- Single-key [bindings](#key-listing) for all the basic rectangle functions: open, insert string, kill, delete, clear, copy, yank.
- Restarts by default, so you can continue actions on the current rectangle.
- Ability to restore the last rectangle's position, which gets auto-saved.
- Additional command to _invert the deletion_, i.e. delete the _unmarked columns_ in affected rows, keeping only the marked rectangle on those lines.
- Change your mind and start a new rectangle from point.
- Delete whitespace from the left rectangle boundary.
- Column shift: slide the marked rectangle position left or right, 1 (hold shift) or 5 (meta-shift) columns at a time (or any number of columns, with a numerical prefix). 
- Two-way interaction with Calc: send sums or tables of data into calc, and yank processed matrix data back into the rectangle from the top of calc's stack.
- Quickly add multiple cursors on each line of the current rectangle.
- Wrap text within the rectangle.
- A useful help page (`?`). 

## Demo
<details>
<summary>Demo of speedrect's key features</summary>
<video src='https://github.com/user-attachments/assets/4cc4a1ed-1d29-48b2-a365-2d39ef531d80'/></video>
</details>

## What's New

See the release [NEWS](NEWS.org).

## Installation

Install from ELPA, and arrange to `(require 'speedrect)`.  

For users of `use-package`:

```elisp
(use-package speedrect
  :config (speedrect-mode))
```

Or, with `use-package` and `straight`:

```elisp
(use-package speedrect
  :straight (speedrect :type git :host github :repo "jdtsmith/speedrect")
  :config (speedrect-mode))
```
## Usage

Start `rectangle-mark-mode` as usual (`C-x SPC`, by default).  Hit `?` to summon a help buffer of available key bindings.  By default most commands restart `rectangle-mark-mode`; `q` (or any other non-shortcut command) to exit.  If you'd prefer speedrect commands not to continue in this way, set `speedrect-continue=nil`.


Note that as mentioned many commands can be modified with a prefix (e.g. `M-10 S-right` or `C-u k`).

## Hints

A rectangle is just a _region_ (point and mark), specially interpreted.  While marking rectangles, you can use `x` to cycle point among any of the rectangles four corners.  This is useful to make changes to the appropriate side of the selected region.

Use calc, it's super-powerful...

### Using Calc

[Calc](https://www.gnu.org/software/emacs/manual/html_mono/calc.html) is an ancient and powerful calculator in emacs with many capabilities, including operating on [matrix data](https://www.gnu.org/software/emacs/manual/html_node/calc/Matrix-Tutorial.html).  In addition to simple sums, `SpeedRect` offers powerful two-way communication with calc for sending in and yanking out columns of numerical data:

1. It can send columns of numbers to calc as a _matrix_ (2D array of numbers).  Once in calc, you can operate on those numbers using a wide array of operations.  Many things "just work" on matrices (e.g. `1 +` will add one to all the numbers).  Others can easily be mapped over matrix elements (e.g. try `v M Q` to map `sqrt` over all elements). You can combine columns, change their order, and _much_ more.
2. Once you have something you're happy with at the top of calc's *stack* (at the bottom of the `*Calculator*` buffer, entry numbered `1:`), you can:
    - hit `q` (or other window navigation) to return to your original buffer (where `rectangle-mark-mode` will still be active),
    - adjust the position of your rectangle if needed (`S-left/right` and/or `x` is useful for this; a zero-width rectangle is fine), and
    - hit `m` to yank the latest matrix from the calc stack into the buffer, replacing the marked rectangle.

You don't have to be in the same `mark-rectangle-mode` session to yank a matrix from calc.  As long as the height of your rectangle matches the number of matrix rows, it will just work.  So you can start in one buffer, accumulate a matrix, manipulate it, switch to another buffer, and yank it there.

> [!NOTE]
> What you see is what you get in calc.  The numbers shown in calc will be yanked as they appear on the stack, except `speedrect` automatically omits brackets and expands `...` vector shortening.  `v ,` will remove commas for a cleaner appearance.  `v >` will right align numbers.  `d f` will let you set the number of digits after the decimal.  And _many_ more numerical formatting options. 

## Commands

Current single keystroke bindings (available from `?`):

```
SpeedRect Rectangle Mark Mode Commands
============================================================================

Insertion:

  [o] open      open rectangle with tabs/spaces, shifting text right
  [t] string    replace rectangle with string
  
Killing:

  [k] kill      kill and save rectangle for yanking (keep space, with prefix)
  [d] delete    kill rectangle without saving
  [SPC] del-ws  delete all whitespace from left (right, with prefix)
  [c] clear     clear rectangle area by overwriting with spaces
  [r] rest      delete the rest of the columns, keeping the marked rectangle
  
Copy/Yank:

  [w] copy      copy rectangle for future rectangle yanking
  [W] copy-text copy rectangle to kill ring as normal lines of text
  [y] yank      yank rectangle, inserting at point
  
Shift Rectangle (can use numeric prefixes):

  [S-left]      move the rectangle left
  [S-right]     move the rectangle right
  [S-up]        move the rectangle up
  [S-down]      move the rectangle down
  [M-S-left]    move the rectangle left 5 columns
  [M-S-right]   move the rectangle right 5 columns
  [M-S-up]      move the rectangle up 5 lines
  [M-S-down]    move the rectangle down 5 lines
  
Change Rectangle:

  [x] corners   move point around corners of the rectangle
  [n] new       start a new rectangle from this location
  [l] last      restore the last used rectangle, if possible
  
Numerical:

  [N] numbers   fill the rectangle with numeric range (prefix to set start)
  [+/=] increment increment the first number (prefix to set increment)
  [-] decrement decrement the first number (prefix to set decrement)
  [#] grab      grab the rectangle as a matrix in calc
  [_] across    sum across rows and grab result in calc as a vector
  [:] down      sum down the columns and grab result in calc
  [m] yank-mat  yank matrix from top of calc stack, overwriting selection
  
Etc:

  [f] fill      fill text within rectangle (prefix to prompt fill width)
  [M] multiple-cursors  add cursors at current column
  [u] undo      undo last edit (and restart)
  [?] help      view this Help buffer
  [q] quit      exit rectangle-mark-mode
```

## Related packages

- [phi-rectangle](https://github.com/zk-phi/phi-rectangle)
- [cua-rectangle-mark-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/CUA-Bindings.html)

## Thanks
 Contributors of ideas from [this discussion](https://www.reddit.com/r/emacs/comments/11k9u73/a_tiny_modal_rectanglemarkmode/)
