# SpeedRect
<img src="https://user-images.githubusercontent.com/93749/223541236-cd77d56c-d1d0-40cd-8d69-f6c12dfe3d3a.png" width=423 align="right">


Quick key bindings and other tools for Emacs' `rectangle-mark-mode`.

SpeedRect is a small Emacs package that provides convenient "modal" key bindings and other tools while in `rectangle-mark-mode` (`C-x SPC`, by default).

## Features

- [Bindings](#key-listing) for all the basic rectangle functions: open, insert string, kill, delete, clear, copy, yank.
- Useful, but not default-bound rectangle command to delete all whitespace right.
- Additional command to _invert the deletion_, i.e. delete the _unmarked columns_, keeping the marked rectangle.
- Save and restore the last rectangle's position.
- Start a new rectangle from point. 
- Calc grab and sum commands.
- Column shift: slide a rectangle left or right, 1 or 5 columns at a time (or any number, with a prefix). 
- A help page (hit `?`). 

## Installation

Just download, add to path, and arrange to `(require 'speedrect)`.  For users of `use-package`:

```elisp
(use-package speedrect
  :load-path "~/code/emacs/speedrect") ; or wherever
```

## Usage

Start `rectangle-mark-mode` as usual (`C-x SPC`, by default).  Hit `?` to summon a help buffer of available key bindings.

## Hints

A rectangle is just a _region_ (point and mark), specially interpreted.  While marking rectangles, you can `C-x C-x` to switch point and mark to make changes to the top/bottom of the selected region.

## Key Listing

```
SpeedRect Rectangle Mark Mode Commands
======================================================================
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
Calc:
  [#] grab      grab the rectangle as a matrix
  [=] across    sum across rows and grab result
  [+] down      sum down the columns and grab result
Etc:
  [?] help      view this Help buffer
  [q] quit      exit rectangle-mark-mode
```

## Thanks
 Contributors of ideas from [this discussion](https://www.reddit.com/r/emacs/comments/11k9u73/a_tiny_modal_rectanglemarkmode/)
