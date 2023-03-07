# SpeedRect
Quick key bindings and other tools for Emacs' `rectangle-mark-mode`.

SpeedRect is a small Emacs package that provides convenient "modal" key bindings and other tools while in `rectangle-mark-mode` (`C-x SPC`, by default).

## Features

- Bindings for all the basic rectangle functions: open, insert string, kill, delete, delete whitespace, clear, copy, yank.
- Restore the last saved rectangle position, or start a new rectangle from point. 
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
