# tagedit.el

A collection of paredit-like functions for editing in html-mode.

## Installation

I highly recommended installing tagedit through elpa.

It's available on [marmalade](http://marmalade-repo.org/) and
[melpa](http://melpa.milkbox.net/):

    M-x package-install tagedit

You can also install the dependencies on your own, and just dump
tagedit in your path somewhere:

 - <a href="https://github.com/magnars/s.el">s.el</a>
 - <a href="https://github.com/magnars/dash.el">dash.el</a>

## Functions

This is it at the moment:

 - `tagedit-forward-slurp-tag` moves the next sibling into this tag.
 - `tagedit-forward-barf-tag` moves the last child out of this tag.
 - `tagedit-raise-tag` replaces the parent tag with this tag.
 - `tagedit-kill-attribute` kills the html attribute at point.

## Setup

If you want tagedit to bind to the same keys as paredit, there's this:

```cl
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)))
```

Or you can cherry-pick functions and bind them however you want:

```cl
(define-key html-mode-map (kbd "C-<right>") 'tagedit-forward-slurp-tag)
(define-key html-mode-map (kbd "C-<left>") 'tagedit-forward-barf-tag)
(define-key html-mode-map (kbd "M-r") 'tagedit-raise-tag)
(define-key html-mode-map (kbd "s-k") 'tagedit-kill-attribute)
```

## Other conveniences

It also expands one-line tags into multi-line tags for you, when you
press refill-paragraph. Like this:

```html
<p>My one very long text inside a tag that I'd like to refill</p>
```

then after `M-q`:

```html
<p>
  My one very long text inside a tag that
  I'd like to refill
</p>
```

You can disable this behavior by setting
`tagedit-expand-one-line-tags` to nil.

## License

Copyright (C) 2012 Magnar Sveen

Author: Magnar Sveen <magnars@gmail.com>
Keywords: convenience

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
