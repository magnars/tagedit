# tagedit.el

A collection of paredit-like functions for editing in html-mode.

## Functions

This is it at the moment:

 - `tagedit-forward-slurp-tag` moves the next sibling into this tag.
 - `tagedit-forward-barf-tag` moves the last child out of this tag.

## Setup

I won't presume to know which keys you want these functions bound to,
so you'll have to set that up for yourself. Here's some example code,
which incidentally is what I use:

```cl
(define-key html-mode-map (kbd "s-<right>") 'tagedit-forward-slurp-tag)
(define-key html-mode-map (kbd "s-<left>") 'tagedit-forward-barf-tag)
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

## Todo

Right now the commands only care about tags. Free floating text is
ignored. My guess is that you'd like to slurp and barf that stuff too.

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
