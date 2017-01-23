# Overview

This package emulates [surround.vim by Tim Pope](https://github.com/tpope/vim-surround).
The functionality is wrapped into a minor mode.
To enable it globally, add the following lines to ~/.emacs:

    (require 'evil-surround)
    (global-evil-surround-mode 1)

Alternatively, you can enable surround-mode along a major mode by adding
`turn-on-surround-mode' to the mode hook.

This package uses [Evil](https://bitbucket.org/lyro/evil/) as its vi layer.

## Add surrounding ##
You can surround in visual-state with `S<textobject>` or `gS<textobject>`.
or in normal-state with `ys<textobject>` or `yS<textobject>`.

## Change surrounding ##
You can change a surrounding with `cs<old-textobject><new-textobject>`.

## Delete surrounding ##
You can delete a surrounding with `ds<textobject>`.

## Add new surround pairs ##
A surround pair is this (trigger char with textual left and right strings):

    (?> . ("<" . ">"))

or this (trigger char and calling a function):

    (?< . surround-read-tag)

You can add new by adding them to `evil-surround-pairs-alist`.
For more information do: `C-h v evil-surround-pairs-alist`.

`evil-surround-pairs-alist` is a buffer local variable, which means that you can have
different surround pairs in different modes.
By default `<` is used to insert a tag, in C++ this may not be useful - but
inserting angle brackets is, so you can add this:

    (add-hook 'c++-mode-hook (lambda ()
                               (push '(?< . ("< " . " >")) evil-surround-pairs-alist)))

Don't worry about having two entries for `<` surround will take the first.

Or in Emacs Lisp modes using \` to enter \` ' is quite useful, but not adding a
pair of \` (the default behavior if no entry in `evil-surround-pairs-alist` is
present), so you can do this:

    (add-hook 'emacs-lisp-mode-hook (lambda ()
                                      (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))

without affecting your Markdown surround pairs, where the default is useful.

To change the default `evil-surround-pairs-alist` you have to use `setq-default`, for
example to remove all default pairs:

    (setq-default evil-surround-pairs-alist '())

or to add a pair that surrounds with two ` if you enter ~:

    (setq-default evil-surround-pairs-alist (cons '(?~ . ("``" . "``"))
                                             evil-surround-pairs-alist))

## Add new supported operators ##
You can add support for new operators by adding them to `evil-surround-operator-alist`.
For more information do: `C-h v evil-surround-operator-alist`.

By default, surround works with `evil-change` and `evil-delete`.
To add support for the evil-paredit package, you need to add `evil-paredit-change`
and `evil-paredit-delete` to `evil-surround-operator-alist`, like so:

    (add-to-list 'evil-surround-operator-alist
                 '(evil-paredit-change . change))
    (add-to-list 'evil-surround-operator-alist
                 '(evil-paredit-delete . delete))

## Usage examples ##

Here are some usage examples (taken from
[surround.vim](https://github.com/tpope/vim-surround/blob/master/README.markdown)):

Press `cs"'` inside

    "Hello world!"

to change it to

    'Hello world!'

Now press `cs'<q>` to change it to

    <q>Hello world!</q>

To go full circle, press `cst"` to get

    "Hello world!"

To remove the delimiters entirely, press `ds"`.

    Hello world!

Now with the cursor on "Hello", press `ysiw]` (`iw` is a text object).

    [Hello] world!

Let's make that braces and add some space (use `}` instead of `{` for no
space): `cs]{`

    { Hello } world!

Now wrap the entire line in parentheses with `yssb` or `yss)`.

    ({ Hello } world!)

Revert to the original text: `ds{ds)`

    Hello world!

Emphasize hello: `ysiw<em>`

    <em>Hello</em> world!

Finally, let's try out visual mode. Press a capital V (for linewise
visual mode) followed by `S<p class="important">`.

    <p class="important">
      <em>Hello</em> world!
    </p>

Suppose you want to call a function on your visual selection or a text
object. You can simply press `f` instead of the aforementioned keys
and are then prompted for a functionname in the minibuffer, like with
the tags. So with:

	"Hello world!"

... after selecting the string, then pressing `Sf`, entering `print`
and pressing return you would get

    print("Hello world!")

# FAAQ (frequently actually asked questions)

## Why does `vs` no longer surround?

This is due to an upstream change in `vim-surround`. It happened in this commit: https://github.com/tpope/vim-surround/commit/6f0984a. See the discussion in this pull request for more details: https://github.com/timcharper/evil-surround/pull/48.

LICENSE
---------

[GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)
