This package emulates [surround.vim by Tim Pope](https://github.com/tpope/vim-surround).
The functionality is wrapped into a minor mode.
To enable it globally, add the following lines to ~/.emacs:

    (require 'surround)
    (global-surround-mode 1)

Alternatively, you can enable surround-mode along a major mode by adding
`turn-on-surround-mode' to the mode hook.

This package uses [Evil](surround.vim by Tim Pope) as its vi layer.

## Add surrounding ##
You can surround in visual-state with `s<textobject><trigger>`
or in normal-state with `ys<textobject><trigger>`.

## Change surrounding ##
You can change a surrounding with `cs<old-trigger><new-trigger>`.

## Delete surrounding ##
You can delete a surrounding with `cd<old-trigger><new-trigger>`.

## Add new surround pairs ##
A surround pair is this (trigger char with textual left and right strings):

    (?> . ("<" . ">"))

or this (trigger char and calling a function):

    (?< . surround-read-tag)

You can add new by adding them to `surround-pairs-alist`.
For more information do: `C-h v surround-pairs-alist`.

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

TODO: Behaves different in evil(-surround).
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
