
This plugin aims to bring some awesomeness of
fuzzy openers like emacs-helm and ctrlp.vim to yi.


Setup
=====

This plugin provides a single function **fuzzyOpen**.

Refer to yi's example config for your preferred keymap
to see how to bind keys to actions.

For emacs and cua it will probably look like this::

  (ctrlCh 'p' ?>>! fuzzyOpen)

And for vim keymap something like this::

  V.mkStringBindingY V.Normal ("<C-p>", fuzzyOpen, id)

Usage
=====

<C-p> (or whatever mapping user chooses) starts fuzzy open dialog.

Typing something filters filelist.

<Esc> and <C-g> cancel the dialog.

<Enter> opens currently selected file
in current (one that fuzzyOpen was initiated from) window.

<C-t> opens currently selected file in a new tab.
<C-s> opens currently selected file in a split.

<KUp> and <C-p> move selection up
<KDown> and <C-n> move selection down

Readline shortcuts <C-a> , <C-e>, <C-u> and <C-k> work as usual.

Support
=======

Don't hesitate to file an issue on github or come by #yi@freenode.
