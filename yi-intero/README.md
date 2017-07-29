# yi-intero

A Yi plugin for intero support.

## installing

To use this library add it as a dependency to your Yi configuration and bind the exposed `Action`s to some keys [or vim ex commands like I did](https://github.com/noughtmare/yi-config/blob/c4ef0d0a92ff87394f5d2aee74e736b0a32ec478/Main.hs#L94-L125).

## usage

Before you can use any intero functions you have to start the background process with the `interoStart` action. This will run `stack ghci --with-ghc intero` in the background.

Currently only these `Action`s are exposed:

  - `interoEval` for sending a raw string to the intero prompt.
  - `interoLocAt` determines the location of the definition of the word under the cursor.
  - `interoUses` finds the usage location of the word under the cursor or the definition location if not using this on a definition.
  - `interoTypeAt` returns the type of the word under the cursor.

Every `Action` results in a split window in which the result of the query are displayed.

## development

yi-intero is still in an early stage so there is a lot to do:

  - [ ] Default keybindgins for vim and emacs (and cua), this might need to be done in a separate package.
  - [ ] Better integration of results. Results should not always just be displayed in a split window.
  - [ ] Tab completion and automatic completion.
  - [ ] Jump to definition/uses.
  - [ ] Automatic installation of Intero in stack projects.
  - [ ] On the fly type checking. (probably first requires https://github.com/yi-editor/yi/issues/896)
  - [ ] Type of selection. Right now only type of current word is implemented.

See https://commercialhaskell.github.io/intero/ for more ideas and information.

