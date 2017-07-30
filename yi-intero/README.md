# yi-intero

A Yi plugin for intero support.

## installing

To use this library add it as a dependency to your Yi configuration and bind the exposed actions to some keys [or vim ex commands like I did](https://github.com/noughtmare/yi-config/blob/c4ef0d0a92ff87394f5d2aee74e736b0a32ec478/Main.hs#L94-L125).

## usage

Before you can use any intero functions you have to start the background process with the `interoStart` action. This will run `stack build intero` followed by `stack ghci --with-ghc intero` in the background (the editor will freeze temporarily).

Currently only these actions are exposed:

  - `interoEval` for sending a raw string to the intero prompt.
  - `interoLocAt` determines the location of the definition of the word under the cursor.
  - `interoUses` finds the usage locations of the word under the cursor. This opens a new buffer in which a list of choices are displayed. The user can then press enter to choose one and jump to the use. The user can also press escape to return to the original buffer.
  - `interoTypeAt` returns the type of the word under the cursor.
  - `interoJump` to jump to the definition of the word under the cursor.
  - `interoModule <Some.Module.Name>` to jump to the given module.

`interoEval`, `interoLocAt`, and `interoTypeAt` display their results in a split window as a raw string.

## development

yi-intero is still in an early stage so there is a lot to do. The difficulties are my own
quick guesses so don't feel discouraged if something turns out to be very difficult.

You can also look at [the github issue tracker and filter on the plugin-intero label](https://github.com/yi-editor/yi/issues?q=is%3Aopen+is%3Aissue+label%3Aplugin-intero) to find github issues where you can discuss most of the following points.

### Keybindings

Difficulty: Easy

Provide default keybindings for vim, emacs and cua. This can initially be done is separate
packages (like `yi-intero-vim`, `yi-intero-emacs` and `yi-intero-cua`), but I think we
should think of a solution that doesn't require a ton of packages that only contain
keybindings.

### Completion

Difficulty: Easy->Medium->Hard (more functionality is more difficulty)

Completion should be possible. First a command can be exposed that returns a list
of possible completions. That would be the easiest. Then we can bind it to a key that cycles
through all possibilities. Then we could add a dropdown menu to the vty and pango ([I tried
this some time ago for vty](https://github.com/noughtmare/yi/commit/45848b06601a49d623eab29dde58101a5322a4f0))
implementations that shows all possible completions. Autocompletion should then be pretty
easy if it is easy to run a command after every keystroke that inserts a character.

We should make sure that the completion is very configurable.

> ### Jump to definition/uses
>
> Difficulty: Easy (for jump to definition) and Medium (for jump to uses)
>
> This should also be pretty straightforward. The jump to uses should in my opinion be done like I explained in the [Integration of results](#integration-of-results) section.
>
> Jump to definition should be pretty easy: convert the output of intero to a location and a filename and then open the file and go to the given position. It should be checked that the file is actually accessible on disk.

This is now done!

### Automatic installation of Intero

Difficulty: Easy

> Intero needs to be installed in the current stack project, installing intero globally can lead
> to problems as explained in the [TOOLING.md file of Intero](https://github.com/commercialhaskell/intero/blob/28271d50ca65c460cd0983cea13a2c4509b95583/TOOLING.md#installing). We can just run `stack build intero` everytime `interoStart` is used. That should be sufficient.

This is done! Right now the editor just freezes while installing intero and opening the project, when running `interoStart`. This should be changed so that a loading screen is shown in which the output of the currently running commands (like "stack build intero" and "stack ghci --with-ghc intero") is shown.

### Type checking and error reporting

Difficulty: Hard

Intero automatically reports errors and warnings for each module it loads, so we can use those
to give instant feedback. This probably requires a good warning system [like proposed here](https://github.com/yi-editor/yi/issues/896).

### Allow selections to be used as input

Difficulty: Easy

Currently only the word under the cursor is given as input to the intero commands, but if the
user has selected a region that region should be used instead.

### More ideas

See https://commercialhaskell.github.io/intero/ for the reference implementation in emacs.

