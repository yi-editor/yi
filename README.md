# Yi

[![Travis](https://travis-ci.org/yi-editor/yi.svg?branch=master)](https://travis-ci.org/yi-editor/yi)
[![Hackage](https://img.shields.io/hackage/v/yi.svg?maxAge=2592000)](https://hackage.haskell.org/package/yi)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/yi.svg?maxAge=2592000)]()

Yi is a collection of packages that serve as building blocks for making your very own text editor.

## Installation

Just running `stack install yi` or `cabal install -j yi` would be akin to unwrapping a box of lego and
finding an assembled spaceship there.
Note that this way, it will not be possible to use a custom configuration.

In order to have a personalized configuration, it is necessary to use Yi as libraries to create your own text editor.

A good starting point is choosing an example configuration of your liking in the
[example-configs][userconfigs] directory, building it, running it, and tinkering with it.

> Note to cabal users, it is necessary to run [hpack](https://hackage.haskell.org/package/hpack) in order to generate a *.cabal* file from the `package.yaml` provided.

#### Static vs dynamic

Yi used to have a dynamic configuration (*à la* Xmonad). So each time after the configuration was changed, Yi needed recompilation before starting.

Now, it's recommended to use a static configuration which makes it possible to distribute binaries without the entire Haskell ecosystem.

*Dynamic configuration is still available using a [separate package](https://hackage.haskell.org/package/yi-dynamic-configuration).*

See this [article](https://yi-editor.github.io/posts/2017-01-06-dyre/) for more detailed explanations about static and dynamic configuration.

### Static example configurations
The static example configurations don't need to be placed in any specific directory (but you can still use `~/.config/yi`).
   1. Copy the example configuration (only the contents of the folder) to your `yi` folder.
   2. Install the configuration (with `stack install` or `hpack; cabal install`).
   *Make sure the folder where stack (or cabal) installs executables is on your PATH.*
   3. Run Yi with the command found in the package.yaml file under the executables line (i.e. `yi-vty-emacs` for the emacs config). You can also change the executable name here.

### Dynamic example configurations
 You can install this just like the static configuration. Make sure to copy the example configuration in the `~/.config/yi` folder. After step 3, you should be able to just use the `yi` command to launch Yi. If you change your configuration file Yi automatically detects your changes and rebuilds itself.

See this [documentation page](https://yi-editor.github.io/pages/installing/)
for more detailed installation instructions. Hacking instructions if you're
using the nix package manager are also there.

## Configuration

Yi, as a library, can be categorized into four parts :

* *Frontends* are responsible for rendering the editor state and passing user events to the core ;
    * a "textual" editor using [yi-frontend-vty](https://hackage.haskell.org/package/yi-frontend-vty)
    * a "graphical" one using [yi-frontend-pango](https://hackage.haskell.org/package/yi-frontend-pango)
    * or both
    
 * *Actions* describe how to interact with the editor, and it's the part that makes most of Yi. It is structured around a stack of three monadic DSLs:
   * **BufferM** for all buffer-local operations, like insertion, deletion of text, and annotation of buffer contents. It can be understood as a monad that encapsulates the state of one buffer.
   * **EditorM** for editor-level operations, e.g., opening and closing windows and buffers. Operations involving *more than one buffer are handled at this level too.*
   * **YiM** for IO-level operations. There, one can operate on files, processes, etc. *This is the only level where IO can be performed.*

 * *Keymaps* represent how to trigger actions, they serve as the basis to use or to create the ones you need;
    * [vim-like](https://hackage.haskell.org/package/yi-keymap-vim)
    * [emacs-like](https://hackage.haskell.org/package/yi-keymap-emacs)
    * [cua-like](https://hackage.haskell.org/package/yi-keymap-cua)
  
* *Glue code*, how the three other parts are shaped together.

#### External configuration examples

Some people share their Yi configurations here on [github](https://github.com/search?utf8=%E2%9C%93&q=yi-config+language%3Ahaskell&type=).

## Documentation

Please visit the [Yi website](https://yi-editor.github.io/).

## Reporting bugs and feature requests

Please report issues (including documentation ones) on [GitHub][issueslist].

## Mailing List

Our mailing list is [yi-devel][], hosted at Google Groups. Please ask us questions on this list! All development discussion occurs on this list.

## IM channels

IRC: #yi@Freenode  Please note that it is rather slow (very slow compared to #haskell), so be prepared to stay for longer than 5 minutes.

Matrix: #yi:matrix.org

## Reading material

There are some papers which might interest you.

* [An editor in Haskell for Haskell][small-yi]

* [Lazy Functional Incremental Parsing][lazy-parsing] in Yi

* [A JavaScript Mode for Yi][js]

* [Robust & Precise incremental parsing of Haskell][precise-haskell]
  talks about the precise Haskell mode.

[github]: https://github.com/yi-editor/
[issueslist]: https://github.com/yi-editor/yi/issues
[yi-devel]: https://groups.google.com/group/yi-devel
[userconfigs]: https://github.com/yi-editor/yi/tree/master/example-configs
[small-yi]: https://publications.lib.chalmers.se/records/fulltext/local_72549.pdf
[lazy-parsing]: https://publications.lib.chalmers.se/records/fulltext/local_94979.pdf
[js]: https://publications.lib.chalmers.se/records/fulltext/112284.pdf
[precise-haskell]: https://publications.lib.chalmers.se/records/fulltext/117337.pdf
