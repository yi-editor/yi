# Yi

Yi is a text editor written in Haskell and extensible in Haskell. The goal of Yi is to provide a flexible, powerful and correct editor core scriptable in Haskell.

Its features include

* a purely functional editor core;
* keybindings written as parsers of the input;
* Emacs, Vim and Cua (subset) emulations provided by default;
* Vty, Gtk2Hs, and, in development, Cocoa and Pango frontends.

The long term goal of the project is to make Yi the editor of choice for the haskell hacker. The main short term goal is to maximize Yi's Fun Factor. This means that we want to

* improve hackability (and therefore architecture) and
* add cool features.

We also want to simplify the core Yi package to make it more accessible, splitting some parts into several packages.

Other information (much of it old) is available on the [Haskell wiki][haskellwiki].

## Installing

Yi requires the Haskell Platform 2011.2.0.0 at minimum (for GHC 7, alex, and cabal-install, among other things).

With the Haskell Platform installed, yi should be installed with cabal-install:

    $ cabal update
    $ cabal install yi

If you're in the source repository, you can install yi from source:

    $ cabal update # Still update to get updated dependencies
    $ cabal install

## Getting Source

Yi has mirrored source repositories on [GitHub][github] and [Google Code][googlecode].

To get the git version,

    $ git clone git://github.com/yi-editor/yi.git

(There may be more repositories in the future, as yi is split more.)

## Reporting Bugs

We have a [Google Code][googlecode] that hosts our issues list. Please report issues at the [issues list][issueslist] hosted there.

## Mailing List

Our mailing list is [yi-devel][], hosted at Google Groups. Please ask us questions on this list! All development discussion occurs on this list.

## Configuring Yi

Yi uses the [Dyre][dyre] package to have dynamic reconfiguration. You can configure Yi by creating `~/.yi/yi.hs`, and then Yi is reconfigured whenever you update this file. Example configuration files are in `yi/examples/` (copy any of these into `~/yi/` as `yi.hs` and restart Yi).

You can also use the sample user configs in the _yi-contrib_ package (see the [list of user configs][userconfigs] in the source repository on GitHub). To use one of these configurations, install the package and then create a configuration file `~/.yi/yi.hs` like this:

    import Yi
    import Yi.Config.Users.Anders

    main = yi config

It's possible to customize even these user configs in the same way as the example configurations.

## Frontend Compatibility

            |  Vty    Pango  Cocoa
    --------+----------------------
    Linux   |   X       X
    OSX     |   X       X      X
    Windows |           X

Windows support for Vty may eventually come; patches on the vty package would certainly be appreciated.

The plan is to move the UI frontends into separate packages, but this has not yet happened.

## Hacking

You can run Yi without installing by running from the `dist` directory. There's a `-fhacking` flag in the Yi package to compile without dynamic reconfiguration. When used, it'll statically compile a `yi` binary using the file `HackerMain.hs` in the source repository (so you'll need to put a configuration file there before trying to compile with `-fhacking`).

    $ cp ~/.yi/yi.hs HackerMain.hs
    $ cabal configure -fhacking
    $ cabal build
    $ ./dist/build/yi/yi

There's also a Makefile build script to do the same thing:

    $ cp ~/.yi/yi.hs HackerMain.hs
    $ make run-inplace

## Profiling

If you're interested in optimizing Yi, the Makefile contains two example rules for profiling called `prof-config-hacking` and `prof-config`, depending on whether you want to use `-fhacking`:

    make prof-config-hacking
    make run-inplace

Without `-fhacking`:

    make prof-config
    make install

Of course, feel free to adjust those rules to your needs. If you go for the prof-config variant, here's an example of how you then can run yi to get profiling output:

    yi --force-recompile --ghc-options="-prof -auto-all" +RTS -p -hc

[haskellwiki]: http://haskell.org/haskellwiki/Yi
[github]: https://github.com/yi-editor/
[googlecode]: http://code.google.com/p/yi-editor/
[issueslist]: http://code.google.com/p/yi-editor/issues/list
[yi-devel]: http://groups.google.com/group/yi-devel
[dyre]: http://hackage.haskell.org/package/dyre
[userconfigs]: https://github.com/yi-editor/yi/tree/master/yi-contrib/src/Yi/Config/Users
