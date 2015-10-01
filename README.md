# Yi [![Build Status](https://travis-ci.org/yi-editor/yi.svg?branch=master)](https://travis-ci.org/yi-editor/yi)

Yi is a text editor written in Haskell and extensible in Haskell. The goal of Yi is to provide a flexible, powerful and correct editor core scriptable in Haskell.

Its features include

* a purely functional editor core;
* keybindings written as parsers of the input;
* Emacs, Vim and Cua (subset) emulations(keymap) provided by default;
* Vty (terminal) and Gtk-based Pango UIs

The long term goal of the project is to make Yi the editor of choice for the haskell hacker. The main short term goal is to maximize Yi's Fun Factor. This means that we want to

* improve hackability (and therefore architecture) and
* add cool features.

We also want to simplify the core Yi package to make it more accessible, splitting some parts into several packages.

# Getting Started
## Installing

If you have cabal installed with ghc>7.8,just run
    
    cabal update
    cabal install yi
    
See [this documentation page](http://yi-editor.github.io/pages/installing/)
for installation instructions(Including Nix).

## Make your own Yi
Yi with indendation mode:

    import Yi
    import qualified Yi.Rope as R
    import           Yi.Keymap.Emacs as Emacs
    import           Yi.String (mapLines)
    
    increaseIndent :: BufferM ()
    increaseIndent = do
       r <- getSelectRegionB
       rf <- unitWiseRegion Line r -- extend the region to full lines.
       modifyRegionB (mapLines (\x->R.append (R.fromString "     ") x)) rf
       --modifyRegionB (mapLines (R.cons ' ')) rf
    
    
    main :: IO ()
    main = yi $ defaultEmacsConfig 
           { defaultKm = Emacs.mkKeymap $
                         override Emacs.defKeymap $
                         \parent _ -> parent {_eKeymap = (_eKeymap parent) ||> (char '\t' ?>>! increaseIndent)}        
                          --bind Tab(char 't') to increaseindent
      }
    
Unlike the usual approach to customize editor like emacs and vim,which use a separate file like .emacs,we develop our own editor by utilize Yi as a library.You can look up the functions @ http://hayoo.fh-wedel.de

You can find some [sample user configs][userconfigs] in the source repository on GitHub. To use one of these configurations, install the package and then create a configuration file `~/.config/yi/yi.hs` like this:

It's possible to customize even these user configs in the same way as the example configurations.

### dynamic reconfiguration
Yi uses the [Dyre][dyre] package to have dynamic reconfiguration. You can configure Yi by creating `~/.config/yi/yi.hs`, and then *Yi is reconfigured whenever you update this file*. Example configuration files are in `yi/examples/` (copy any of these into `~/.config/yi/` as `yi.hs` and restart Yi).

# More Info
## Getting Source

Yi source repository is available on [GitHub][github].

To get the latest version,

    $ git clone git://github.com/yi-editor/yi.git

If you plan to do more serious hacking, you probably want the
supporting repositories from the
[GitHub project page][github]. You should
cross-reference with the cabal file to see what you might need.

## Frontend Compatibility

            |  Vty    Pango
    --------+----------------
    Linux   |   X       X
    OSX     |   X       X
    Windows |           X

Windows support for Vty may eventually come; patches on the vty package would certainly be appreciated.

The plan is to move the UI frontends into separate packages, but this has not yet happened.

## Profiling

If you're interested in optimizing Yi, here is a [way to get profiling][profiling-discussion]:

1. Change ghcOptions in `yi/src/library/Yi/Main.hs`:
```
-                  ghcOptions = [],
+                  ghcOptions = ["-auto-all", "-prof", "-osuf=p_o", "-hisuf=p_hi", "-rtsopts"],
```
2. Recompile yi with `--enable-library-profiling`:
```
cabal configure --enable-library-profiling && cabal install --reinstall
```
3. Run `yi` first to get a compiled **real** executable.

4. Then call **real** executable from cache directory with profiling options. On any XDG-compatible (Unix-like) system this should look like:
```
~/.cache/yi/yi-linux-x86_64 +RTS -Pa
```

## Documentation

[hackage]: https://hackage.haskell.org/package/yi

There are some papers which might interest you. If you plan on hacking on Yi, it's very recommended that you read these

* [An editor in Haskell for Haskell][small-yi] : introduce the fundamental principles and structure of Yi

* [Lazy Functional Incremental Parsing][lazy-parsing] in Yi

* [A JavaScript Mode for Yi][js]

* [Robust & Precise incremental parsing of Haskell][precise-haskell]
  talks about the precise Haskell mode.


[haskellwiki]: http://haskell.org/haskellwiki/Yi
[github]: https://github.com/yi-editor/
[issueslist]: https://github.com/yi-editor/yi/issues
[yi-devel]: http://groups.google.com/group/yi-devel
[dyre]: http://hackage.haskell.org/package/dyre
[userconfigs]: https://github.com/yi-editor/yi/tree/master/example-configs
[profiling-discussion]: https://groups.google.com/forum/?fromgroups=#!topic/yi-devel/2dUXKJMSFsM
[small-yi]: http://publications.lib.chalmers.se/records/fulltext/local_72549.pdf
[lazy-parsing]: http://publications.lib.chalmers.se/records/fulltext/local_94979.pdf
[js]: http://publications.lib.chalmers.se/records/fulltext/112284.pdf
[precise-haskell]: http://publications.lib.chalmers.se/records/fulltext/117337.pdf

Other information (outdated) is available on the [Haskell wiki][haskellwiki].

## Reporting Bugs

Please report issues on [GitHub][issueslist].

## Mailing List

Our mailing list is [yi-devel][], hosted at Google Groups. Please ask us questions on this list! All development discussion occurs on this list.

## IRC channel

Our channel is #yi at Freenode. Please note that it is rather slow (very slow compared to #haskell), so be prepared to stay for longer than 5 minutes.

