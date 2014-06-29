# Yi

Yi is a text editor written in Haskell and extensible in Haskell. The goal of Yi is to provide a flexible, powerful and correct editor core scriptable in Haskell.

Its features include

* a purely functional editor core;
* keybindings written as parsers of the input;
* Emacs, Vim and Cua (subset) emulations provided by default;
* Vty (terminal) and Gtk-based Pango UIs

The long term goal of the project is to make Yi the editor of choice for the haskell hacker. The main short term goal is to maximize Yi's Fun Factor. This means that we want to

* improve hackability (and therefore architecture) and
* add cool features.

We also want to simplify the core Yi package to make it more accessible, splitting some parts into several packages.

Other information (much of it old) is available on the [Haskell wiki][haskellwiki].

## Contents
* [Installing](#installing)
  * [Installing inside a Cabal sandbox](#installing-inside-a-cabal-sandbox)
* [Getting Source](#getting-source)
* [Reporting Bugs](#reporting-bugs)
* [Mailing List](#mailing-list)
* [IRC channel](#irc-channel)
* [Configuring Yi](#configuring-yi)
* [Frontend Compatibility](#frontend-compatibility)
* [Profiling](#profiling)
* [Reading material](#reading-material)

## Installing

Yi requires the Haskell Platform 2011.2.0.0 at minimum (for GHC 7, alex, and cabal-install, among other things).

With the Haskell Platform installed, yi should be installed with cabal-install:

    $ cabal update
    $ cabal install yi

On Linux systems, you'll probably need ncurses development headers for the Vty frontend. On Ubuntu, you'll need to install the `libncurses5-dev` package.

You can specify frontends to compile, also:

    $ cabal install yi -fvty -fpango

Options are `-fvty` and `-fpango`.

You can also install the `yi-contrib` package, which contains some extra contributed things (like user configs):

    $ cabal install yi-contrib

If you're in the source repository, you can install yi from source:

    $ cabal update # Still update to get updated dependencies
    $ (cd yi && cabal install)

And the contrib package:

    $ (cd yi-contrib && cabal install)

If you're getting errors about Alex version bounds or are experiencing
similar problems, it's recommended that you install from the sources
available in the GitHub repository which has the version bounds
adjusted and contains a couple of nice fixes that might not be present
in the latest Hackage version.

### Installing inside a Cabal sandbox

Many people want to install Yi inside a cabal sandbox (cabal-install
1.18 feature). This is especially important if you plan on hacking on
Yi itself or on libraries for Yi.

As Yi compiles your config file once you start it, the config needs to
know where to look for any of its dependencies, such as Yi itself! If
these are inside of the sandbox, it doesn't know where to look and
you'll get config compilation errors due to missing modules.

To sandbox, navigate to your source yi directory. For me it's
`~/programming/yi/yi`.

Once there, run `cabal sandbox init` as you normally would, then
`cabal install`. Now check your `.cabal-sandbox` directory. You should
see a bunch of files but we're looking for the directory storing all
the sandboxed packages. For me, it's
`i386-linux-ghc-7.6.3-packages.conf.d`. If you `cabal install` with
different versions of the compiler, you might have more than one of
these directories. I also have
`i386-linux-ghc-7.9.20140129-packages.conf.d`. This is important if
you want to work on Yi with multiple compiler versions but most people
will only have a single such directory.

Next, we need to somehow tell Yi/GHC where to look for these packages
when compiling our config. Thankfully, a `GHC_PACKAGE_PATH` environment
variable can do just that, in combination with passing the `-package-db` option
through to ghc. Here's mine, called `runyi` that I put in my `$PATH` for easy
access:

```
#!/bin/bash
SANDBOX_DB=$HOME/programming/yi/yi/.cabal-sandbox/i386-linux-ghc-7.6.3-packages.conf.d
export GHC_PACKAGE_PATH=$SANDBOX_DB:/usr/lib/ghc-7.6.3/package.conf.d
$HOME/programming/yi/yi/.cabal-sandbox/bin/yi --ghc-option="-package-db $SANDBOX_DB" "$@"
```

What it does is it sets `GHC_PACKAGE_PATH` to the directory we have
found inside `.cabal-sandbox` earlier and then runs the `yi` binary
which is also in the sandbox. The `"$@"` part means that all the
arguments we pass to this script are passed on to the Yi binary which
means we can still use all the regular flags, such as `runyi
--as=emacs`. Of course, you'll need to adjust the paths used to match
your sandbox and package directories. Pick the version that your
compiler is going to be when running Yi.

Note that cabal sandboxes still inherit some packages (such as `base`) from the
global package database, so it is necessary to include the global package
(`/usr/lib/ghc-7.6.3/package.conf.d` in the example above) directory in
`GHC_PACKAGE_PATH`. The path may vary per system, and can be found with a
command like:

```
find / -name package.conf.d 2>/dev/null
```

There's one more thing to mention in this section and that is config
dependencies. One of the great things about Yi is that we have access
to the wealth of existing Haskell libraries and we can take advantage
of this in our config file. There are two scenarios:

If the package your config depends on is on Hackage and you want to
use that, just use `cabal install` in the sandboxed Yi directory. So
if your config depends on `semigroups`, you'd run `cabal install
semigroups`. After doing this, `semigroups` should now be visible when
your config is getting compiled.

If the package your config depends on is local, for example when
you're developing the library that you want to use or if you need a
patched version, you'll have to use `cabal sandbox add-source`
command. As an example, I'm developing a `yi-haskell-utils` package
and my config depends on it. To accommodate for this, I ran `cabal
sandbox add-source ~/programming/yi-haskell-utils`.

I suspect that it'd be perfectly possible to make your config file
into a cabal project and manage the dependencies that way but I have
not yet investigated this approach.

## Getting Source

Yi source repository is available on [GitHub][github].

To get the git version,

    $ git clone git://github.com/yi-editor/yi.git

(There may be more repositories in the future, as yi is split more.)

## Reporting Bugs

Please report issues on [GitHub][issueslist].

## Mailing List

Our mailing list is [yi-devel][], hosted at Google Groups. Please ask us questions on this list! All development discussion occurs on this list.

## IRC channel

Our channel is #yi at Freenode. Please note that it is rather slow (very slow compared to #haskell), so be prepared to stay for longer than 5 minutes.

## Configuring Yi

Yi uses the [Dyre][dyre] package to have dynamic reconfiguration. You can configure Yi by creating `~/.config/yi/yi.hs`, and then Yi is reconfigured whenever you update this file. Example configuration files are in `yi/examples/` (copy any of these into `~/.config/yi/` as `yi.hs` and restart Yi).

You can also use the sample user configs in the `yi-contrib` package (see the [list of user configs][userconfigs] in the source repository on GitHub). To use one of these configurations, install the package and then create a configuration file `~/.config/yi/yi.hs` like this:

    import Yi
    import Yi.Config.Users.Anders

    main = yi config

It's possible to customize even these user configs in the same way as the example configurations.

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

## Reading material

There are some papers which might interest you. If you plan on hacking
on Yi, it's very recommended that you read these

* [An editor in Haskell for Haskell][small-yi]

* [Lazy Functional Incremental Parsing][lazy-parsing] in Yi

* [A JavaScript Mode for Yi][js]

* [Robust & Precise incremental parsing of Haskell][precise-haskell]
  talks about the precise Haskell mode.


[haskellwiki]: http://haskell.org/haskellwiki/Yi
[github]: https://github.com/yi-editor/
[issueslist]: https://github.com/yi-editor/yi/issues
[yi-devel]: http://groups.google.com/group/yi-devel
[dyre]: http://hackage.haskell.org/package/dyre
[userconfigs]: https://github.com/yi-editor/yi/tree/master/yi-contrib/src/Yi/Config/Users
[profiling-discussion]: https://groups.google.com/forum/?fromgroups=#!topic/yi-devel/2dUXKJMSFsM
[small-yi]: http://publications.lib.chalmers.se/records/fulltext/local_72549.pdf
[lazy-parsing]: http://publications.lib.chalmers.se/records/fulltext/local_94979.pdf
[js]: http://publications.lib.chalmers.se/records/fulltext/112284.pdf
[precise-haskell]: http://publications.lib.chalmers.se/records/fulltext/117337.pdf
