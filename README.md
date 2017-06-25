# Yi

[![Join the chat at https://gitter.im/yi-editor/yi](https://badges.gitter.im/yi-editor/yi.svg)](https://gitter.im/yi-editor/yi?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Travis](https://travis-ci.org/yi-editor/yi.svg?branch=master)](https://travis-ci.org/yi-editor/yi)
[![Hackage](https://img.shields.io/hackage/v/yi.svg?maxAge=2592000)](https://hackage.haskell.org/package/yi)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/yi.svg?maxAge=2592000)]()
[![yi on Stackage LTS 7](https://stackage.org/package/yi/badge/lts-7)](https://stackage.org/lts-7/package/yi)
[![yi on Stackage Nightly](https://stackage.org/package/yi/badge/nightly)](https://stackage.org/nightly/package/yi)

Yi is a collection of packages that serve as building blocks for making your very own text editor.

## Installing

Just doing `stack install yi` would be akin to unwrapping a box of legos and
finding an assembled spaceship there.

A good starting point is choosing an example config of your liking in the
[example-configs][userconfigs] directory, building it, running it, and tinkering with it.

See [this documentation page](https://yi-editor.github.io/pages/installing/)
for more detailed installation instructions. Hacking instructions if you're
using the nix package manager are also there.

## Getting Source

Yi source repository is available on [GitHub][github].

## Documentation

Please visit the [Yi website](https://yi-editor.github.io/).

## Reporting bugs and feature requests

Please report issues (including documentation ones) on [GitHub][issueslist].

## Mailing List

Our mailing list is [yi-devel][], hosted at Google Groups. Please ask us questions on this list! All development discussion occurs on this list.

## IM channels

IRC: #yi@Freenode  Please note that it is rather slow (very slow compared to #haskell), so be prepared to stay for longer than 5 minutes.

Gitter: [yi-editor/yi](https://gitter.im/yi-editor/yi)

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
