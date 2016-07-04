Testing Vim bindings
====================

Single file test format
-----------------------

  Files with names like "foo.test" are treated as single file tests.

::

  -- Input <-- This marks beginning of initial buffer state
  (1,1)    <-- This is initial cursor position, (row, column), both 1-based
  aaaaa    <-- Here goes actual buffer content
  b b b
  ccccc
  -- Output <-- This marks beginning of expected buffer state after test
  (3,5)     <-- This is expected cursor position
  aaaaa     <-- Expected buffer text
  b bfoo b
  ccccc
  -- Events <-- This line starts event sequence that is fed to editor after loading initial state
  2wifoo
  <Esc>jj

Directory test format
---------------------

  Directories that contain files "input", "output" and "events" and nothing else are considered directory tests. These three files have the same format as the sections of single file test described above.

Test file naming note
---------------------

Originally we've used names like "ddP.test" and "ddp.test". It turned out to be really inconvenient on OSX, which uses case-insensitive filesystem by default. We had to rename tests to avoid collisions: "ddP.test" became "dd_capP.test".

Event notation
--------------

  Event parsing expects a subset of vim notation (see :help <>). Backslash escaping is not supported. So to enter left angle bracket one must write <lt> and not \<.

Intentionally not supported features of Vim
-------------------------------------------

  * select mode
  * folds
  * 0<C-d> and ^<C-d> in insert mode
  * After i_<C-o> only motions are allowed

Features incompatible with Vim because why not
----------------------------------------------

  * Y yanks to EOL
  * A and I in linewise visual mode behave like in blockwise visual mode
  * 999rZ turns ABC into ZZZ instead of doing nothing
  * Operators in visual mode always leave cursor at selection start. Vim doesn't do this in some cases and I don't understand the pattern.
  * Paragraph text object is slightly different
  * Repeating insert actions with dot works differently when insertion events contain oneshot normal commands, e.g. "ifoo<C-o>hbar<Esc>". In Vim dot would insert only "bar", but yi dot inserts "fobaro"
  * Scrolling motions (<C-f>, PageUp, etc) are treated like linewise motions.
  * <C-w>, <C-u> remove whole region, not only entered characters.

