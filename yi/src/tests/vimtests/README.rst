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

Event notation
--------------

  Event parsing expects a subset of vim notation (see :help <>). Backslash escaping is not supported. So to enter left angle bracket one must write <lt> and not \<.

Vim features not supported intentionally

Intentionally not supported features of Vim
-------------------------------------------

  * select mode
  * folds

Features incompatible with Vim because why not
----------------------------------------------

  * Y yanks to EOL
  * A and I in linewise visual mode behave like in blockwise visual mode
  * 999rZ turns ABC into ZZZ instead of doing nothing