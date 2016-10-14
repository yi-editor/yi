
.. image:: https://travis-ci.org/yi-editor/yi-snippet.svg?branch=master
    :target: https://travis-ci.org/yi-editor/yi-snippet

This plugin provides snippet editing.

Setup
=====

You need to do two things: make a snippet collection and bind snippet expansion
action to some input.

Here's a minimum viable snippet collection just for now::

  mySnippets = [Snippet "text" (lit "import qualified Data.Text as T")]

Emacs and cua
-------------

Binding the action would probably look like this::

  (ctrlCh 's' ?>>! expandSnippetE (pure ()) mySnippets)

Vim
---

For vim keymap it's slightly more convoluted::

  imapE "<C-s>"
    (expandSnippetE (defEval "<Esc>") mySnippets)

where::

  import qualified Yi.Keymap.Vim as V

  imapE x y = V.VimBindingE (\evs state -> case V.vsMode state of
                              V.Insert _ ->
                                  fmap (const (y >> return V.Drop))
                                       (evs `V.matchesString` x)
                              _ -> V.NoMatch)

If you want `<Tab>` to be responsible for both snippet expansion and inserting a (possibly expanded) tab when there's no snippet to be expanded, you can use this::

  imapY "<Tab>"
    (withEditor $ do
        let defEval = V.pureEval (extractValue V.defVimConfig)
        expanded <- Snippet.expandSnippetE (defEval "<Esc>") mySnippets 
        when (not expanded) (defEval "<Tab>"))

Usage
=====

This is pretty straightforward and should be familiar from other editors.

Press <Tab> to go to the next placeholder.
Press <Esc> to end snippet editing.
Type stuff to type stuff.

Writing snippets
================

`Snippet` record has two fields: a trigger and a body. 

The body can be written using `do` notation::

  Snippet "lp" $ do
    lit "{-# LANGUAGE "
    _ <- place "OverloadedStrings"
    lit " #-}"

Avaliable primitives are:

lit <text>
  just a piece of text

place <text>
  placeholder with default value <text>

refer <placeholder>
  refer to current value of <placeholder>

finish
  mark a place where cursor must end up after visiting all placeholders

And there are some convenience functions:

nl
  a newline, same as `lit "\n"`

line <text>
  same as `do lit <text>; nl`

mirror <placeholder>
  same as `do value <- refer <placeholder>; lit value`

Predefined variables
--------------------

Currently there's just one: `filename`. Here is how you can use it to write a
snippet for haskell module header::

  import Data.Char (isUpper)
  import qualified Data.Text as T
  import System.FilePath (dropExtension)
  import qualified Yi.Rope as R

  let moduleHeaderSnippet =
          Snippet "m" $ do
              moduleName <- guessModuleName <$> refer filename
              line ("module " <> moduleName)
              lit "    (" >> finish >> nl
              lit "    ) where"

      guessModuleName :: R.YiString -> R.YiString
      guessModuleName =
          R.fromText . T.intercalate "."
              . reverse . takeWhile isCapitalized . reverse
              . T.splitOn "/"
              . T.pack . dropExtension . T.unpack
              . R.toText
          where
          isCapitalized s = case T.uncons s of
              Just (c, _) -> isUpper c
              Nothing -> False

Example snippets
----------------

Some examples are available in examples/MySnippets.hs

When all else fails
===================

Please don't hesitate to complain, request features or send patches at https://github.com/yi-editor/yi-snippet.

Our irc channel is #yi@freenode.