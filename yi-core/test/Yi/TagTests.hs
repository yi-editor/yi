{-# LANGUAGE OverloadedStrings #-}
module Yi.TagTests (testSuite) where

import Test.Tasty
import Test.Tasty.HUnit

import Yi.Tag
import qualified Yi.CompletionTree as CT

import Data.List (sort)
import Data.Map (keys)

testSuite :: TestTree
testSuite = testGroup "Tag" [unitTests]

exampleTagTable :: TagTable
exampleTagTable = TagTable 
  { tagFileName = "exampleFileName"
  , tagBaseDir = "."
  , tagFileMap = cts
  , tagCompletionTree = CT.fromList . map (_unTag) $ keys cts
  }
  where 
    cts = readCTags $ 
      "!_TAG_FILE_SORTED        1        ~\n\
      \Tag               src/Yi/Tag.hs        73;\"        t\n\
      \Tag               src/Yi/Tag.hs        73;\"        C\n\
      \Tag               src/Yi/Tag.hs        20;\"        m\n\
      \TagTable          src/Yi/Tag.hs        82;\"        t\n\
      \TagTable          src/Yi/Tag.hs        82;\"        C\n\
      \Tags              src/Yi/Tag.hs        66;\"        t\n\
      \Tags              src/Yi/Tag.hs        66;\"        C\n\
      \TagsFileList      src/Yi/Tag.hs        53;\"        t\n\
      \TagsFileList      src/Yi/Tag.hs        53;\"        C\n\
      \_unTag            src/Yi/Tag.hs        73;\"        f\n\
      \_unTagsFileList   src/Yi/Tag.hs        53;\"        f\n\
      \completeTag       src/Yi/Tag.hs        134;\"       f\n\
      \getTags           src/Yi/Tag.hs        151;\"       f\n\
      \hintTags          src/Yi/Tag.hs        127;\"       f\n\
      \importTagTable    src/Yi/Tag.hs        115;\"       f\n\
      \lookupTag         src/Yi/Tag.hs        97;\"        f\n\
      \readCTags         src/Yi/Tag.hs        104;\"       f\n\
      \resetTags         src/Yi/Tag.hs        147;\"       f\n\
      \setTags           src/Yi/Tag.hs        143;\"       f\n\
      \tagBaseDir        src/Yi/Tag.hs        86;\"        f\n\
      \tagCompletionTree src/Yi/Tag.hs        91;\"        f\n\
      \tagFileMap        src/Yi/Tag.hs        89;\"        f\n\
      \tagFileName       src/Yi/Tag.hs        83;\"        f\n\
      \tagsFileList      src/Yi/Tag.hs        63;\"        f\n\
      \unTag'            src/Yi/Tag.hs        75;\"        f"

unitTests :: TestTree
unitTests = testGroup "unit tests"
  [ testGroup "lookupTag" $ 
      [ testCase "finds a tag in a taglist" $
          lookupTag (Tag "Tag") exampleTagTable @?= sort [("./src/Yi/Tag.hs", 73),("./src/Yi/Tag.hs",73),("./src/Yi/Tag.hs",20)]
      , testCase "returns an empty list if no tag is found" $
          lookupTag (Tag "") exampleTagTable @?= []
      ]
  , testGroup "hintTags" $
      [ testCase "completes the input with all possible tags" $
          hintTags exampleTagTable "Tag" @?= ["Tag", "TagTable", "Tags", "TagsFileList"]
      , testCase "returns an empty list if the input is not a prefix of any tag" $
          hintTags exampleTagTable "FooBar" @?= []
      ]
  , testGroup "completeTag" $
      [ testCase "extends the input to the longest certain length" $
          completeTag exampleTagTable "_" @?= "_unTag"
      , testCase "returns the input when there are no completions possible" $
          completeTag exampleTagTable "FooBar" @?= "FooBar"
      ]
  ]
