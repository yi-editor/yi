{-# LANGUAGE TemplateHaskell #-}
module Shim.Messages where

import Shim.MessagesTH
import qualified Shim.Hsinfo as Hsinfo
import Shim.SHM
import Shim.Sexp
import Shim.Utils

import SrcLoc
import ErrUtils ( Severity(..) )
import FastString
import Directory

import Control.Monad.State
import System.FilePath ( (</>) )

simpleCompleteModule :: FilePath -> String -> SHM (Response ([String], String))
simpleCompleteModule filename name = do
  l <- Hsinfo.findModulesPrefix filename name
  retVal (l, commonPrefix l)


fuzzyCompleteModule :: FilePath -> String -> SHM (Response [[String]])
fuzzyCompleteModule filename name = do
  l <- Hsinfo.findModulesPrefix filename name
  retVal (map (\x -> [x,"","",""]) l)

getModuleExports :: FilePath -> String -> String -> SHM (Response [[String]])
getModuleExports filename name prefix = do
  l <-  Hsinfo.getModuleExports filename name prefix
  retVal (map (\(x,y) -> [x,y,"",""]) l)

quit :: SHM (Response ())
quit = do
  logInfo "received the quit command, exiting"
  error "quit"
  return$ Error "quit"

fuzzyCompleteIdentifier :: FilePath -> String -> SHM (Response [[String]])
fuzzyCompleteIdentifier filename name = do
  l <- Hsinfo.findIdPrefix filename name
  retVal (map (\(s,ty) -> [s,ty,"",""]) l)

simpleCompleteIdentifier :: FilePath -> String -> SHM (Response ([String], String))
simpleCompleteIdentifier filename name = do
  l <- map fst `fmap` Hsinfo.findIdPrefix filename name
  retVal $ (l, commonPrefix l)

bufferNeedsPreprocessing :: FilePath -> String -> SHM (Response [Bool])
bufferNeedsPreprocessing filename source = do
  res <- Hsinfo.bufferNeedsPreprocessing filename source
  retVal [res]

-- loadFile :: FilePath -> Maybe String -> SHM (Response Se)
loadFile filename source = do
                     (res,_) <- Hsinfo.load filename True source
                     case res of
                       FileCompiled notes ->
                           retVal $ Se (S "file-compiled", encodeNotes notes)
                       ImportsOnly notes ->
                           retVal $ Se (S "compilation-failed", encodeNotes notes)
                       PreludeOnly notes ->
                           retVal $ Se (S "compilation-failed", encodeNotes notes)
                       NothingCompiled exc notes ->
                           retVal $ Se (S "compilation-exception", exc, encodeNotes notes)

lookupType :: FilePath -> Int -> Int -> Maybe String -> SHM (Response String)
lookupType filename line col source = do
  s <- Hsinfo.findTypeOfPos filename line col source
  retVal s

findDefinition :: FilePath -> Int -> Int -> Maybe String -> SHM (Response Se)
findDefinition filename line col source = do
  srcLoc <- Hsinfo.findDefinition filename line col source
  loc <- encodeSrcLoc srcLoc
  retVal loc

testUnexpectedExc :: SHM (Response a)
testUnexpectedExc = error "test-unexpected-exc"


-----------------------------------------

retVal :: a -> SHM (Response a)
retVal = return . Response

encodeSrcLoc :: SrcLoc -> SHM Se
encodeSrcLoc srcLoc
  | isGoodSrcLoc srcLoc = do projectDir <- io $ Directory.getCurrentDirectory
                             let filename0 = unpackFS $ srcLocFile srcLoc
                                 filename = case filename0 of
                                              ('/':_) -> filename0
                                              _ -> projectDir </> filename0
                             return $ Se (S ":loc", filename,
                                          srcLocLine srcLoc, srcLocCol srcLoc)
  | otherwise = return $ Se nil


encodeNotes :: [CompileNote] -> Se
encodeNotes l =
  Se . map (\c -> (encodeSeverity (severity c),
                   encodeSrcSpan c (projectdir c),
                   show (message c (pprStyle c)))) $ l
 where encodeSeverity SevInfo    = S ":sev-info"
       encodeSeverity SevWarning = S ":sev-warning"
       encodeSeverity SevError   = S ":sev-error"
       encodeSeverity SevFatal   = S ":sev-fatal"
       encodeSrcSpan s dir =
         maybe (Se nil)
               (\(f,l1,c1,l2,c2) -> if l1==l2 && c1==c2  -- hack to deal with empty
                                                         -- spans(emacs gets confused),
                                                         -- due to parsing errors
                                    then Se (S ":span",f,l1,c1,l2,c2+1)
                                    else Se (S ":span",f,l1,c1,l2,c2))
               $ excToMaybe $
         let loc1 = srcSpanStart $ srcSpan s
             loc2 = srcSpanEnd $ srcSpan s
             filename0 = unpackFS $ srcLocFile loc1
         in (dir </> filename0, srcLocLine loc1,
             srcLocCol loc1, srcLocLine loc2, srcLocCol loc2 )
             -- TODO: srcLoc{File,Line,Col} can panic, use isGoodSrcLoc

$(mkMsg 'simpleCompleteModule)
$(mkMsg 'fuzzyCompleteModule)
$(mkMsg 'simpleCompleteIdentifier)
$(mkMsg 'fuzzyCompleteIdentifier)
$(mkMsg 'lookupType)
$(mkMsg 'findDefinition)
$(mkMsg 'getModuleExports)
$(mkMsg 'bufferNeedsPreprocessing)
$(mkMsg 'loadFile)
$(mkMsg 'quit)

$(mkMessageList)
