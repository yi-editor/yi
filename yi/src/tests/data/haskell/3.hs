-- CONSIDER: call quitEditor when there are no other window in the 'interactive' function.
-- (Not possible since the windowset type disallows it -- should it be relaxed?)
closeWindow :: YiM ()
closeWindow = do
    winCount <- withEditor $ uses windowsA PL.length
    tabCount <- withEditor $ uses tabsA PL.length
    when (winCount == 1 && tabCount == 1) quitEditor
    withEditor $ tryCloseE


onYiVar :: (Yi -> YiVar -> IO (YiVar, a)) -> YiM a
onYiVar f = do
    yi <- ask
    io $ modifyMVar (yiVar yi) (f yi)

-- | Kill a given subprocess
terminateSubprocesses :: (SubprocessInfo -> Bool) -> Yi -> YiVar -> IO (YiVar, ())
terminateSubprocesses shouldTerminate _yi var = do
        let (toKill, toKeep) = partition (shouldTerminate . snd) $ M.assocs $ yiSubprocesses var
        forM toKill $ terminateProcess . procHandle . snd
        return (var {yiSubprocesses = M.fromList toKeep}, ())

-- | Start a subprocess with the given command and arguments.
startSubprocess :: FilePath -> [String] -> (Either Exception ExitCode -> YiM x) -> YiM BufferRef
startSubprocess cmd args onExit = onYiVar $ \yi var -> do
        let (e', bufref) = runEditor
                              (yiConfig yi)
                              (printMsg ("Launched process: " ++ cmd) >> newEmptyBufferE (Left bufferName))
                              (yiEditor var)
            procid = yiSubprocessIdSupply var + 1
        procinfo <- createSubprocess cmd args bufref
        startSubprocessWatchers procid procinfo yi onExit
        return (var {yiEditor = e',
                     yiSubprocessIdSupply = procid,
                     yiSubprocesses = M.insert procid procinfo (yiSubprocesses var)
                    }, bufref)
  where bufferName = "output from " ++ cmd ++ " " ++ show args

startSubprocessWatchers :: SubprocessId -> SubprocessInfo -> Yi -> (Either Exception ExitCode -> YiM x) -> IO ()
startSubprocessWatchers procid procinfo yi onExit = do
    mapM_ forkOS ([pipeToBuffer (hErr procinfo) (send . append True) | separateStdErr procinfo] ++
                  [pipeToBuffer (hOut procinfo) (send . append False),
                   waitForExit (procHandle procinfo) >>= reportExit])
  where send a = output yi [makeAction a]
        append :: Bool -> String -> YiM ()
        append atMark s = withEditor $ appendToBuffer atMark (bufRef procinfo) s
        reportExit ec = send $ do append True ("Process exited with " ++ show ec)
                                  removeSubprocess procid
                                  onExit ec
                                  return ()

removeSubprocess :: SubprocessId -> YiM ()
removeSubprocess procid = modifiesRef yiVar (\v -> v {yiSubprocesses = M.delete procid $ yiSubprocesses v})

appendToBuffer :: Bool -> BufferRef -> String -> EditorM ()
appendToBuffer atErr bufref s = withGivenBuffer0 bufref $ do
    -- We make sure stdout is always after stderr. This ensures that the output of the
    -- two pipe do not get interleaved. More importantly, GHCi prompt should always
    -- come after the error messages.
    me <- getMarkB (Just "StdERR")
    mo <- getMarkB (Just "StdOUT")
    let mms = if atErr then [mo,me] else [mo]
    forM_ mms (flip modifyMarkB (\v -> v {markGravity = Forward}))
    insertNAt s =<< getMarkPointB (if atErr then me else mo)
    forM_ mms (flip modifyMarkB (\v -> v {markGravity = Backward}))

sendToProcess :: BufferRef -> String -> YiM ()
sendToProcess bufref s = do
    yi <- ask
    Just subProcessInfo <- find ((== bufref) . bufRef) . yiSubprocesses <$> readRef (yiVar yi)
    io $ hPutStr (hIn subProcessInfo) s

pipeToBuffer :: Handle -> (String -> IO ()) -> IO ()
pipeToBuffer h append =
  handle (const $ return ()) $ forever $ (hWaitForInput h (-1) >> readAvailable h >>= append)


waitForExit :: ProcessHandle -> IO (Either Exception ExitCode)
waitForExit ph =
    handle (\e -> return (Left e)) $ do
      mec <- getProcessExitCode ph
      case mec of
          Nothing -> threadDelay (500*1000) >> waitForExit ph
          Just ec -> return (Right ec)

withSyntax :: (Show x, YiAction a x) => (forall syntax. Mode syntax -> syntax -> a) -> YiM ()
withSyntax f = do
            b <- gets currentBuffer
            act <- withGivenBuffer b $ gets (withSyntax0 f)
            runAction $ makeAction $ act

userForceRefresh :: YiM ()
userForceRefresh = withUI UI.userForceRefresh
