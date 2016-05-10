{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception (handle, SomeException)
import           Control.Lens hiding (argument)
import           Control.Monad (void, unless, filterM)
import           Control.Monad.Logger ( runStdoutLoggingT
                                      , filterLogger
                                      , LogLevel(LevelDebug))
import           Data.Ini (lookupValue, Ini, readIniFile, sections)
import           Data.List (nub, sort)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lens (unpacked)
import qualified System.Directory as D
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
import           System.Process (proc, CreateProcess(..))
import           System.Process.Text as PT

import           Slack.Bot

import           Parse
import           Options (options)


main :: IO ()
main = do
  (debug, cfgfp) <- options
  cfgs <- readIniFile cfgfp >>= \case
            Left  err  -> error $ show err
            Right cfg  -> return $ map (readSection cfg) $ sections cfg
  void $ mapConcurrently (runUltron debug) cfgs


data UltronCfg = UltronCfg
  { prefix  :: Text
  , channel :: Text
  , token   :: String
  , paths   :: [FilePath]
  }


runUltron :: Bool -> UltronCfg -> IO ()
runUltron debug cfg =
  runStdoutLoggingT . debugFilter $ runBot (token cfg) (ultron cfg)
 where
  debugFilter | debug     = id
              | otherwise = filterLogger (\_ -> (/= LevelDebug))


ultron :: UltronCfg -> SlackBot
ultron cfg (Message cid (UserComment uid) msg _ _ _) =
  unless (cid ^. getId /= channel cfg) $
    case dropPrefix (prefix cfg) msg of
      Nothing   -> return ()
      Just msg' ->
        case parseCommand msg' of
          Left  _           -> return ()
          Right (cmd, args) -> do
            resp <- liftIO $ runCommand cid uid (paths cfg) cmd args
            sendMessage cid resp
ultron _ _ = return ()


readSection :: Ini -> Text -> UltronCfg
readSection ini sectionName =
  UltronCfg
  { prefix  = opt "prefix"
  , channel = opt "channel"
  , token   = T.unpack $ opt "token"
  , paths   = map T.unpack . T.splitOn ":" $ opt "paths"
  }
 where
  opt optName =
    case lookupValue sectionName optName ini of
      Left err -> error $ "Error in section `"++T.unpack sectionName++"`: "++err
      Right x  -> x


runCommand :: ChannelId -> UserId -> [FilePath] -> Text -> [Text] -> IO Text
runCommand cid uid dirpaths cmd args = handle handler go
 where
  handler :: SomeException -> IO Text
  handler e = return $ "*ERROR:* "<>tshow e
  go | cmd == "help" = help dirpaths
     | otherwise     =
         getBin dirpaths cmd >>= \case
           Nothing  -> return $ "*ERROR:* Unknown command: " <> cmd
           Just bin -> runbin bin
  cmdErrorResponse stdout stderr errNum =
    "<@"<>uid ^. getId<>">\n"
    <>"*ERROR:* `"<>cmd<>"` failed with exit code: "<>tshow errNum<>"\n"
    <>emptyIfNull "stdout" stdout
    <>emptyIfNull "stderr" stderr
  emptyIfNull _    "" = ""
  emptyIfNull name s  = (T.intercalate "\n" [ "*" <> name <> ":*"
                                            , "```"
                                            , s
                                            , "```"
                                            ]) <> "\n"
  mkProc bin = (proc bin (map T.unpack args))
               { env = Just [ ("ULTRON_CID", (cid ^. getId . unpacked))
                            , ("ULTRON_UID", (uid ^. getId . unpacked))
                            ]
               , close_fds = True
               }
  runbin bin = do
    (ec, stdout, stderr) <- PT.readCreateProcessWithExitCode (mkProc bin) ""
    return $ case ec of
               ExitSuccess        -> stdout
               ExitFailure errNum -> cmdErrorResponse stdout stderr errNum


help :: [FilePath] -> IO Text
help dirpaths = T.intercalate "\n" . map T.pack <$> listCommands dirpaths


listCommands :: [FilePath] -> IO [String]
listCommands dps = do
  existingDirs <- filterM D.doesDirectoryExist dps
  allbins <- concat <$> mapM executables existingDirs
  return . sort $ nub allbins


executables :: FilePath -> IO [String]
executables dp = do
  fns <- D.getDirectoryContents dp
  filterM (isExecutable dp) fns


getBin :: [FilePath] -> Text -> IO (Maybe FilePath)
getBin dps cmd = do
  existingDirs <- filterM D.doesDirectoryExist dps
  filterM hasCmd existingDirs >>= \case
    []   -> return Nothing
    dp:_ -> return . Just $ dp </> cmdstr
 where
  cmdstr = T.unpack cmd
  hasCmd dp = elem cmdstr <$> executables dp


isExecutable :: FilePath -> FilePath -> IO Bool
isExecutable dp fn = D.executable <$> D.getPermissions (dp </> fn)


tshow :: Show a => a -> Text
tshow = T.pack . show
