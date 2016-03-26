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
import           Data.List (nub)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lens (unpacked)
import           Slack.Bot
import qualified System.Directory as D
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
import           System.Process
import           System.Process.Text as PT

import           Options (options)


main :: IO ()
main = do
  (debug, cfgfp) <- options
  cfgs <- readIniFile cfgfp >>= \case
            Left  err  -> error $ show err
            Right cfg  -> return $ map (readSection cfgfp cfg) $ sections cfg
  void $ mapConcurrently (runUltron debug) cfgs


data UltronCfg = UltronCfg { prefix  :: Text
                           , channel :: Text
                           , token   :: String
                           , paths   :: [FilePath]
                           }


runUltron :: Bool -> UltronCfg -> IO ()
runUltron debug cfg =
  (if debug
      then runStdoutLoggingT
      else runStdoutLoggingT . filterLogger (\_ -> (/= LevelDebug)))
  $ runBot (token cfg) (ultron cfg)


ultron :: UltronCfg -> SlackBot
ultron cfg (Message cid (UserComment uid) msg _ _ _) =
  unless (cid ^. getId /= channel cfg) $
    case parsemsg (prefix cfg) msg of
      Nothing -> return ()
      Just (cmd, args) -> do
        resp <- liftIO $ runcmd cid uid (paths cfg) cmd args
        sendMessage cid resp
ultron _ _ = return ()


readSection :: FilePath -> Ini -> Text -> UltronCfg
readSection fp ini sectionName =
  UltronCfg { prefix  = opt "prefix"
            , channel = opt "channel"
            , token   = T.unpack $ opt "token"
            , paths   = map T.unpack . T.splitOn ":" $ opt "paths"
            }
 where
  opt optName =
    case lookupValue sectionName optName ini of
      Left err -> error $ "Error in section `" ++ T.unpack sectionName ++ "` "
                          ++ "of `" ++ fp ++ "`: " ++ err
      Right x -> x


parsemsg :: Text -> Text -> Maybe (Text, [Text])
parsemsg pref msg =
  let preflen = T.length pref in
  if T.null msg || T.length msg < preflen || T.take preflen msg /= pref
    then Nothing
    else case T.words $ T.drop preflen msg of
           [] -> Nothing
           xs -> Just (head xs, tail xs)


runcmd :: ChannelId -> UserId -> [FilePath] -> Text -> [Text] -> IO Text
runcmd cid uid dps cmd args =
  handle
    (\e -> return $ "*ERROR:* " <> tshow (e :: SomeException))
    (if cmd == "help" then do
       bins <- listCommands dps
       return $ T.intercalate "\n" (map T.pack bins)
     else do
       getBin dps cmd >>= \case
         Nothing  -> return $ "*ERROR:* Unknown command: " <> cmd
         Just bin -> runbin bin)
 where
  cmdErrorResp stdout stderr fc =
    "<@" <> uid ^. getId <> ">\n"
    <> "*ERROR:* `" <> cmd <> "` failed with exit code: " <> tshow fc <>"\n"
    <> emptyIfNull "stdout" stdout
    <> emptyIfNull "stderr" stderr
  emptyIfNull _ "" = ""
  emptyIfNull name s = (T.intercalate "\n" [ "*" <> name <> ":*"
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
                ExitSuccess    -> stdout
                ExitFailure fc -> cmdErrorResp stdout stderr fc


listCommands :: [FilePath] -> IO [String]
listCommands dps = do
  existingDirs <- filterM D.doesDirectoryExist dps
  allbins <- concat <$> mapM executables existingDirs
  return $ nub allbins


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
