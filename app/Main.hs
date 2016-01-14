{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Async
import Control.Exception (handle, SomeException)
import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Ini as INI
import qualified Data.Ini.Reader as INI
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import System.Process.Text as PT
import Web.Slack
import Web.Slack.Message


main :: IO ()
main = do
  cfgfp <- execParser $
    info (helper <*> argument str (metavar "CONFIG"))
         (  fullDesc
         <> header "ultron - A Slack Bot for UNIX-style ChatOps"
         )
  cfgs <- readcfg cfgfp <$> readFile cfgfp
  void $ mapConcurrently runUltron cfgs


data UltronCfg
  = UltronCfg { prefix  :: Text
              , channel :: Text
              , token   :: String
              , paths   :: [FilePath]
              }


runUltron :: UltronCfg -> IO ()
runUltron cfg = runBot (SlackConfig (token cfg)) (ultron cfg) ()


ultron :: UltronCfg -> SlackBot ()
ultron cfg (Message cid (UserComment uid) msg _ _ _) =
  unless (cid ^. getId /= channel cfg) $
    case parsemsg (prefix cfg) msg of
      Nothing -> return ()
      Just (cmd, args) -> do
        resp <- liftIO $ runcmd cid uid (paths cfg) cmd args
        sendMessage cid resp
ultron _ _ = return ()


readcfg :: FilePath -> String -> [UltronCfg]
readcfg fp s =
  case INI.parse s of
    Left  err  -> error $ show err
    Right cfg  -> map (go cfg) $ INI.sections cfg
 where
  go cfg name =
    case INI.getSection name cfg of
      Nothing -> error $ "Section `" ++ name ++ "` not found in " ++ fp
      Just _ ->
        let getOpt optname =
              fromMaybe
                (error $ optname ++ " not found in section `" ++ name ++ "` in " ++ fp)
                (INI.getOption name optname cfg) in
        UltronCfg
        { prefix  = T.pack $ getOpt "prefix"
        , channel = T.pack $ getOpt "channel"
        , token   = getOpt "token"
        , paths   = map T.unpack . T.splitOn ":" . T.pack $ getOpt "paths"
        }


parsemsg :: Text -> Text -> Maybe (Text, [Text])
parsemsg pref msg =
  let preflen = T.length pref in
  if T.null msg || T.length msg < preflen || T.take preflen msg /= pref
    then Nothing
    else case T.words $ T.drop preflen msg of
           [] -> Nothing
           xs -> Just (head xs, tail xs)


runcmd :: ChannelId -> UserId -> [FilePath] -> Text -> [Text] -> IO Text
runcmd cid uid paths' cmd args =
  handle
    (\e -> return $ "*ERROR:* "<>tshow (e :: SomeException))
    (if cmd == "help" then do
       bins <- listBins paths'
       return $ T.intercalate "\n" (map T.pack bins)
     else do
       binMB <- getBin paths' cmd
       case binMB of
         Nothing -> return $ "*ERROR:* Unknown command: "<>cmd
         Just bin -> runbin bin)
 where
  runbin bin = do
    let p = (proc bin (map T.unpack args))
              { env = Just [ ("ULTRON_CID", T.unpack (cid ^. getId))
                           , ("ULTRON_UID", T.unpack (uid ^. getId))
                           ]
              , close_fds = True
              }
    (ec, stdout, stderr) <- PT.readCreateProcessWithExitCode p ""
    return $ case ec of
                ExitSuccess -> stdout
                ExitFailure x ->
                  "<@"<>uid ^. getId<>">\n"
                  <> "*ERROR:* `"<>cmd<>"` failed with exit code: "<>tshow x<>"\n"
                  <> if not $ T.null stdout
                       then "*stdout:*\n```\n"<>stdout<>"```\n"
                       else ""
                  <> if not $ T.null stderr
                       then "*stderr:*\n```\n"<>stderr<>"```"
                       else ""


listBins :: [FilePath] -> IO [FilePath]
listBins paths' = do
  existing <- filterM doesDirectoryExist paths'
  allbins <- concat <$> mapM (\p -> filterM (isExec p) =<< getDirectoryContents p) existing
  return $ nub allbins

getBin :: [FilePath] -> Text -> IO (Maybe FilePath)
getBin paths' cmd = do
  filteredPaths <- filterM filterPath =<< filterM doesDirectoryExist paths'
  case filteredPaths of
    [] -> return Nothing
    path:_ -> return . Just $ path </> cmdstr
 where
  cmdstr = T.unpack cmd
  filterPath path = do
    files <- filterM (isExec path) =<< getDirectoryContents path
    return $ cmdstr `elem` files


isExec :: FilePath -> FilePath -> IO Bool
isExec path name = executable <$> getPermissions (path </> name)


tshow :: Show a => a -> Text
tshow = T.pack . show
