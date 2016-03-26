module Options where

import Options.Applicative


options :: IO (Bool, FilePath)
options = execParser $
  info (helper <*> ((,) <$> switch (  short 'd'
                                   <> long "debug"
                                   <> help "Set log level to DEBUG")
                        <*> argument str (metavar "CONFIG")))
       (fullDesc <> header "ultron - A Slack Bot for UNIX-style ChatOps")
