{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lighthouse.Connection
import Lighthouse.Options
import Lighthouse.Utils.Logging
import qualified Data.Text as T
import System.Environment (getEnv)

app :: Listener
app = mempty
  { onConnect = do
      logInfo "app" "Connected!"
  }

main :: IO ()
main = do
  username <- T.pack <$> getEnv "LIGHTHOUSE_USERNAME"
  token    <- T.pack <$> getEnv "LIGHTHOUSE_TOKEN"
  let auth  = Authentication { authUsername = username, authToken = token }
      opts  = Options { optAuthentication = auth, optLogHandler = simpleLogHandler infoLevel }

  runLighthouseApp app opts
