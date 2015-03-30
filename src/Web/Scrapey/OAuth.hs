{-# LANGUAGE OverloadedStrings #-}


module Web.Scrapey.OAuth where

import Web.Authenticate.OAuth
import Data.Configurator.Types
import Data.Configurator as CFG
import Control.Monad.IO.Class (liftIO)

-- throws a key error if the bindings don't exist
twitterAuth :: Config ->  IO (OAuth)
twitterAuth cfg = do
   consumerKey <- CFG.require  cfg "twitter_consumer_key"
   consumerSecret <- CFG.require cfg "twitter_consumer_secret"
   return $ newOAuth {oauthConsumerKey = consumerKey, oauthConsumerSecret = consumerSecret }

-- throws a key error if the bindings don't exist
twitterCreds :: Config -> IO (Credential)
twitterCreds cfg =  do
   accessToken <- CFG.require  cfg "twitter_access_token"
   accessSecret <- CFG.require cfg "twitter_access_secret"
   return $ newCredential accessToken accessSecret