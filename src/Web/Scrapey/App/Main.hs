{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Web.Scrapey.App.Main where

import Text.HTML.Scalpel
import Data.Text (pack)
import qualified Text.StringLike()
import qualified Web.Scotty as WS
import Data.Functor()
import Control.Monad.IO.Class (liftIO)
import Web.Scrapey
import Network.HTTP.Types (status404)
import Web.Twitter.Types
import Control.Applicative
import qualified Text.Parsec as P
import Control.Error.Safe
import Data.Configurator.Types
import Data.Configurator
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= \ case
      (arg: _) -> load [Required arg] >>= \config -> scottyStart config
      _ -> putStrLn "You must supply the fully qualified path to your scrapey.cfg" >> exitWith (ExitFailure (-1))


scottyStart :: Config -> IO ()
scottyStart config = WS.scotty 3000 $ do
        WS.get "/pagetitle" $ do
          url <- WS.param "url"
          liftIO (pageTitle url) >>= maybe (WS.status status404) WS.json
        WS.get "/tweet" $ do
          url <- WS.param "url"
          liftIO (tweet url config) >>= maybe (WS.status status404) WS.json

pageTitle :: String -> IO (Maybe PageTitle)
pageTitle url = scrapeURL url title
  where
    title =  do
      t <- text $ pack "title"
      return $ PageTitle t (pack url)

tweet :: String -> Config -> IO (Maybe Status)
tweet url config =   case rightMay (P.parse twitterStatusUrl "(source)" $ url) of
  Just tweetIdStr -> do
        credentials <- twitterCreds config
        oauth <- twitterAuth config
        lookupTweet tweetIdStr oauth credentials
  _ -> pure Nothing


twitterStatusUrl :: P.Parsec String () String
twitterStatusUrl  =  do
      P.string "https://twitter.com" <|> P.string "twitter.com" <|> P.string "www.twitter.com"
      P.char '/'
      P.many (P.alphaNum <|> P.char '_')
      P.string "/status/"
      id <- P.many P.digit
      return id