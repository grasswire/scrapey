{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Scrapey.App.Main where

import Text.HTML.Scalpel
import qualified Data.Text as T
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
import Network.URI

main :: IO ()
main = scottyStart

scottyStart :: IO ()
scottyStart = WS.scotty 3000 $ do
        WS.get "/healthcheck" $ do
          WS.addHeader  "Access-Control-Allow-Origin"  "*"
          WS.text "healthy"
        WS.get "/pagetitle" $ do
          WS.addHeader  "Access-Control-Allow-Origin"  "*"
          url <- WS.param "url"
          liftIO (pageTitle url) >>= maybe (WS.status status404) WS.json
        WS.get "/images" $ do
          WS.addHeader  "Access-Control-Allow-Origin"  "*"
          url <- WS.param "url"
          liftIO (pageImgSources url) >>= maybe (WS.status status404) WS.json
        WS.get "/link_preview" $ do
          WS.addHeader "Access-Control-Allow-Origin" "*"
          url <- WS.param "url"
          liftIO (pagePreview url) >>= maybe (WS.status status404) WS.json


pageTitle :: String -> IO (Maybe PageTitle)
pageTitle url = scrapeURL url title
  where
    title =  do
      t <- text ("title" :: String)
      return $ PageTitle t (T.pack url)

pagePreview :: String -> IO (Maybe LinkPreview)
pagePreview url = scrapeURL url preview
  where
    preview = do
      title <- text ("title" :: String)
      images <- (attrs ("src" :: String) ("img" :: String))
      return $ LinkPreview (T.pack title) "" "" "" "" (T.pack <$> images)

getCanonicalUrl :: String -> Maybe String
getCanonicalUrl url = uriRegName <$> (uriAuthority =<< parseURI url)

pageImgSources :: String -> IO (Maybe [T.Text])
pageImgSources url = scrapeURL url $ attrs ("src" :: String) ("img" :: String)

tweet :: String -> Config -> IO (Maybe Status)
tweet url config =   case rightMay (P.parse twitterStatusUrl "(source)" $ url) of
  Just tweetIdStr -> do
        credentials <- twitterCreds config
        oauth <- twitterAuth config
        lookupTweet tweetIdStr oauth credentials
  _ -> pure Nothing

twitterStatusUrl :: P.Parsec String () String
twitterStatusUrl = do
      P.string "https://twitter.com" <|> P.string "twitter.com" <|> P.string "www.twitter.com"
      P.char '/'
      P.many (P.alphaNum <|> P.char '_')
      P.string "/status/"
      id <- P.many P.digit
      return id
