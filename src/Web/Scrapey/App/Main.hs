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
import Control.Applicative
import Control.Error.Safe
import Network.URI

main :: IO ()
main = scottyStart

scottyStart :: IO ()
scottyStart = WS.scotty 3000 $ do
        WS.get "/healthcheck" $ do
          WS.addHeader  "Access-Control-Allow-Origin"  "*"
          WS.text "healthy"
        WS.get "/link_preview" $ do
          WS.addHeader "Access-Control-Allow-Origin" "*"
          url <- WS.param "url"
          liftIO (linkPreview url) >>= maybe (WS.status status404) WS.json

linkPreview :: String -> IO (Maybe LinkPreview)
linkPreview url = scrapeURL url preview
  where
    preview = do
      title <- text ("title" :: String)
      images <- attrs ("src" :: String) ("img" :: String)
      description <- attr ("content" :: String) (("meta" :: String)  @: [("name" :: String) @= ("description" :: String)]) <|> attr ("content" :: String) (("meta" :: String)  @: [("property" :: String) @= ("og:description" :: String)])
      return $ LinkPreview (T.pack title) (T.pack url) (T.pack <$> getCanonicalUrl url)  (T.pack description) (T.pack <$> makeAbsPaths images)
        where
          makeAbsPaths imgs = case getCanonicalUrl url of
                                Just u -> (\i -> if (isAbsoluteURI i) then i else u ++ i) <$> imgs
                                _ -> imgs

getCanonicalUrl :: String -> Maybe String
getCanonicalUrl url = uriRegName <$> (uriAuthority =<< parseURI url)
