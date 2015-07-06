{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Scrapey.App.Main where

import Text.HTML.Scalpel
import qualified Data.Text as T
import qualified Text.StringLike()
import qualified Web.Scotty as WS
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import Data.ByteString (ByteString)
import Data.Functor()
import Control.Monad.IO.Class (liftIO)
import Web.Scrapey
import Network.HTTP.Types (status404, status400)
import Control.Applicative
import Control.Error.Safe
import Network.URI
import Data.Aeson

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
          maybe (WS.status status400 >> WS.json (object [("error" , String (T.pack $ url ++ " is not a valid url"))])) (\uri -> liftIO (linkPreview uri) >>= maybe (WS.status status404) WS.json) (parseURI url)


linkPreview :: URI -> IO (Maybe LinkPreview)
linkPreview url = scrapeURL (show url) preview
  where
    b2t = E.decodeUtf8
    s2t = T.pack
    b2s = T.unpack . b2t
    stext :: String -> Scraper ByteString ByteString
    stext = text
    sattrs :: String -> String -> Scraper ByteString [ByteString]
    sattrs = attrs
    sattr :: String -> Selector -> Scraper ByteString ByteString
    sattr = attr
    (@@:) :: String -> [AttributePredicate] -> Selector
    (@@:) = (@:)
    (@@=) :: String -> String -> AttributePredicate
    (@@=) = (@=)
    preview :: Scraper ByteString LinkPreview
    preview = do
      title <- stext "title"
      images <- sattrs "src" "img"
      description <-  sattr "content" ("meta" @@: ["name" @@= "description"])
                  <|> sattr "content" ("meta" @@: ["property" @@= "og:description"])
      return $ LinkPreview (b2t title) (s2t (show url)) (s2t <$> getCanonicalUrl url) (b2t description) (s2t <$> makeAbsPaths (filter (not . null) (map b2s images)))
        where
          makeAbsPaths imgs = case getCanonicalUrl url of
                                Just u -> (\i -> if isAbsoluteURI i then prependScheme i else prependScheme (u ++ i)) <$> imgs
                                _ -> imgs
          prependScheme = (++) (uriScheme url ++ "//")

getCanonicalUrl :: URI -> Maybe String
getCanonicalUrl url = uriRegName <$> uriAuthority url
