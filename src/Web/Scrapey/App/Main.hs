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
import Data.List as DL
import Data.Maybe (isJust)
import Network.Wai.Middleware.RequestLogger
import Control.Monad (mfilter)
import Network.Curl

main :: IO ()
main =  scottyStart

scottyStart :: IO ()
scottyStart = WS.scotty 3000 $ do
        WS.middleware logStdout
        WS.get "/healthcheck" $ do
          WS.addHeader  "Access-Control-Allow-Origin"  "*"
          WS.text "healthy"
        WS.get "/link_preview" $ do
          WS.addHeader "Access-Control-Allow-Origin" "*"
          url <- WS.param "url"
          case parseURI url of
            Just uri -> liftIO (linkPreview uri) >>= maybe (WS.status status404) WS.json
            _ -> WS.status status400 >> WS.json (object [("error" , String (T.pack $ url ++ " is not a valid url"))])


linkPreview :: URI -> IO (Maybe LinkPreview)
linkPreview url = scrapeURLWithOpts [CurlFollowLocation True, CurlUserAgent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.89 Safari/537.36"] (show url) preview
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
      title <- sattr  "content" ("meta" @@: ["property" @@= "og:title"]) <|>  stext "title" 
      image <- Just <$> sattr  "content" ("meta" @@: ["property" @@= "og:image"]) <|> return Nothing
      description <- Just <$> (sattr "content" ("meta" @@: ["name" @@= "description"])
                  <|> sattr "content" ("meta" @@: ["property" @@= "og:description"])) <|> return Nothing
      return $ LinkPreview (b2t title) (s2t (show url)) (s2t <$> getCanonicalUrl url) (b2t <$> description) (s2t <$> (mfilter (not . isDataScheme) $ makeAbsPaths <$> (b2s <$> image)))
        where
          makeAbsPaths img = case getCanonicalUrl url of
                                Just u -> uriWithScheme u img
                                _      -> img

          uriWithScheme _ i | isAbsoluteURI i = i
          uriWithScheme u i@('/' : '/' : _)   = uriScheme url ++ i
          uriWithScheme u i                   = uriScheme url ++ "//" ++ u ++ i

          isDataScheme uri =  maybe True (isJust . DL.stripPrefix "data" . uriScheme) (parseURI uri)

getCanonicalUrl :: URI -> Maybe String
getCanonicalUrl url = uriRegName <$> uriAuthority url
