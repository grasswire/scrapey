{-# LANGUAGE OverloadedStrings #-}

module Web.Scrapey.App.Main where

import Text.HTML.Scalpel
import Data.Text (Text, pack)
import qualified Text.StringLike()
import Data.Monoid
import qualified Web.Scotty as WS
import Data.Functor()
import Control.Monad.IO.Class (liftIO)
import Web.Scrapey
import Network.HTTP.Types (status404)



main :: IO ()
main = WS.scotty 3000 $ do
     WS.get "/pagetitle" $ do
          url <- WS.param "url"
          liftIO (pageTitle url) >>= maybe (WS.status status404) WS.json
     WS.get "/tweet" $ do
          url <- WS.param "url"
          liftIO (tweet url) >>= maybe (WS.status status404) WS.json


pageTitle :: String -> IO (Maybe PageTitle)
pageTitle url = scrapeURL url title
  where
    title =  do
      t <- text $ pack "title"
      return $ PageTitle t (pack url)


tweet :: String -> IO (Maybe Tweet)
tweet url = scrapeURL url getTweet
  where
    getTweet :: Scraper Text Tweet
    getTweet = chroot (pack "div" @: [hasClass "tweet"]) innerTweet

    innerTweet :: Scraper Text Tweet
    innerTweet = do
      screenName <- text $ pack "span" @: [hasClass "username"]
      tweetText <- text $ pack "p" @: [hasClass "tweet-text"]
      return $ Tweet screenName tweetText
