{-# LANGUAGE OverloadedStrings #-}

module Web.Scrapey.App.Main where

import Text.HTML.Scalpel
import Data.Text (Text, pack, unpack)
import qualified  Data.Text.Lazy as LT
import qualified Text.StringLike()
import Data.Monoid
import qualified Web.Scotty as WS
import Data.Functor
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Web.Scrapey
import Data.Aeson
import Control.Applicative (pure)
import Network.HTTP.Types (Status, StdMethod, status404)



main :: IO ()
main = WS.scotty 3000 $ do
     WS.get "/pagetitle" $ do
          url <- WS.param "url"
          liftIO (pageTitle url) >>= maybe (WS.status status404) WS.json


test = do
  t <- tweet "https://twitter.com/LeviNotik/status/581526803631775744"
  putStrLn (show t)

f = PageTitle "foo" "bar"

type HuntDescription = Text
type HuntLink = Text
type HuntTitle = Text

data Hunt = Hunt HuntTitle HuntDescription HuntLink
 deriving Show

type TwitterHandle = Text
type TweetText = Text

data Tweet = Tweet TwitterHandle TweetText
  deriving Show

-- instance WS.ToJSON Tweet where



allHunts :: IO (Maybe [Hunt])
allHunts = scrapeURL "http://www.producthunt.com" hunts
   where
       hunts :: Scraper Text [Hunt]
       hunts = chroots (pack "div" @: [hasClass "url"]) huntDescriptions

       huntDescriptions :: Scraper Text Hunt
       huntDescriptions = do
           title      <- text $ pack "a" @: [hasClass "title"]
           desc      <- text $ pack "span" @: [hasClass "description"]
           link      <-  attr "href" $ pack "a"
           return $ Hunt title desc  (pack "http://www.producthunt.com" <> link)

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
      handle <- text $ pack "span" @: [hasClass "username"]
      tweetText <- text $ pack "p" @: [hasClass "tweet-text"]
      return $ Tweet handle tweetText
