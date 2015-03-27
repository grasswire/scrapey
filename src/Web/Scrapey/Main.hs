{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HTML.Scalpel
import Data.Text (Text, pack)
import qualified Text.StringLike()
import Data.Monoid

main :: IO ()
main = allHunts >>= \hunts -> putStrLn (show hunts)

type HuntDescription = Text
type HuntLink = Text
type HuntTitle = Text

data Hunt = Hunt HuntTitle HuntDescription HuntLink
 deriving Show

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




