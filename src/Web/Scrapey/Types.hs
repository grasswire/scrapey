{-# LANGUAGE OverloadedStrings #-}

module Web.Scrapey.Types where

import Data.Aeson

import Data.Text (Text)

type Title = Text
type Url = Text

data PageTitle = PageTitle Title Url
  deriving (Show, Eq)

instance ToJSON PageTitle where
     toJSON (PageTitle title url) = object ["title" .= title, "url" .= url]



type HuntDescription = Text
type HuntLink = Text
type HuntTitle = Text

data Hunt = Hunt HuntTitle HuntDescription HuntLink
  deriving (Show, Eq)

type ScreenName = Text
type TweetText = Text

data Tweet = Tweet ScreenName TweetText
  deriving (Show, Eq)

instance ToJSON Tweet where
     toJSON (Tweet screenName tweetText) = object ["screenName" .= screenName, "tweetText" .= tweetText]