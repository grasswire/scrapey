{-# LANGUAGE OverloadedStrings #-}

module Web.Scrapey.Types where

import Data.Aeson

import Data.Text (Text)

type Title = Text
type Url = Text
type CanonicalUrl = Text
type Description = Text
type ImageUrl = Text

data PageTitle = PageTitle Title Url
  deriving (Show, Eq)

instance ToJSON PageTitle where
     toJSON (PageTitle title url) = object ["title" .= title, "url" .= url]
   -- {
   --     "title":"title",
   --     "url":"original url",
   --     "pageUrl":"page url",
   --     "canonicalUrl":"cannonical url",
   --     "description":"description",
   --     "images": "img1|img2|...",
   --     "video":"yes|no",
   --     "videoIframe":"video iframe if it is video"
   --  }

data LinkPreview = LinkPreview Title Url (Maybe CanonicalUrl) Description [ImageUrl]
  deriving (Show, Eq)

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

instance ToJSON LinkPreview where
  toJSON (LinkPreview t u c d i) = object ["title" .= t, "url" .= u,  "canonicalUrl" .= c, "description" .= d, "images" .= i]
