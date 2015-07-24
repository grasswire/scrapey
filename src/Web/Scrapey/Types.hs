{-# LANGUAGE OverloadedStrings #-}

module Web.Scrapey.Types where

import Data.Aeson

import Data.Text (Text)

type Title = Text
type Url = Text
type CanonicalUrl = Text
type Description = Text
type ImageUrl = Text

data LinkPreview = LinkPreview Title Url (Maybe CanonicalUrl) Description (Maybe ImageUrl)
  deriving (Show, Eq)

instance ToJSON LinkPreview where
  toJSON (LinkPreview t u c d i) = object ["title" .= t, "url" .= u,  "canonicalUrl" .= c, "description" .= d, "image" .= i]
