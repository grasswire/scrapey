{-# LANGUAGE OverloadedStrings #-}

module Web.Scrapey.Types where

import Data.Aeson

import Data.Text (Text)

type Title = Text
type Url = Text

data PageTitle = PageTitle Title Url

instance ToJSON PageTitle where
     toJSON (PageTitle title url) = object ["title" .= title, "url" .= url]



