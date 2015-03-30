module Web.Scrapey.Types.Class where


import Data.Text (Text)

type Title = Text
type Url = Text

data PageTitle = Title Url

