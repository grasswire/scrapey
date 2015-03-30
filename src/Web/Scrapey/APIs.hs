module Web.Scrapey.APIs where

import Web.Twitter.Types
import Network.HTTP.Conduit
import Data.ByteString (ByteString)
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import Data.Maybe
import Control.Error.Safe
import Control.Monad (join)


lookupTweet :: String -> OAuth -> Credential  -> IO (Maybe Status) -- ^ If there is any error parsing the JSON data, it
lookupTweet tweetIdStr oauth creds = do
-- Firstly, we create a HTTP request with method GET (it is the default so we don't have to change that).
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/lookup.json?id=" ++ tweetIdStr
-- Using a HTTP manager, we authenticate the request and send it to get a response.
  res <- withManager $ \m -> do
-- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
-- appropriate authentication.
    signedreq <- signOAuth oauth creds req
-- Send request.
    httpLbs signedreq m
-- Decode the response body.
  return $ join $ fmap listToMaybe $ rightMay (eitherDecode $ responseBody res)



