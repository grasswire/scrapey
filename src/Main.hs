module Main where
import qualified  Web.Scrapey.App.Main as ScrapeyApp
import Web.Scrapey.OAuth
import Data.Configurator
import Web.Scrapey.APIs
import System.Environment

main :: IO()
main =   ScrapeyApp.main --putStrLn (show $ ScrapeyApp.parse ScrapeyApp.twitterStatusUrl "https://twitter.com/BothsidesTV/status/579339241416577024")

-- testConfig :: IO()
--
-- testConfig = do
--   config <- load [Required "/Users/levinotik/haskell/scrapey/test.cfg"]
--   credentials <- twitterCreds config
--   oauth <- twitterAuth config
--   t <- lookupTweet "582609392974966784" oauth credentials
--   putStrLn (show t)




