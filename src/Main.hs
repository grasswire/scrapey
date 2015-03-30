module Main where
import qualified  Web.Scrapey.App.Main as ScrapeyApp
import Web.Scrapey.OAuth
import Data.Configurator
import Web.Scrapey.APIs
import System.Environment

main :: IO()
main =   ScrapeyApp.main
