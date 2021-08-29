{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Text
import qualified Data.Text.IO           as TIO (readFile)
import           Web.Spock
import           Web.Spock.Config

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main = do
    indexHtml <- TIO.readFile "static/index.html"
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8080 (spock spockCfg $ app indexHtml)


app :: Text -> SpockM () MySession MyAppState ()
app indexHtml = do
    get root $ html indexHtml
