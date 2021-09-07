{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.HashMap.Strict    as HM
import           Data.IORef
import           Data.Maybe             (fromJust)
import           Data.Text
import qualified Data.Text.IO           as TIO (readFile)
import qualified Data.ByteString        as BS  (readFile)
import           System.Directory       (doesFileExist, copyFile)
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
    post "generate" generateHandler




data TargetFormat
    = EBOOK
    | HTML
    | PDF
    deriving (Show, Read, Eq)


generateHandler :: MonadIO m => ActionCtxT ctx m a
generateHandler = do
    params <- paramsPost
    liftIO $ print params

    dbFile <- HM.lookup "dbFile" <$> files
    liftIO $ print dbFile

    -- dat2 <- liftIO $ BS.readFile (uf_tempLocation $ fromJust dbFile)
    -- liftIO $ print dat2

    liftIO $ copyFile (uf_tempLocation $ fromJust dbFile) "/home/alx/testfile"

    file "placeholder File" "./kobohighlights.cabal"
