{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.ByteString           as BS (readFile)
import qualified Data.HashMap.Strict       as HM
import           Data.IORef
import           Data.Maybe                (fromJust)
import           Data.Text
import qualified Data.Text.IO              as TIO (readFile)
import           Network.HTTP.Types.Status
import           System.Directory          (copyFile, doesFileExist)
import           System.Exit
import           System.FilePath           (replaceExtensions)
import           System.Process            (readProcessWithExitCode)
import           Web.Spock
import           Web.Spock.Config

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)


kobonotesBin :: FilePath
kobonotesBin = "/opt/kobonotes"


tmpDir :: FilePath
tmpDir = "/opt/koboNotesTmp"


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

    liftIO $ copyFile (uf_tempLocation $ fromJust dbFile) $ tmpDir <> "/testfile"

    case (lookup "targetFormat" params) of
        Nothing      -> setStatus badRequest400
        Just "EBOOK" -> (liftIO $ generateFinal EBOOK $ tmpDir <> "/testfile") >>= (\x -> liftIO $ print x)
        Just "HTML"  -> (liftIO $ generateFinal HTML  $ tmpDir <> "/testfile") >>= (\x -> liftIO $ print x)
        Just "PDF"   -> (liftIO $ generateFinal PDF   $ tmpDir <> "/testfile") >>= (\x -> liftIO $ print x)
        Just _       -> setStatus badRequest400

    file "placeholder File" "./kobohighlights.cabal"


generate :: TargetFormat -> FilePath -> IO
generate tf inputFile = undefined

generateFinal :: TargetFormat -> FilePath -> IO (Either String FilePath)
generateFinal EBOOK inputFile = performConvertion ebookParams inputFile $ replaceExtensions inputFile "epub"
generateFinal HTML  inputFile = performConvertion htmlParams  inputFile $ replaceExtensions inputFile "html"
generateFinal PDF   inputFile = performConvertion pdfParams   inputFile $ replaceExtensions inputFile "pdf"

performConvertion params inputFile outputFile = do
    (exitCode, _, err) <- readProcessWithExitCode "pandoc"
        [ inputFile
        , "-o " <> outputFile
        , params
        ] ""
    case exitCode of
        ExitFailure _ -> pure $ Left err
        ExitSuccess   -> pure $ Right outputFile

ebookParams = "--toc --toc-depth=1 --standalone --mathjax"
htmlParams  = "--toc --toc-depth=1 --template ./template/template.html --standalone --mathjax"
pdfParams   = "--toc --toc-depth=1 -H ./template/template.tex --standalone --mathjax -V geometry:margin=2cm"


generateMarkdown :: FilePath -> IO (Either String FilePath)
generateMarkdown dbFile = do
    let outputFile = replaceExtensions dbFile "md"
    (exitCode, _, err) <- readProcessWithExitCode kobonotesBin [dbFile, outputFile] ""

    case exitCode of
        ExitFailure _ -> pure $ Left err
        ExitSuccess   -> pure $ Right outputFile
