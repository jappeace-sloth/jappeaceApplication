module Main where

import Development.Shake
import Development.Shake.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hSetEncoding, utf8, stdout, stderr)
import Text.Pandoc
  ( runIOorExplode
  , readMarkdown
  , writeHtml5String
  , def
  , readerExtensions
  , pandocExtensions
  )

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  shakeArgs shakeOptions{shakeFiles="_build"} $ do

    "_site//*.html" %> \out -> do
      let src = "content" </> dropDirectory1 out -<.> "md"
      need [src]
      markdown <- liftIO $ T.readFile src
      html <- liftIO $ renderMarkdown markdown
      liftIO $ T.writeFile out html

    phony "build" $ do
      mds <- getDirectoryFiles "content" ["//*.md"]
      let htmls = ["_site" </> md -<.> "html" | md <- mds]
      need htmls

    phony "clean" $ do
      putInfo "Cleaning _site and _build"
      removeFilesAfter "_site" ["//*"]
      removeFilesAfter "_build" ["//*"]

renderMarkdown :: T.Text -> IO T.Text
renderMarkdown markdown = runIOorExplode $ do
  doc <- readMarkdown def{readerExtensions = pandocExtensions} markdown
  writeHtml5String def doc
