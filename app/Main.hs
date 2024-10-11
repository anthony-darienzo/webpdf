{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Web.Scotty

import qualified Network.Wai.Middleware.Static as NS
import qualified Network.Wai.Middleware.StaticEmbedded as NSE

import Data.ByteString (ByteString)
import Data.FileEmbed (makeRelativeToProject, embedDir)
import System.FilePath ( (</>) )

import Text.Read (readMaybe)
import System.Environment (getArgs)

import Viewer (viewer)

default_port :: Int
default_port = 3000

grabEmbeddedFiles :: [(FilePath, ByteString)]
grabEmbeddedFiles =
  let xs  = $(makeRelativeToProject "web" >>= embedDir)
      ys  = $(makeRelativeToProject "pdfjs-dist" >>= embedDir)
      xs' = map (\ (f,b) -> ("web" </> f, b)) xs
      ys' = map (\ (f,b) -> ("pdfjs-dist" </> f, b)) ys
  in xs' <> ys'
  
mainScottyLoop :: Int -> FilePath -> IO ()
mainScottyLoop port pdf_path = do
  putStrLn $ "Serving PDF file " <> pdf_path
  scotty port $ do
    let pdf_policy = NS.only [("currentPDF", pdf_path)] -- "This actual means /currentPDF"
    middleware (NS.unsafeStaticPolicy pdf_policy)
    middleware (NSE.static grabEmbeddedFiles)
    get "/pdf" $ do
      html viewer
    get "/" $ do
      redirect "/pdf?file=/currentPDF"

printHelpDialog :: IO ()
printHelpDialog = putStr $ unlines
  [ "webpdf.hs : Haskell PDF.js server."
  , ""
  , "Uses scotty and pdf.js to render a PDF supllied on the command line into"
  , " a static server."
  , "  - Use -p to specify a port (default 3000)."
  , ""
  , "Usage: wedpdf.hs [-p PORT] SAMPLE.PDF"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                      -> printHelpDialog
    ["-h"]                  -> printHelpDialog
    [pdf_path]              -> mainScottyLoop default_port pdf_path
    ["-p", portarg, pdf_path]  -> do
      let port = readMaybe portarg
      case port of
        Nothing -> ioError $ userError "Error : Invalid port."
        Just p  -> mainScottyLoop p pdf_path
    _ -> printHelpDialog
