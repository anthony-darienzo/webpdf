{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Web.Scotty

import Network.Wai.Middleware.StaticEmbedded

import Data.ByteString (ByteString)
import Data.FileEmbed (makeRelativeToProject, embedDir)
import System.FilePath ( (</>) )

grabEmbeddedFiles :: [(FilePath, ByteString)]
grabEmbeddedFiles =
  let xs  = $(makeRelativeToProject "web" >>= embedDir)
      ys  = $(makeRelativeToProject "pdfjs-dist" >>= embedDir)
      xs' = map (\ (f,b) -> ("web" </> f, b)) xs
      ys' = map (\ (f,b) -> ("pdfjs-dist" </> f, b)) ys
  in xs' <> ys'

main :: IO ()
main = scotty 3000 $ do
  middleware (static grabEmbeddedFiles)
  get "/pdf" $ do
    html "<h1>PDF viewer in progress.</h1>"
    
