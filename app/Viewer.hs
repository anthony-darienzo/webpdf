{-# LANGUAGE TemplateHaskell #-}
module Viewer where

import Data.ByteString (ByteString)
import Data.FileEmbed (makeRelativeToProject, embedDir)
import System.FilePath ( (</>) )

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet ( shamletFile )

import Data.Text.Internal.Lazy (Text)
import Data.FileEmbed (makeRelativeToProject)

grabEmbeddedFiles :: [(FilePath, ByteString)]
grabEmbeddedFiles =
  let xs  = $(makeRelativeToProject "web" >>= embedDir)
      ys  = $(makeRelativeToProject "pdfjs-dist" >>= embedDir)
      xs' = map (\ (f,b) -> ("web" </> f, b)) xs
      ys' = map (\ (f,b) -> ("pdfjs-dist" </> f, b)) ys
  in xs' <> ys'

viewer :: Text 
viewer = renderHtml $(makeRelativeToProject "app/viewer.hamlet" >>= shamletFile)