{-# LANGUAGE TemplateHaskell #-}
module Viewer where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet ( shamletFile )

import Data.Text.Internal.Lazy (Text)

viewer :: Text
viewer = renderHtml $(shamletFile "app/viewer.hamlet")