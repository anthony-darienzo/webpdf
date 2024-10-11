{-# LANGUAGE TemplateHaskell #-}
module Viewer where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet ( shamletFile )

import Data.Text.Internal.Lazy (Text)
import Data.FileEmbed (makeRelativeToProject)

viewer :: Text
viewer = renderHtml $(makeRelativeToProject "app/viewer.hamlet" >>= shamletFile)
