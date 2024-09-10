{-# LANGUAGE TemplateHaskell #-}

import BrowserWorkspaceStore
import Data.FileEmbed      (embedFile)
import Reflex.Dom
import SplitUI

main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") (app BrowserWorkspaceStore)
