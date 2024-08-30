{-# LANGUAGE TemplateHaskell #-}

import BrowserActionsStore
import Data.FileEmbed      (embedFile)
import Reflex.Dom
import SplitUI

main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") (app BrowserActionsStore)
