{-# LANGUAGE TemplateHaskell #-}

import SplitUI
import BrowserActionsStore
import           Data.FileEmbed    (embedFile)
import Reflex.Dom

main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") (app BrowserActionsStore)
