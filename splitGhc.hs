{-# LANGUAGE TemplateHaskell #-}

import ActionsStore   (StubActionsStore (StubActionsStore))
import Data.FileEmbed (embedFile)
import Reflex.Dom     (mainWidgetWithCss)
import SplitUI        (app)

main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") (app StubActionsStore)
