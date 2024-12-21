{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedFile)
import Reflex.Dom     (mainWidgetWithCss)
import SplitUI        (app)
import WorkspaceStore (StubWorkspaceStore (StubWorkspaceStore))

main :: IO ()
main
  = mainWidgetWithCss $(embedFile "split.css")
    (app StubWorkspaceStore (\_-> return ()) Nothing)
