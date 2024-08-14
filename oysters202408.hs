{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import Data.FileEmbed (embedFile)
import MoneySplit
import Reflex.Dom     (mainWidgetWithCss)
import SplitReport

main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") $ do
  report actions5 nullify5
