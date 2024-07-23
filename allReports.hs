{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import Data.FileEmbed (embedFile)
import MoneySplit
import Reflex.Dom     (blank, el, mainWidgetWithCss, text)
import SplitReport

main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") $ do
  report actions4 nullify4
  text "------"; el "br" blank
  report actions3 nullify3
  text "------"; el "br" blank
  report actions2 nullify2
  text "------"; el "br" blank
  report actions1 nullify1

