{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import           BrowserActionsStore
import           Data.Aeson                (encode)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.FileEmbed            (embedFile)
import           Data.JSString             (pack)
import           JavaScript.Web.Storage    (getItem, localStorage, setItem)
import           MoneySplit
import           Reflex.Dom
import           SplitUI

main :: IO ()
main = do
  getItem "splitActions" localStorage >>= \case
    Nothing -> do
      setItem
        "splitActions"
        (pack . UTF8.toString . encode $ actions3)
        localStorage
    Just _ -> do return ()
  mainWidgetWithCss $(embedFile "split.css") $ do
    elAttr "span" ("class" =: "notice") $ do
      text "The following page is pre-populated with sample data for the "
      elAttr "a" ("href" =: "https://split.apps.tellary.ru")
        $ text "split.apps.tellary.ru"
      text " application"

    app BrowserActionsStore
