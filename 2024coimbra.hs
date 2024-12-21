{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import           BrowserWorkspaceStore
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (encode)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.FileEmbed            (embedFile)
import           Data.JSString             (pack)
import           JavaScript.Web.Storage    (getItem, localStorage, setItem)
import           MoneySplit
import           Reflex.Dom
import           SplitUI
import           WorkspaceStore            (defaultWorkspaceName)

restoreDefaultWorkspace
  = setItem
    (workspaceKey defaultWorkspaceName)
    (pack . UTF8.toString . encode $ actions3)
    localStorage

main :: IO ()
main = do
  getItem (workspaceKey defaultWorkspaceName) localStorage >>= \case
    Nothing -> restoreDefaultWorkspace
    Just _ -> do return ()
  mainWidgetWithCss $(embedFile "split.css") $ do
    restoreEv <- elAttr "span" ("class" =: "notice") $ do
      el "p" $ do
        text "The following page is pre-populated with sample data for the "
        elAttr "a" ("href" =: "https://split.apps.tellary.ru")
          $ text "split.apps.tellary.ru"
        text " application"

      el "p" $ do
        text "The sample data in default workspace may be restored by pressing: "
        el "br" blank
        restoreEv <- button "Restore default workspace"
        performEvent ((liftIO restoreDefaultWorkspace) <$ restoreEv)

    widgetHold
      ( app BrowserWorkspaceStore (\_ -> return ()) (return ()) Nothing )
      ( app
        BrowserWorkspaceStore
        (\_ -> return ()) (return ())
        Nothing
        <$ restoreEv
      )
    return ()
