{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import Data.List (delete)
import Control.Monad.Fix
import Control.Monad (forM)
import Data.FileEmbed (embedFile)
import Reflex.Dom --     (el, mainWidgetWithCss, text)
import Data.Text (Text)

addUserInput :: (DomBuilder t m, MonadHold t m, MonadFix m) => m (Event t Text)
addUserInput = do
  rec
    input <- inputElement $ def & inputElementConfig_setValue .~ ("" <$ evText)
    let evEnter = keypress Enter input
    let evText = tagPromptlyDyn (value input) evEnter
  return evText

displayUsers :: DomBuilder t m => [Text] -> m (Event t Text)
displayUsers users = do
  deleteEvents <- el "ul" . forM users $ \user -> do
    el "li" $ do
      text user
      text " ["
      (deleteUserEl, _) <- elAttr' "a" ("class" =: "link") $ text "X"
      text "]"
      return (user <$ domEvent Click deleteUserEl)
  return . leftmost $ deleteEvents

manageUsers
  :: (Reflex t, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => m (Dynamic t [Text])
manageUsers = do
  addUserEv <- addUserInput
  rec
    users <-
      foldDyn ($) []
      ( mergeWith (.)
        [ ((:) <$> addUserEv)
        , delete <$> deleteUserEv
        ]
      )
    deleteUserEv <- switchHold never =<< dyn (displayUsers <$> users)
  return users
  
main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") $ do
  manageUsers
  return ()

