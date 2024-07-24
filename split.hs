{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import           Control.Monad     (forM)
import           Control.Monad.Fix (MonadFix)
import           Data.FileEmbed    (embedFile)
import           Data.List         (delete)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           MoneySplit
import           Reflex.Dom
import           Text.Read         (readMaybe)

resettableInput
  :: (DomBuilder t m, MonadHold t m, MonadFix m)
  => m (Event t Text)
resettableInput = do
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
  addUserEv <- resettableInput
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

addSplitAllPurchase
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => m (Event t Purchase)
addSplitAllPurchase = do
  text "Add purchase"
  el "br" blank
  text "User: "
  user <- value <$> inputElement def
  el "br" blank
  text "Description: "
  desc <- value <$> inputElement def
  el "br" blank
  text "Amount: "
  rec
    amount <- fmap (readMaybe . T.unpack) <$> value <$> inputElement def
    amountOnPress <- holdDyn (Just 0) (tagPromptlyDyn amount addEv)
    text " "
    dynText . ffor amountOnPress $ \case
      Nothing -> "Bad amount"
      Just _ -> ""
    el "br" blank
    addEv <- button "Add purchase"
    let purchase
          = do
              userVal <- user
              descVal <- desc
              amountMaybe <- amount
              return
                $   Purchase
                <$> pure (T.unpack userVal)
                <*> pure (T.unpack descVal)
                <*> amountMaybe
                <*> pure SplitEquallyAll
    let purchaseMaybeEv = tagPromptlyDyn purchase $ addEv
  return . traceEvent "add purchase" . mapMaybe id $ purchaseMaybeEv

manageActions = undefined
  
main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") $ do
  manageUsers
  dynText =<< holdDyn "" =<< fmap (T.pack . show) <$> addSplitAllPurchase
  return ()

