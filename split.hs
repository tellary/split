{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import           Control.Monad              (forM)
import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import           Data.FileEmbed             (embedFile)
import           Data.List                  (delete)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           MoneySplit
import           Reflex.Dom
import           Text.Read                  (readMaybe)

resettableInput
  :: (DomBuilder t m, MonadHold t m, MonadFix m)
  => Event t a -> m (Event t Text)
resettableInput submitEvent = do
  rec
    input <- inputElement $ def & inputElementConfig_setValue .~ ("" <$ evText)
    let evEnter = keypress Enter input
    let evText = tagPromptlyDyn (value input) (leftmost [evEnter, () <$ submitEvent])
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
  el "h2" $ text "Manage users"
  rec
    addUserEv <- resettableInput addUserButtonEv
    addUserButtonEv <- button "Add user"
    users <-
      foldDyn ($) []
      ( mergeWith (.)
        [ ((:) <$> addUserEv)
        , delete <$> deleteUserEv
        ]
      )
    deleteUserEv <- switchHold never =<< dyn (displayUsers <$> users)
  return users

type ValidInput t a = ExceptT Text (Dynamic t) a

validInput
  :: (DomBuilder t m, MonadHold t m, PostBuild t m)
  => Event t b -> (Text -> Either Text a) -> m (ValidInput t a)
validInput submitEvent validation = do
  inputValue :: Dynamic t Text <- value <$> inputElement def
  let errorOrValue = fmap validation inputValue
  let error = fmap (either id (const "")) errorOrValue
  errorOnSubmitOrChange <-
    holdDyn ""
    ( tagPromptlyDyn error
      ( leftmost [() <$ submitEvent, () <$ updated inputValue]
      )
    )
  text " "
  dynText errorOnSubmitOrChange
  return . ExceptT $ errorOrValue

tagOnSubmit :: DomBuilder t m => ValidInput t a -> Event t b -> m (Event t a)
tagOnSubmit errorOrValueT submitEvent
  = return
  . mapMaybe id
  . fmap (either (const Nothing) Just) -- Event Either -> Event Maybe
  . tagPromptlyDyn (runExceptT errorOrValueT)
  $ submitEvent

addSplitAllPurchase
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => m (Event t Purchase)
addSplitAllPurchase = do
  el "h2" $ text "Add purchase"
  el "br" blank
  text "User: "
  user <- value <$> inputElement def
  el "br" blank
  text "Description: "
  desc <- value <$> inputElement def
  el "br" blank
  text "Amount: "
  rec
    amount <- validInput addEv $ \txt ->
      maybe
      (Left $ "Failed to read amount: " `T.append` txt) Right
      (readMaybe . T.unpack $ txt :: Maybe Amount)
    el "br" blank
    addEv <- button "Add purchase"
  let purchase
        = Purchase
        <$> (ExceptT . fmap (Right . T.unpack) $ user)
        <*> (ExceptT . fmap (Right . T.unpack) $ desc)
        <*> amount
        <*> pure SplitEquallyAll
  tagOnSubmit purchase $ addEv

manageActions = undefined
  
main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") $ do
  manageUsers
  dynText =<< holdDyn "" =<< fmap (T.pack . show) <$> addSplitAllPurchase
  return ()

