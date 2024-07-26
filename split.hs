{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import           Control.Monad              (forM, join)
import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import           Data.FileEmbed             (embedFile)
import           Data.List                  (delete)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           MoneySplit
import           Reflex.Dom
import           SplitReport
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

dynList :: forall t m a . (DomBuilder t m, MonadHold t m, PostBuild t m)
  => (a -> Text) -> Dynamic t [a] -> m (Event t a)
dynList showF itemsDyn = switchHold never =<< dyn listWidget
  where
    listWidget :: DomBuilder t m => Dynamic t (m (Event t a))
    listWidget = ffor itemsDyn $ \items -> do
      deleteEvents <- el "ul" . forM items $ \item -> do
        el "li" $ do
          text . showF $ item
          text " ["
          (deleteItemEl, _) <- elAttr' "a" ("class" =: "link") $ text "X"
          text "]"
          return (item <$ domEvent Click deleteItemEl)
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
        [ (:) <$> addUserEv
        , delete <$> deleteUserEv
        ]
      )
    deleteUserEv <- dynList id users
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
      ( leftmost [() <$ submitEvent, () <$ updated inputValue] )
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

dynToDyn :: (DomBuilder t m, MonadHold t m, PostBuild t m)
  => a -> Dynamic t (m (Dynamic t a)) -> m (Dynamic t a)
dynToDyn initVal dynWidget =
  join <$> (holdDyn (constDyn initVal) =<< dyn dynWidget)

addSplitAllPurchase
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [Text] -> m (Event t Purchase)
addSplitAllPurchase users = do
  el "h3" $ text "Add purchase"
  text "User: "
  rec
    user :: ValidInput t Text <- ExceptT <$> dynToDyn
            (Left "")
            ( ffor users $ \users -> do
                if null users
                  then do
                    text "Please add a user first"
                    return . constDyn . Left $ "Please add a user first"
                  else do
                    el <- dropdown
                          (head users)
                          (constDyn . M.fromList $ zip users users)
                          def
                    return $ Right <$> value el
            )
    el "br" blank
    text "Description: "
    desc <- validInput addEv $ \txt ->
      let txt' = T.strip txt
      in if T.null txt'
         then Left "No description provided"
         else Right txt'
    el "br" blank
    text "Amount: "
    amount <- validInput addEv $ \txt ->
      maybe
      (Left $ "Failed to read amount: " `T.append` txt) Right
      (readMaybe . T.unpack $ txt :: Maybe Amount)
    el "br" blank
    addEv <- button "Add purchase"
  let purchase
        = Purchase
        <$> fmap (T.unpack) user
        <*> fmap (T.unpack) desc
        <*> amount
        <*> pure SplitEquallyAll
  tagOnSubmit purchase addEv

manageActions
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [Text] -> m (Dynamic t Actions)
manageActions users = do
  el "h2" $ text "Manage actions"
  addPurchaseEv <- addSplitAllPurchase users
  rec
    purchases <-
      foldDyn ($) []
      ( mergeWith (.)
        [ (:) <$> addPurchaseEv
        , delete <$> deletePurchaseEv
        ]
      )
    el "h3" $ text "Actions list"
    deletePurchaseEv <- dynList (T.pack . show) purchases
  return $ do
    usersVal <- users
    purchasesVal <- purchases
    return $ Actions (map T.unpack usersVal) [] (map PurchaseAction purchasesVal)

main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") $ do
  users <- manageUsers
  actions <- manageActions users
  let nullified = (nullifyBalances . actionsToTransactions) <$> actions
  el "h2" $ text "Report"
  dyn (report <$> actions <*> nullified)
  return ()

