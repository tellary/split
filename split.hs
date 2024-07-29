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
    let evText = tagPromptlyDyn
                 (value input)
                 (leftmost [evEnter, () <$ submitEvent])
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

manageGroups
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [Text] -> m (Dynamic t [[Text]])
manageGroups users = do
  el "h2" $ text "Manage user groups"
  el "h3" $ text "Select users for the group"
  groupUsers <- selectUsers "groups" users
  addGroupEv <- button "Add group"
  rec
    userGroups <-
      foldDyn ($) []
      ( mergeWith (.)
        [ (:) <$> tagPromptlyDyn groupUsers addGroupEv
        , delete <$> deleteUserGroupEv
        ]
      )
    el "h3" $ text "User groups"
    deleteUserGroupEv <- dynList (T.pack . show) userGroups
  return userGroups
  
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

data ActionType
  = PurchaseSplitEquallyAllActionType
  | PurchaseSplitEquallyActionType
  | PurchaseItemizedSplitActionType
  | PaymentTransactionActionType deriving (Eq, Ord, Show)

addAction
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [Text] -> m (Event t Action)
addAction users = do
  text "Choose action type: "
  dropdownEl <- dropdown Nothing
    ( constDyn
     (    Just PurchaseSplitEquallyAllActionType
          =: "Purchase Split Equally All"
       <> Just PurchaseSplitEquallyActionType
          =: "Purchase Split Equally"
       <> Just PurchaseItemizedSplitActionType
          =: "Purchase Itemized Split"
       <> Just PaymentTransactionActionType
          =: "Payment"
     )) def
  let actionType = value dropdownEl
  switchHold never =<< do
    dyn . ffor actionType $ \case
      Nothing -> return never
      Just PurchaseSplitEquallyAllActionType
        -> fmap (fmap PurchaseAction) $ addSplitAllPurchase users
      Just PurchaseSplitEquallyActionType
        -> fmap (fmap PurchaseAction) $ addSplitEquallyPurchase users
      Just PurchaseItemizedSplitActionType
        -> fmap (fmap PurchaseAction) $ addItemizedSplitPurchase users
      _ -> return never

userInput users = do
  text "User: "
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
  return user

descriptionInput addEv = do
  text "Description: "
  desc <- validInput addEv $ \txt ->
    let txt' = T.strip txt
    in if T.null txt'
       then Left "No description provided"
       else Right txt'
  el "br" blank
  return desc

amountInput addEv = do
  text "Amount: "
  amount <- validInput addEv $ \txt ->
    maybe
    (Left $ "Failed to read amount: " `T.append` txt) Right
    (readMaybe . T.unpack $ txt :: Maybe Amount)
  el "br" blank
  return amount

addSplitAllPurchase
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [Text] -> m (Event t Purchase)
addSplitAllPurchase users = do
  el "h3" $ text "Add \"split all\" purchase"
  user <- userInput users
  rec
    desc   <- descriptionInput addEv
    amount <- amountInput addEv
    addEv  <- button "Add purchase"
  let purchase
        = Purchase
        <$> fmap (T.unpack) user
        <*> fmap (T.unpack) desc
        <*> amount
        <*> pure SplitEquallyAll
  tagOnSubmit purchase addEv

selectUsers  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Dynamic t [Text] -> m (Dynamic t [Text])
selectUsers idPrefix users
  = dynToDyn [] . ffor users $ \users -> do
      selectedCbs :: Dynamic t [Bool] <-
        fmap sequence . forM users $ \user -> do
          cb <- inputElement
            $ def
            & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ (  "type" =: "checkbox"
               <> "id" =: idPrefix `T.append` "_" `T.append` user)
          elAttr "label" ("for" =: "split_" `T.append` user) $ do
            text " "
            text user
          el "br" blank
          return . _inputElement_checked $ cb
      return . fmap (map snd . filter fst)
        $ (zip <$> selectedCbs <*> pure users)

addSplitEquallyPurchase
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [Text] -> m (Event t Purchase)
addSplitEquallyPurchase users = do
  el "h3" $ text "Add \"split equally\" purchase"
  user <- userInput users
  rec
    desc   <- descriptionInput addEv
    amount <- amountInput addEv
    selectedUsers :: ValidInput t [Text]
        <- fmap (ExceptT . fmap Right) $ selectUsers "split" users
    addEv <- button "Add purchase"
  let purchase
        = Purchase
        <$> fmap (T.unpack) user
        <*> fmap (T.unpack) desc
        <*> amount
        <*> (SplitEqually . fmap T.unpack <$> selectedUsers)
  tagOnSubmit purchase addEv

addSplitItem
  ::(DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [Text] -> m (Event t SplitItem)
addSplitItem users = do
  user   <- userInput users
  rec
    desc   <- descriptionInput addEv
    amount <- amountInput addEv
    addEv  <- button "Add split item"
  let splitItem
        = SplitItem
        <$> fmap (T.unpack) user
        <*> fmap (T.unpack) desc
        <*> amount
  tagOnSubmit splitItem addEv
  
manageSplitItems :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [Text] -> m (Dynamic t [SplitItem])
manageSplitItems users = do
  addSplitItemEv <- addSplitItem users
  rec
    splitItems <-
      foldDyn ($) []
      ( mergeWith (.)
        [ (:) <$> addSplitItemEv
        , delete <$> deleteSplitItemEv
        ]
      )
    el "h5" $ text "Split items"
    deleteSplitItemEv <- dynList (T.pack . show) splitItems
  return splitItems

addItemizedSplitPurchase
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [Text] -> m (Event t Purchase)
addItemizedSplitPurchase users = do
  el "h3" $ text "Add \"itemized split\" purchase"
  user <- userInput users
  rec
    desc       <- descriptionInput addEv
    text "Amount: "
    let amount = fmap (sum . map splitItemAmount) $ splitItems
    dynText . fmap (T.pack . show) $ amount
    splitItems <- 
      elAttr "table" ("class" =: "nested") $ do
        splitItems <- el "tr" $ do
          elAttr "td" ("class" =: "nested") $ do
            text "Split items: "
          elAttr "td" ("rowspan" =: "2" <> "class" =: "nested") $ do
            manageSplitItems users
        el "tr" $ do
          el "td" $ blank
        return splitItems
    addEv <- button "Add purchase"
  let purchase
        = Purchase
        <$> fmap (T.unpack) user
        <*> fmap (T.unpack) desc
        <*> (ExceptT . fmap (Right . sum . map splitItemAmount) $ splitItems)
        <*> (ExceptT . fmap (Right . ItemizedSplit) $ splitItems)
  tagOnSubmit purchase addEv

manageActions
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [Text] -> Dynamic t [[Text]] -> m (Dynamic t Actions)
manageActions users groups = do
  el "h2" $ text "Manage actions"
  addActionEv <- addAction users
  rec
    actions <-
      foldDyn ($) []
      ( mergeWith (.)
        [ (:) <$> addActionEv
        , delete <$> deleteActionEv
        ]
      )
    el "h3" $ text "Actions list"
    deleteActionEv <- dynList (T.pack . show) actions
  return
    $   Actions
    <$> fmap (map T.unpack) users
    <*> fmap (map (map T.unpack)) groups
    <*> actions

main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") $ do
  users <- manageUsers
  groups <- manageGroups users
  actions <- manageActions users groups
  let nullified = (nullifyBalances . actionsToTransactions) <$> actions
  el "h2" $ text "Report"
  dyn (report <$> actions <*> nullified)
  return ()

