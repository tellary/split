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
import           Reflex.Dom                 hiding (Group)
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
  => (a -> m ()) -> Dynamic t [a] -> m (Event t a)
dynList showF itemsDyn = switchHold never =<< dyn listWidget
  where
    listWidget :: DomBuilder t m => Dynamic t (m (Event t a))
    listWidget = ffor itemsDyn $ \items -> do
      deleteEvents <- el "ul" . forM items $ \item -> do
        el "li" $ do
          showF $ item
          deleteX item
      return . leftmost $ deleteEvents

deleteX item = do
  text " ["
  (deleteItemEl, _) <- elAttr' "a" ("class" =: "link") $ text "X"
  text "]"
  return (item <$ domEvent Click deleteItemEl)  

manageUsers
  :: (Reflex t, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => m (Dynamic t [User])
manageUsers = do
  el "h2" $ text "Manage users"
  rec
    addUserEv <- fmap (fmap T.unpack) . resettableInput $ addUserButtonEv
    addUserButtonEv <- button "Add user"
    users <-
      foldDyn ($) []
      ( mergeWith (.)
        [ (:) <$> addUserEv
        , delete <$> deleteUserEv
        ]
      )
    deleteUserEv <- dynList (text . T.pack) users
  return users

manageGroups
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> m (Dynamic t [[User]])
manageGroups users = do
  el "h2" $ text "Manage user groups"
  el "h3" $ text "Select users for the group"
  rec
    let usersNotInGroups = users >>= \users ->
          userGroups >>= \groups ->
            return
            . filter (not . \user -> any (\group -> user `elem` group) groups)
            $ users
    newGroupUsers <- selectUsers "groups" usersNotInGroups
    usersNotInGroupsOnNewUsers <-
      holdDyn [] ((tag . current) usersNotInGroups (updated newGroupUsers))
    let validation :: [User] -> Dynamic t (Either Text [User])
          = \newGroupUsers ->
              ffor usersNotInGroupsOnNewUsers $ \usersNotInGroups ->
                if null usersNotInGroups
                then Left ""
                else case newGroupUsers of
                       []  -> Left $ "No users selected. "
                              `T.append` "At least 2 users necessary for a group"
                       [_] -> Left $ "Only one user is selected. "
                              `T.append` "At least 2 users necessary for a group"
                       (_) -> Right $ newGroupUsers
    let newGroupUsersValid :: ValidInput t [User]
          = ExceptT $ validation =<< newGroupUsers
    addGroupButtonEv <- button "Add group"
    let addGroupEv = tagValid newGroupUsersValid addGroupButtonEv
    let error = fmap (either id (const "")) . runExceptT $ newGroupUsersValid
    dynText =<< holdDyn "" (tagPromptlyDyn error addGroupButtonEv)
    userGroups :: Dynamic t [[User]] <-
      foldDyn ($) []
      ( mergeWith (.)
        [ (:)    <$> addGroupEv
        , delete <$> deleteGroupEv
        ]
      )
    el "h3" $ text "User groups"
    deleteGroupEv <-
      dynList (text . T.pack . printUsersList) userGroups
  return userGroups

type ValidInput t a = ExceptT Text (Dynamic t) a

-- | Assume the dynamic value is valid without validation
validDyn :: Reflex t => Dynamic t a -> ValidInput t a
validDyn d = ExceptT (Right <$> d)

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

tagValid :: Reflex t => ValidInput t a -> Event t b -> Event t a
tagValid errorOrValueT submitEvent
  = mapMaybe id
  . fmap (either (const Nothing) Just) -- Event Either -> Event Maybe
  . tagPromptlyDyn (runExceptT errorOrValueT)
  $ submitEvent

dynToDyn :: (DomBuilder t m, MonadHold t m, PostBuild t m)
  => a -> Dynamic t (m (Dynamic t a)) -> m (Dynamic t a)
dynToDyn initVal dynWidget =
  join <$> (holdDyn (constDyn initVal) =<< dyn dynWidget)

dynToEvent
  :: (Adjustable t m, NotReady t m, PostBuild t m, MonadHold t m)
  => Dynamic t (m (Event t a)) -> m (Event t a)
dynToEvent dynWidget = switchHold never =<< dyn dynWidget

data ActionType
  = PurchaseSplitEquallyAllActionType
  | PurchaseSplitEquallyActionType
  | PurchaseItemizedSplitActionType
  | PaymentTransactionActionType deriving (Eq, Ord, Show)

addAction
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> Dynamic t [Group] -> m (Event t Action)
addAction users groups = do
  text "Choose action type: "
  dropdownEl <- dropdown PurchaseSplitEquallyAllActionType
    ( constDyn
     (    PurchaseSplitEquallyAllActionType
          =: "Purchase Split Equally All"
       <> PurchaseSplitEquallyActionType
          =: "Purchase Split Equally"
       <> PurchaseItemizedSplitActionType
          =: "Purchase Itemized Split"
       <> PaymentTransactionActionType
          =: "Payment"
     )) def
  let actionType = value dropdownEl
  switchHold never =<< do
    dyn . ffor actionType $ \case
      PurchaseSplitEquallyAllActionType
        -> fmap (fmap PurchaseAction) $ addSplitAllPurchase users
      PurchaseSplitEquallyActionType
        -> fmap (fmap PurchaseAction) $ addSplitEquallyPurchase users
      PurchaseItemizedSplitActionType
        -> fmap (fmap PurchaseAction) $ addItemizedSplitPurchase users
      PaymentTransactionActionType
        -> fmap (fmap PaymentAction)  $ addPaymentTransaction users groups

userInput
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Dynamic t [User] -> m (ValidInput t User)
userInput label users = do
  text $ label `T.append` ": "
  user :: ValidInput t User <- ExceptT <$> dynToDyn
    ( Left "" )
    ( ffor users $ \users -> do
        if null users
          then do
            text "Please add a user first"
            return . constDyn . Left $ "Please add a user first"
          else do
            el <- dropdown
                  (head users)
                  (constDyn . M.fromList $ zip users (map T.pack users))
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
  => Dynamic t [User] -> m (Event t Purchase)
addSplitAllPurchase users = do
  el "h3" $ text "Add \"split all\" purchase"
  user <- userInput "User" users
  rec
    desc   <- descriptionInput addEv
    amount <- amountInput addEv
    addEv  <- button "Add purchase"
  let purchase
        = Purchase
        <$> user
        <*> fmap (T.unpack) desc
        <*> amount
        <*> pure SplitEquallyAll
  return $ tagValid purchase addEv

selectUsers  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => String -> Dynamic t [User] -> m (Dynamic t [User])
selectUsers idPrefix users
  = dynToDyn [] . ffor users $ \users -> do
      selectedCbs :: Dynamic t [Bool] <-
        fmap sequence . forM users $ \user -> do
          cb <- inputElement
            $ def
            & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ (  "type" =: "checkbox"
               <> "id" =: T.pack (idPrefix ++ "_" ++ user))
          elAttr "label"
              ("for" =: T.pack (idPrefix ++ "_" ++ user)) $ do
            text " "
            text . T.pack $ user
          el "br" blank
          return . _inputElement_checked $ cb
      return . fmap (map snd . filter fst)
        $ (zip <$> selectedCbs <*> pure users)

addSplitEquallyPurchase
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> m (Event t Purchase)
addSplitEquallyPurchase users = do
  el "h3" $ text "Add \"split equally\" purchase"
  user <- userInput "User" users
  rec
    desc   <- descriptionInput addEv
    amount <- amountInput addEv
    selectedUsers :: ValidInput t [User]
        <- fmap (ExceptT . fmap Right) $ selectUsers "split" users
    addEv <- button "Add purchase"
  let purchase
        = Purchase
        <$> user
        <*> fmap (T.unpack) desc
        <*> amount
        <*> (SplitEqually <$> selectedUsers)
  return $ tagValid purchase addEv

addSplitItem
  ::(DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> m (Event t SplitItem)
addSplitItem users = do
  user   <- userInput "User" users
  rec
    desc   <- descriptionInput addEv
    amount <- amountInput addEv
    addEv  <- button "Add split item"
  let splitItem
        = SplitItem
        <$> user
        <*> fmap (T.unpack) desc
        <*> amount
  return $ tagValid splitItem addEv

splitWidget item = do
  text . T.pack . splitItemUser $ item
  text ", "
  text . T.pack . splitItemDesc $ item
  text " -- "
  text . T.pack . show . splitItemAmount $ item

manageSplitItems :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> m (Dynamic t [SplitItem])
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
    deleteSplitItemEv <- dynList splitWidget splitItems
  return splitItems

addItemizedSplitPurchase
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> m (Event t Purchase)
addItemizedSplitPurchase users = do
  el "h3" $ text "Add \"itemized split\" purchase"
  user <- userInput "User" users
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
        <$> user
        <*> fmap (T.unpack) desc
        <*> (ExceptT . fmap (Right . sum . map splitItemAmount) $ splitItems)
        <*> (ExceptT . fmap (Right . ItemizedSplit) $ splitItems)
  return $ tagValid purchase addEv

addPaymentTransaction
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> Dynamic t [Group] -> m (Event t Transaction)
addPaymentTransaction users groups = do
  el "h3" $ text "Add payment"
  debitUser  <- userInput "Debit User"  users
  creditUser <- userInput "Credit User" users
  rec
    amount <- amountInput addEv
    addEv  <- button "Add payment"
    let transaction = do
          groups <- validDyn groups
          let groupsByUsersVal = groupsByUsers groups
          debitAccount <- userToAccount groupsByUsersVal <$> debitUser
          creditAccount <- userToAccount groupsByUsersVal <$> creditUser
          amount <- amount
          return
            $ Transaction debitAccount creditAccount amount TxReasonPayment
  return $ tagValid transaction addEv

actionWidgetPayedFor
    ( PurchaseAction
      ( Purchase
        { purchaseUser = purchaseUser
        , purchaseDesc = purchaseDesc
        , purchaseAmount = purchaseAmount
        }
      )
    ) = do
  text . T.pack $ purchaseUser
  text " payed "
  text . T.pack . show $ purchaseAmount
  text " for \""
  text . T.pack $ purchaseDesc
  text "\""
actionWidgetPayedFor _
  = error "actionWidgetPayedFor: only implemented for purchases so far"

actionWidget
    action@( PurchaseAction
      ( Purchase { purchaseSplit = SplitEquallyAll } )
    ) = do
  actionWidgetPayedFor action
  text " split equally to all"
actionWidget
    action@( PurchaseAction
      ( Purchase { purchaseSplit = SplitEqually [user] } )
    ) = do
  actionWidgetPayedFor action
  text " for "
  text . T.pack $ user
actionWidget
    action@( PurchaseAction
      ( Purchase { purchaseSplit = SplitEqually users } )
    ) = do
  actionWidgetPayedFor action
  text " split equally to "
  text . T.pack . printUsersList $ users
actionWidget
    action@( PurchaseAction
      ( Purchase { purchaseSplit = ItemizedSplit splits } )
    ) = do
  actionWidgetPayedFor action
  text " split in "
  text . T.pack . show . length $ splits
  text " items"
actionWidget a = do
  text . T.pack . show $ a

manageActions
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> Dynamic t [[User]] -> m (Dynamic t Actions)
manageActions users groups = do
  el "h2" $ text "Manage actions"
  rec
    addActionEv <- addAction users groups
    actionsList <-
      foldDyn ($) []
      ( mergeWith (.)
        [ (:) <$> addActionEv
        , delete <$> deleteActionEv
        ]
      )
    let actions = Actions
                  <$> users
                  <*> groups
                  <*> actionsList
    el "h3" $ text "Actions list"
    deleteActionEv <- dynList actionWidget actionsList
  return actions

main :: IO ()
main = mainWidgetWithCss $(embedFile "split.css") $ do
  users <- manageUsers
  groups <- manageGroups users
  actions <- manageActions users groups
  let nullified = (nullifyBalances . actionsToTransactions) <$> actions
  el "h2" $ text "Report"
  dyn (report <$> actions <*> nullified)
  return ()

