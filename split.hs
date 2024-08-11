{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import           Control.Monad     (forM, join)
import           Control.Monad.Fix (MonadFix)
import           Data.Either       (fromLeft, fromRight, isLeft, isRight)
import           Data.FileEmbed    (embedFile)
import           Data.List         (delete, find, (\\))
import qualified Data.Map          as M
import           Data.Maybe        (isNothing)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.These        (These (These))
import           MoneySplit
import           Reflex.Dom        hiding (Group)
import           SplitReport
import           Text.Printf       (printf)
import           Text.Read         (readMaybe)
import           ValidDynamic      (ErrorDynamic, ValidDynamic, errorDyn,
                                    fromDynamic, fromDynamicEither, tagValid,
                                    validDyn)

resettableInput
  :: forall t m a b c . (Show c, DomBuilder t m, MonadHold t m, MonadFix m)
  => Event t a -> Event t b -> (Text -> Either Text c)
  -> m (ErrorDynamic t, Event t c)
resettableInput submitEvent resetEvent validation = do
  rec
    input <- inputElement $ def & inputElementConfig_setValue .~ ("" <$ evText)
    let evEnter = keypress Enter input
    let submitOrEnterEv = leftmost [evEnter, () <$ submitEvent]
    validInput :: ValidDynamic t c <-
      validDyn
        "" 0
        (1 <$ submitOrEnterEv)
        (0 <$ resetEvent)
        (updated . value $ input)
        validation
    let evText :: Event t c
          = tagValid validInput submitOrEnterEv
  error <- errorDyn submitEvent validInput
  return (error, evText)

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

tupleToThese (a, b) = These a b

fanTuple :: Reflex t => Event t (a, b) -> (Event t a, Event t b)
fanTuple = fanThese . fmap tupleToThese

manageUsers
  :: forall t m . (Reflex t, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t Actions -> m (Dynamic t [User])
manageUsers actions = do
  el "h2" $ text "Manage users"
  rec
    let errorAndAddUserEvDyn :: Dynamic t (m (ErrorDynamic t, Event t User))
          = (users >>= \users ->
                return . resettableInput addUserButtonEv deleteUserEv
                $ \user ->
                    let userStr = T.unpack . T.strip $ user
                    in if null $ userStr
                       then Left $ "Empty users names are not allowed"
                       else if userStr `elem` users
                            then Left (T.pack $ printf "User '%s' already exists" userStr)
                            else Right userStr )
    (   errorDynEv  :: Event t (ErrorDynamic t)
      , addUserEvEv :: Event t (Event t User)
      ) <- fanTuple <$> dyn errorAndAddUserEvDyn
    addUserEv <- switchHold never addUserEvEv
    errorEv :: Event t Text <- switchHold never (updated <$> errorDynEv)
    errorTextDyn <- holdDyn "" errorEv
    addUserButtonEv <- button "Add user"
    text " "
    dynText errorTextDyn
    users :: Dynamic t [User] <-
      foldDyn ($) []
      ( mergeWith (.)
        [ (:) <$> addUserEv
        , delete <$> deleteUserEv
        ]
      )
    deleteUserEv <- el "ul" $ simpleListOneEvent users (userListItem actions)
  return users

simpleListOneEvent
  :: (Eq item, Adjustable t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [item] -> (Dynamic t item -> m (Event t item))
  -> m (Event t item)
simpleListOneEvent items itemWidget
  = switchDyn <$> (simpleList items itemWidget >>=
                    \evs -> return $ (leftmost <$> evs))

userListItem :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m)
  => Dynamic t Actions -> Dynamic t User -> m (Event t User)
userListItem actions user = el "li" $ do
  dynText (T.pack <$> user)
  deleteUserEv <- switchHold never =<< (dyn $ deleteX <$> user)
  let deleteUserOrErrorEv :: Event t (Either Text User)
        = switchDyn . ffor actions $ \actions ->
            ffor deleteUserEv $ \user ->
              case find (isUserAction user) (actionsArr actions) of
                Just userAction
                  -> Left . T.pack
                  $ printf "Can't delete user '%s' because '%s' action exists"
                    user (show userAction)
                Nothing -> Right user
  let deleteValidUserEv :: Event t User
        = fmap (fromRight (error "Already checked for 'isRight'"))
        . ffilter isRight $ deleteUserOrErrorEv
  let deleteUserErrorEv :: Event t Text
        = fmap (fromLeft (error "Already checked for 'isLeft'"))
        . ffilter isLeft $ deleteUserOrErrorEv
  text " "
  dynText =<< holdDyn "" deleteUserErrorEv
  return deleteValidUserEv

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
    newGroupUsersValid :: ValidDynamic t [User]
      <- validDyn
         [] 1
         (1 <$ addGroupButtonEv)
         (1 <$ leftmost [deleteGroupEv, updated users] )
         (updated $ newGroupUsers)
         $ \case
             [ ] -> Left $ "No users selected. "
                    `T.append` "At least 2 users necessary for a group"
             [_] -> Left $ "Only one user is selected. "
                    `T.append` "At least 2 users necessary for a group"
             gs  -> Right $ gs
    addGroupButtonEv <- button "Add group"
    let addGroupEv = tagValid newGroupUsersValid addGroupButtonEv
    dynText =<< errorDyn addGroupButtonEv newGroupUsersValid
    userGroups :: Dynamic t [Group] <-
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

validInput
  :: (Show a, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Event t b -> (Text -> Either Text a)
  -> m (ValidDynamic t a)
validInput submitEvent validation = do
  inputValue <- value <$> inputElement def
  inputValueValid
    <- validDyn "" 0 (0 <$ submitEvent) never (updated inputValue) validation
  text " "
  dynText =<< errorDyn submitEvent inputValueValid
  return inputValueValid

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
        -> addPaymentTransaction users groups

userInput
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Dynamic t [User] -> m (ValidDynamic t User)
userInput label users = do
  text $ label `T.append` ": "
  user <- fromDynamicEither <$> dynToDyn
    ( Left "" )
    ( ffor users $ \users -> do
        if length users < 2
          then do
            let msg = "At least two users required, "
                      `T.append` "please add users in \"Manage users\""
            text msg
            return . constDyn . Left $ msg
          else do
            el <- dropdown
                  (head users)
                  (constDyn . M.fromList $ zip users (map T.pack users))
                  def
            return $ Right <$> value el
    )
  el "br" blank
  return user

descriptionInput :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Event t a -> m (ValidDynamic t Text)
descriptionInput addEv = do
  text "Description: "
  desc <- validInput addEv $ \txt ->
    let txt' = T.strip txt
    in if T.null txt'
       then Left "No description provided"
       else Right txt'
  el "br" blank
  return desc

amountInput :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Event t a -> m (ValidDynamic t Amount)
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
        fmap distributeListOverDyn . forM users $ \user -> do
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
    desc          <- descriptionInput addEv
    amount        <- amountInput addEv
    selectedUsers <- fromDynamic <$> selectUsers "split" users
    addEv         <- button "Add purchase"
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
        <*> (fmap (sum . map splitItemAmount) . fromDynamic $ splitItems)
        <*> (fmap ItemizedSplit . fromDynamic $ splitItems)
  return $ tagValid purchase addEv

preselectedUsersDropdown users selectedUser =
  case selectedUser of
    Nothing
      -> dropdown
         ""
         (constDyn . M.fromList $ [])
         $ def
    Just selectedUser
      -> dropdown
         selectedUser
         (constDyn . M.fromList $ zip users (map T.pack users))
         $ def

notSelectedUserDropdown
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => [User] -> Maybe User -> Event t [User] -> m (Dynamic t User)
notSelectedUserDropdown users init selectedUsers = mdo
  elDyn <- widgetHold
           (preselectedUsersDropdown users init)
           (preselectedUsersDropdown users <$> notSelectedUserEv)
  let val = join $ value <$> elDyn
  let usersEv = attach (current val) selectedUsers
  let notSelectedUserEv
        = fmap (
            \(_, selectedUsers) -> case users \\ selectedUsers of
                       [] -> Nothing
                       x:_ -> Just x
            )
        . ffilter (\(val, selectedUsers) -> val `elem` selectedUsers)
        $ usersEv
  return val

addPaymentTransaction0
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => [User] -> [Group] -> m (Event t Action)
addPaymentTransaction0 users groups =
  if length users < 2
  then do
    let msg = "At least two users required, "
              `T.append` "please add users in \"Manage users\""
    text msg
    return never
  else
    let maybeSecondUser = findUserNotInTheSameGroup users groups (head users)
    in if isNothing maybeSecondUser
       then do
         let msg = "All users should not belong to the same group, "
                   `T.append` "please add users in \"Manage users\" or "
                   `T.append` "remove groups in \"Manage groups\""
         text msg
         return never
       else mdo
         text "Debit user: "
         let firstUser = head users
         debitUser :: Dynamic t User
           <- notSelectedUserDropdown
              users (Just firstUser)
              (currentGroupOrUser groups <$> updated creditUser)
         el "br" blank
         text "Credit user: "

         creditUser :: Dynamic t User
           <- notSelectedUserDropdown
              users maybeSecondUser
              (currentGroupOrUser groups <$> updated debitUser)
         el "br" blank
         amount <- amountInput addEv
         addEv  <- button "Add payment"
         let action
               = PaymentAction
                 <$> (fromDynamic debitUser)
                 <*> (fromDynamic creditUser)
                 <*> amount
         return $ tagValid action addEv

addPaymentTransaction
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> Dynamic t [Group] -> m (Event t Action)
addPaymentTransaction users groups = do
  el "h3" $ text "Add payment"
  switchHold never =<< (dyn $ do
    users <- users
    groups <- groups
    return $ addPaymentTransaction0 users groups
    )

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
actionWidget ( PaymentAction debitUser creditUser amount ) = do
  text . T.pack $ debitUser
  text " payed "
  text . T.pack $ creditUser
  text " "
  text . T.pack . show $ amount

manageActions
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> Dynamic t [Group] -> m (Dynamic t Actions)
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
  rec 
    users <- manageUsers actions
    groups <- manageGroups users
    actions <- manageActions users groups
  let nullified = (nullifyBalances . actionsToTransactions) <$> actions
  el "h2" $ text "Report"
  dyn (report <$> actions <*> nullified)
  return ()

