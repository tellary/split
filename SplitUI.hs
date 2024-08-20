{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module SplitUI where

import           ActionsStore           (ActionsStore, getActions, putActions)
import           Control.Monad          (forM, join)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Data.List              (delete, find, (\\))
import qualified Data.Map               as M
import           Data.Maybe             (isJust)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.These             (These (These))
import           MoneySplit             hiding (addTips)
import           Reflex.Dom             hiding (Group)
import           SplitReport
import           Text.Printf            (printf)
import           Text.Read              (readMaybe)
import           ValidDynamic           (ValidDynamic, assumeValidDynamic,
                                         dropValidDynamic, errorDyn,
                                         errorWidget, fromDynamic,
                                         fromDynamicEither, fromEvent,
                                         tagValid, unwrapValidDynamicWidget)

resettableInput
  :: forall t m a b . (Show b, DomBuilder t m, MonadHold t m, MonadFix m)
  => Event t a -> (Text -> Either Text b)
  -> m (Event t b, ValidDynamic t Text b)
resettableInput submitEvent validation = do
  rec
    input <- inputElement $ def & inputElementConfig_setValue .~ ("" <$ ev)
    let evEnter = keypress Enter input
    let submitOrEnterEv = leftmost [evEnter, () <$ submitEvent]
    let validInput :: ValidDynamic t Text b
          = fromDynamic (value $ input) validation 
    let ev :: Event t b
          = tagValid validInput submitOrEnterEv
  return (ev, validInput)

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
  :: forall t m .
     (Reflex t, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => [User] -> Dynamic t Actions -> m (Dynamic t [User])
manageUsers users0 actions = do
  el "h2" $ text "Manage users"
  rec
    let addUserEvDyn :: Dynamic t (m (Event t User)) = do
        users <- users
        let widget :: m (Event t User) = mdo
              (ev, userInput) <- resettableInput addUserButtonEv $
                \user ->
                  let userStr = T.unpack . T.strip $ user
                  in if null $ userStr
                  then Left $ "Empty users names are not allowed"
                  else if userStr `elem` users
                       then Left
                            . T.pack
                            $ printf "User '%s' already exists" userStr
                       else Right userStr
              addUserButtonEv <- button "Add user"
              text " "
              dynText =<< (errorDyn "" addUserButtonEv $ userInput)
              return $ tagValid userInput ev
        return widget
    addUserEv <- switchHold never =<< dyn addUserEvDyn
    users :: Dynamic t [User] <-
      foldDyn ($) users0
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

data UserDeletionErr
  = UserInActionErr User Action
  | UserInGroupErr User Group
  deriving Show

userListItem :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m)
  => Dynamic t Actions -> Dynamic t User -> m (Event t User)
userListItem actions user = el "li" $ do
  dynText (T.pack <$> user)
  deleteUserEv <- switchHold never =<< (dyn $ deleteX <$> user)
  deleteUserValid <- unwrapValidDynamicWidget "" $ do
    actionsArrVal <- actionsArr    <$> actions
    groupsVal     <- actionsGroups <$> actions
    userVal       <- user
    return . fromEvent Nothing (Just <$> deleteUserEv)
      $ \case
         Nothing -> Right userVal
         Just user
           -> case find (isUserAction user) actionsArrVal of
                Just userAction
                  -> Left $ UserInActionErr user userAction
                Nothing -> case find (user `elem`) groupsVal of
                  Just group -> Left $ UserInGroupErr user group
                  Nothing    -> Right user
  text " "
  errorWidget never deleteUserValid (return ()) $ \case
    UserInActionErr user action -> do
      text . T.pack
        $ printf "Can't delete user '%s' referenced in action: " user
      actionWidget action
      return ()
    UserInGroupErr user group -> do
      text . T.pack
        $ printf "Can't delete user '%s' referenced in group: %s"
            user (printUsersList group)
  return $ tagValid deleteUserValid deleteUserEv

addGroup
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => [User] -> [Group] -> m (Event t Group)
addGroup users groups = mdo
  let usersNotInGroups
        = filter (not . \user -> any (\group -> user `elem` group) groups)
          $ users
  newGroupUsers <- selectUsers "group" $ constDyn usersNotInGroups
  let newGroupUsersValid :: ValidDynamic t Text [User]
        = fromDynamic newGroupUsers
          $ \case
              [ ] -> Left $ "No users selected. "
                     `T.append` "At least 2 users necessary for a group"
              [_] -> Left $ "Only one user is selected. "
                     `T.append` "At least 2 users necessary for a group"
              gs  -> Right $ gs
  addGroupButtonEv <- button "Add group"
  let addGroupEv = tagValid newGroupUsersValid addGroupButtonEv
  dynText
    =<< errorDyn "" addGroupButtonEv
    -- 'selectUsers' generates 2 unnecessary updates
    -- we drop them to prevent "" error text being replaced
    -- with an actual error before any actual update is done
    =<< dropValidDynamic (Left "") 2 newGroupUsersValid
  return addGroupEv

data GroupDeletionErr = GroupInActionErr Group Action deriving Show

groupListItem actions group = do
  dynText $ T.pack . printUsersList <$> group
  deleteGroupEv <- switchHold never =<< (dyn $ deleteX <$> group)
  deleteGroupValid <- unwrapValidDynamicWidget [] $ do
    actionsArrVal <- actionsArr    <$> actions
    groupVal      <- group
    return . fromEvent Nothing (Just <$> deleteGroupEv)
      $ \case
          Nothing -> Right groupVal
          Just group ->
            case find (isGroupAction group) actionsArrVal of
              Just groupAction
                -> Left $ GroupInActionErr group groupAction
              Nothing -> Right group
  text " "
  errorWidget never deleteGroupValid (return ()) $ \case
    GroupInActionErr group action -> do
      text . T.pack
        $ printf
          "Can't delete group '%s' referenced in action: "
          (printUsersList group)
      actionWidget action
      return ()
  return $ tagValid deleteGroupValid deleteGroupEv

manageGroups
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => [Group] -> Dynamic t Actions -> Dynamic t [User] -> m (Dynamic t [[User]])
manageGroups groups0 actions users = do
  el "h2" $ text "Manage user groups"
  el "h3" $ text "Select users for the group"
  rec
    let addGroupEvDyn :: Dynamic t (m (Event t Group)) = do
          users  <- users
          groups <- userGroups
          return $ addGroup users groups
    addGroupEv <- switchHold never =<< dyn addGroupEvDyn
    userGroups :: Dynamic t [Group] <-
      foldDyn ($) groups0
      ( mergeWith (.)
        [ (:)    <$> addGroupEv
        , delete <$> deleteGroupEv
        ]
      )
    el "h3" $ text "User groups"
    deleteGroupEv <-
      el "ul" $ simpleListOneEvent userGroups (groupListItem actions)
  return userGroups

validInput
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Event t b -> (Text -> Either Text a)
  -> m (ValidDynamic t Text a)
validInput submitEvent validation = do
  inputValue <- value <$> inputElement def
  let inputValueValid = fromDynamic inputValue validation
  text " "
  dynText =<< errorDyn "" submitEvent inputValueValid
  return inputValueValid

unwrapDynWidget :: (Adjustable t m, NotReady t m, MonadHold t m, PostBuild t m)
  => a -> Dynamic t (m (Dynamic t a)) -> m (Dynamic t a)
unwrapDynWidget initVal dynWidget =
  join <$> (holdDyn (constDyn initVal) =<< dyn dynWidget)

unwrapEventWidget
  :: (Adjustable t m, NotReady t m, PostBuild t m, MonadHold t m)
  => Dynamic t (m (Event t a)) -> m (Event t a)
unwrapEventWidget evWidget = switchHold never =<< dyn evWidget

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
        -> fmap (fmap PurchaseAction) $ addItemizedSplitPurchase users groups
      PaymentTransactionActionType
        -> addPaymentTransaction users groups

userInput
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Dynamic t [User] -> m (ValidDynamic t Text User)
userInput label users = do
  text $ label `T.append` ": "
  user :: ValidDynamic t Text User <- fromDynamicEither <$> unwrapDynWidget
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
  => Event t a -> m (ValidDynamic t Text Text)
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
  => Event t a -> m (ValidDynamic t Text Amount)
amountInput addEv = do
  text "Amount: "
  amount <- validInput addEv $ \txt ->
    maybe
    (Left $ "Failed to read amount: " `T.append` txt) id
    $ do
      amt <- readMaybe . T.unpack $ txt :: Maybe Amount
      if amt < 0
        then return . Left $ "Negative amount not allowed"
        else if amt == 0
             then return . Left $ "Zero amount not allowed"
             else return . Right $ amt
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

selectUserItem
  :: forall t m . (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => String -> Dynamic t User -> m (Dynamic t Bool)
selectUserItem idPrefix user = unwrapDynWidget False $ do
  user <- user
  let widget :: m (Dynamic t Bool) = do
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
  return widget

selectUsers
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => String -> Dynamic t [User] -> m (Dynamic t [User])
selectUsers idPrefix users = do
  selectedCbs :: Dynamic t [Bool]
    <- fmap (join . fmap distributeListOverDyn)
       $ simpleList users (selectUserItem idPrefix)
  return . fmap (map snd . filter fst)
    $ (zip <$> selectedCbs <*> users)

addSplitEquallyPurchase
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> m (Event t Purchase)
addSplitEquallyPurchase users = do
  el "h3" $ text "Add \"split equally\" purchase"
  user <- userInput "User" users
  rec
    desc          <- descriptionInput addEv
    amount        <- amountInput addEv
    selectedUsers <- assumeValidDynamic <$> selectUsers "split" users
    addEv         <- button "Add purchase"
  let purchase
        = Purchase
        <$> user
        <*> fmap (T.unpack) desc
        <*> amount
        <*> (SplitEqually . map SplitToUser <$> selectedUsers)
  return $ tagValid purchase addEv

resettableDropdown
  :: forall t m k b
   . (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Ord k)
  => (k -> Text) -> Maybe k -> Dynamic t [k] -> Event t (Maybe b)
  -> m (Dynamic t (Maybe k))
resettableDropdown showF init items resetEv = do
  let dd init = dropdown
           init
           ( fmap
             ( M.fromList
               . ((Nothing, "") :)
               . map (\item -> (Just item, showF item))
             )
             $ items
           )
           $ def
  elDyn <- widgetHold (dd init) (dd Nothing <$ ffilter isJust resetEv)
  return . join $ value <$> elDyn

addSplitItem
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => User -> Dynamic t [User] -> Dynamic t [Group] -> m (Event t SplitItem)
addSplitItem firstUser users groups = do
  rec
    text "Payed for user: "
    user  <- resettableDropdown
             T.pack
             (Just firstUser)
             users
             (updated group)
    text " or group: "
    group <- resettableDropdown
             (T.pack . printUsersList)
             Nothing
             groups
             (updated user)
    splitTo <- holdDyn (SplitToUser firstUser)
      . fmap
        ( \case
            (Just user, Nothing   ) -> SplitToUser  $ user
            (Nothing  , Just group) -> SplitToGroup $ group
            _ -> error "filtered out"
        )
      . ffilter
        ( \case
            (Just _, Nothing) -> True
            (Nothing, Just _) -> True
            _ -> False
        )
      . updated
      $ zipDyn user group
    el "br" blank
    desc   <- descriptionInput addEv
    amount <- amountInput addEv
    addEv  <- button "Add split item"
  let splitItem
         =  SplitItem
        <$> assumeValidDynamic splitTo
        <*> fmap (T.unpack) desc
        <*> amount
  return $ tagValid splitItem addEv

splitItemWidget :: DomBuilder t m
  => SplitItemOnView
  -> m (Event t SplitItemOnView)
splitItemWidget onView@(SplitItemOnView deletable item) = el "li" $ do
  text . T.pack . printUsersList . splitItemUsers $ item
  text ", "
  text . T.pack . splitItemDesc $ item
  text " -- "
  text . T.pack . show . splitItemAmount $ item
  ev <- if deletable
        then deleteX onView
        else return never
  el "br" $ blank
  return ev

type CanBeDeleted = Bool
data SplitItemOnView = SplitItemOnView CanBeDeleted SplitItem deriving Eq

splitItemOnView (SplitItemOnView _ item) = item

manageSplitItems :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t (Maybe Tips) -> Dynamic t [User] -> Dynamic t [Group]
  -> m (Dynamic t [SplitItem])
manageSplitItems tips users groups = do
  addSplitItemEv <- unwrapEventWidget
                    . twoUsersFromDifferentGroupsWidget users groups never
                    $ \firstUser _ -> addSplitItem firstUser users groups
  rec
    splitItems <-
      foldDyn ($) []
      ( mergeWith (.)
        [ (:) . SplitItemOnView True <$> addSplitItemEv
        , delete <$> deleteSplitItemEv
        ]
      )
    let splitItemsWithTips = do
          splitItems <- splitItems
          users <- users
          groups <- groups
          tips <- tips
          return
            $  splitItems
            ++ ( map (SplitItemOnView False)
                 . tipItems users groups tips
                 . map splitItemOnView
                 $ splitItems
               )
    el "h5" $ text "Split items"
    deleteSplitItemEv <-
      el "ul" . simpleListOneEvent splitItemsWithTips
        $ \itemDyn -> unwrapEventWidget $ do
            item <- itemDyn
            return $ splitItemWidget item
  return . fmap (map splitItemOnView) $ splitItems
      
nestedWidget :: DomBuilder t m => Text -> m a -> m a
nestedWidget label widget = do
  elAttr "table" ("class" =: "nested") $ do
    result <- el "tr" $ do
      elAttr "td" ("class" =: "nested") $ do
        text label
      elAttr "td" ("rowspan" =: "2" <> "class" =: "nested") $ do
        widget
    el "tr" $ do
      el "td" $ blank
    return result

data SplitTipsType
  = SplitTipsEquallyType
  | SplitTipsEquallyAllType
--  | ItemizedSplitTipsType
  | RelativeSplitTipsType
  deriving (Eq, Ord)

manageSplitTipsItems
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User]
  -> m (Dynamic t [SplitTipsItem])
manageSplitTipsItems = do
  undefined

addTips
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> m (Dynamic t (Maybe Tips))
addTips users = do
  text "Tip percentage: "
  tips <- value
          <$> (dropdown
              Nothing
              (constDyn (Just 10 =: "10%" <> Just 20 =: "20%"))
              def)
  el "br" $ blank
  let tipsMaybeDynDyn :: Dynamic t (m (Dynamic t (Maybe Tips))) = do
        tips <- tips
        return $ maybe
          ( return . return $ Nothing )
          ( \tips -> do
              text "How to split tips? "
              splitType <- value <$> dropdown
                             RelativeSplitTipsType
                             ( constDyn
                             (  SplitTipsEquallyType    =: "Split equally"
                             <> SplitTipsEquallyAllType =: "Split equally all"
                             -- <> ItemizedSplitTipsType   =: "Itemized split"
                             <> RelativeSplitTipsType   =: "Split relatively"
                             )
                           )
                           def
              el "br" $ blank
              unwrapDynWidget Nothing $ do
                  splitType <- splitType
                  return case splitType of
                    SplitTipsEquallyType -> do
                      users <- selectUsers "tipsUsers" users
                      let splitTos = fmap (map SplitToUser) users
                      return (Just <$> (Tips <$> constDyn tips <*> (SplitTipsEqually <$> splitTos)))
                    SplitTipsEquallyAllType ->
                      return . return . Just $ Tips tips SplitTipsEquallyAll
                    RelativeSplitTipsType ->
                      return . return . Just $ Tips tips RelativeSplitTips
          )
          $ tips
  unwrapDynWidget Nothing tipsMaybeDynDyn

addItemizedSplitPurchase
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> Dynamic t [Group] -> m (Event t Purchase)
addItemizedSplitPurchase users groups = do
  el "h3" $ text "Add \"itemized split\" purchase"
  user <- userInput "User" users
  rec
    desc       <- descriptionInput addEv
    text "Amount: "
    let amount = fmap (sum . map splitItemAmount) $ splitItems
    dynText . fmap (T.pack . show) $ amount
    tips <- nestedWidget "Tips: " (addTips users)
    splitItems <- nestedWidget "Split items: "
                  $ manageSplitItems tips users groups
    addEv <- button "Add purchase"
  let purchase
        = Purchase
        <$> user
        <*> fmap (T.unpack) desc
        <*> ( fmap (sum . map splitItemAmount) . assumeValidDynamic
              $ splitItems
            )
        <*> ( assumeValidDynamic $ ItemizedSplit <$> tips <*> splitItems )
  return $ tagValid purchase addEv

preselectedItemDropdown
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Ord k)
  => (k -> Text) -> k -> Dynamic t [k] -> Maybe k
  -> m (Dropdown t k)
preselectedItemDropdown showF noItemsVal items selectedItem = 
  case selectedItem of
    Nothing
      -> dropdown
         noItemsVal
         (constDyn . M.fromList $ [])
         $ def
    Just selectedItem
      -> dropdown
         selectedItem
         (M.fromList <$> (zip <$> items <*> fmap (map showF) items))
         $ def

notSelectedItemDropdown
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Ord a)
  => (a -> Text) -> a -> Maybe a -> Dynamic t [a] -> Event t [a]
  -> m (Dynamic t a)
notSelectedItemDropdown showF noItemsVal init items selectedItems = mdo
  elDyn <- widgetHold
           ( preselectedItemDropdown
             showF noItemsVal items init )
           ( preselectedItemDropdown
             showF noItemsVal items <$> notSelectedUserEv
           )
  let val = join $ value <$> elDyn
  let notSelectedUserEv
        = fmap (
            \((_, items), selectedItems) -> case items \\ selectedItems of
                       [] -> Nothing
                       x:_ -> Just x
            )
        . ffilter (\((val, _), selectedItems) -> val `elem` selectedItems)
        . attach (current $ zipDyn val items)
        $ selectedItems
  return val

twoUsersFromDifferentGroupsWidget
  :: (DomBuilder t m, MonadHold t m, PostBuild t m)
  => Dynamic t [User] -> Dynamic t [Group] -> a -> (User -> User -> m a)
  -> Dynamic t (m a)
twoUsersFromDifferentGroupsWidget users groups errorVal widget = do
  users  <- users
  groups <- groups
  if length users < 2
    then return $ do
      let msg = "At least two users required, "
                `T.append` "please add users in \"Manage users\""
      text msg
      return errorVal
    else
      let firstUser = head users
          maybeSecondUser = findUserNotInTheSameGroup users groups firstUser
      in return $ maybe
         ( do
             let msg = "All users should not belong to the same group, "
                       `T.append` "please add users in \"Manage users\" or "
                       `T.append` "remove groups in \"Manage groups\""
             text msg
             return errorVal
         )
         ( widget firstUser )
         maybeSecondUser
         
addPaymentTransaction0
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => User -> User -> Dynamic t [User] -> Dynamic t [Group] -> m (Event t Action)
addPaymentTransaction0 firstUser secondUser users groups = mdo
  text "Debit user: "
  debitUser :: Dynamic t User
    <- notSelectedItemDropdown
       T.pack ""
       (Just firstUser)
       users
       ( fmap (\(groups, user) -> currentGroupOrUser groups user)
         . (attach . current) groups
         . updated
         $ creditUser
       )
  el "br" blank
  text "Credit user: "
  creditUser :: Dynamic t User
    <- notSelectedItemDropdown
       T.pack ""
       (Just secondUser)
       users
       ( fmap (\(groups, user) -> currentGroupOrUser groups user)
         . (attach . current) groups
         . updated
         $ debitUser
       )
  el "br" blank
  amount <- amountInput addEv
  addEv  <- button "Add payment"
  let action
        = PaymentAction
          <$> assumeValidDynamic debitUser
          <*> assumeValidDynamic creditUser
          <*> amount
  return $ tagValid action addEv

addPaymentTransaction
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> Dynamic t [Group] -> m (Event t Action)
addPaymentTransaction users groups = do
  el "h3" $ text "Add payment"
  unwrapEventWidget . twoUsersFromDifferentGroupsWidget users groups never
    $ \firstUser secondUser ->
        addPaymentTransaction0 firstUser secondUser users groups

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
      ( Purchase { purchaseSplit = SplitEqually [splitTo] } )
    ) = do
  actionWidgetPayedFor action
  text " for "
  text . T.pack . printUsersList . splitToToUsers $ splitTo
actionWidget
    action@( PurchaseAction
      ( Purchase { purchaseSplit = SplitEqually splitTos } )
    ) = do
  actionWidgetPayedFor action
  text " split equally to "
  text . T.pack . printUsersList . splitTosUsers $ splitTos
actionWidget
    action@( PurchaseAction
      ( Purchase { purchaseSplit = ItemizedSplit Nothing splits } )
    ) = do
  actionWidgetPayedFor action
  text " split in "
  text . T.pack . show . length $ splits
  text " items"
actionWidget
    action@( PurchaseAction
      ( Purchase { purchaseSplit = ItemizedSplit (Just (Tips tips _)) splits } )
    ) = do
  actionWidgetPayedFor action
  text " split in "
  text . T.pack . show . length $ splits
  text " items with "
  text . T.pack . show $ tips
  text "% tips"
actionWidget ( PaymentAction debitUser creditUser amount ) = do
  text . T.pack $ debitUser
  text " payed "
  text . T.pack $ creditUser
  text " "
  text . T.pack . show $ amount

manageActions
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => [Action] -> Dynamic t [User] -> Dynamic t [Group] -> m (Dynamic t Actions)
manageActions actionsArr0 users groups = do
  el "h2" $ text "Manage actions"
  rec
    addActionEv <- addAction users groups
    actionsList <-
      foldDyn ($) actionsArr0
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

app
  :: (Reflex t, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, ActionsStore s, MonadIO m)
  => s -> m ()
app store = do
  actions0 <- getActions store
  rec 
    users <- manageUsers (actionsUsers actions0) actions
    groups <- manageGroups (actionsGroups actions0) actions users
    actions <- manageActions (actionsArr actions0) users groups
  dyn (actions >>= (return . putActions store))
  let nullified = (nullifyBalances . actionsToTransactions) <$> actions
  el "h2" $ text "Report"
  dyn (report <$> actions <*> nullified)
  return ()  
