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
import           Control.Monad          (join)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Data.List              (delete, find, (\\))
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (isJust)
import           Data.Text              (Text)
import qualified Data.Text              as T
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
  => Dynamic t [item] -> (Dynamic t item -> m (Event t a))
  -> m (Event t a)
simpleListOneEvent items itemWidget
  = switchDyn <$> (simpleList items itemWidget >>=
                    \evs -> return $ (leftmost <$> evs))

listWithKeyOneEvent
  :: (Ord k, Eq item, Adjustable t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t (Map k item) -> (k -> Dynamic t item -> m (Event t a))
  -> m (Event t a)
listWithKeyOneEvent items itemWidget
  = switchDyn <$> (listWithKey items itemWidget >>=
                    \evs -> return $ (leftmost . M.elems <$> evs))

data UserDeletionErr
  = UserInActionErr User Action
  | UserInGroupErr User Group
  deriving Show

userListItem :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m)
  => Dynamic t Actions -> Dynamic t User -> m (Event t User)
userListItem actions user = el "li" $ do
  dynText (T.pack <$> user)
  deleteUserEv <- switchHold never =<< (dyn $ actionLink "delete" <$> user)
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
      actionText action
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
  newGroupUsers <- selectUsers "group" Nothing $ constDyn usersNotInGroups
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

groupListItem actions group = el "li" $ do
  dynText $ T.pack . printUsersList <$> group
  deleteGroupEv <- switchHold never =<< (dyn $ actionLink "delete" <$> group)
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
      actionText action
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
  => (a -> Text) -> Maybe a -> Event t b -> (Text -> Either Text a)
  -> m (ValidDynamic t Text a)
validInput showF currentVal submitEvent validation = do
  inputValue <- value
    <$> ( inputElement
          $ def
          & inputElementConfig_initialValue .~ (maybe "" showF currentVal)
        )
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

actionToType (PurchaseAction (Purchase { purchaseSplit = SplitEquallyAll   }))
  = PurchaseSplitEquallyAllActionType
actionToType (PurchaseAction (Purchase { purchaseSplit = SplitEqually  _   }))
  = PurchaseSplitEquallyActionType
actionToType (PurchaseAction (Purchase { purchaseSplit = ItemizedSplit _ _ }))
  = PurchaseItemizedSplitActionType
actionToType (PaymentAction _ _ _)
  = PaymentTransactionActionType

actionForm
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Maybe Action
  -> Dynamic t [User] -> Dynamic t [Group]
  -> m (Event t Action)
actionForm actionLabel currentAction users groups = do
  text "Choose action type: "
  dropdownEl
    <- dropdown
       (maybe PurchaseSplitEquallyAllActionType actionToType currentAction)
       ( constDyn
         (    PurchaseSplitEquallyAllActionType
              =: "Purchase Split Equally All"
           <> PurchaseSplitEquallyActionType
              =: "Purchase Split Equally"
           <> PurchaseItemizedSplitActionType
              =: "Purchase Itemized Split"
           <> PaymentTransactionActionType
              =: "Payment"
         )
       )
       def
  let actionType = value dropdownEl
  unwrapEventWidget $ do
    actionType <- actionType
    return case actionType of
      PurchaseSplitEquallyAllActionType
        -> fmap (fmap PurchaseAction)
           $ splitAllPurchaseForm
             actionLabel
             currentAction
             users
      PurchaseSplitEquallyActionType
        -> fmap (fmap PurchaseAction)
           $ splitEquallyPurchaseForm
             actionLabel
             currentAction
             users
      PurchaseItemizedSplitActionType
        -> fmap (fmap PurchaseAction)
           $ itemizedSplitPurchaseForm
             actionLabel
             currentAction
             users groups
      PaymentTransactionActionType
        -> paymentTransactionForm actionLabel currentAction users groups

userInput
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Maybe User -> Dynamic t [User] -> m (ValidDynamic t Text User)
userInput label currentUser users = do
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
                  (maybe (head users) id currentUser)
                  (constDyn . M.fromList $ zip users (map T.pack users))
                  def
            return $ Right <$> value el
    )
  el "br" blank
  return user

descriptionInput :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Maybe Text -> Event t a -> m (ValidDynamic t Text Text)
descriptionInput currentVal addEv = do
  text "Description: "
  desc <- validInput id currentVal addEv $ \txt ->
    let txt' = T.strip txt
    in if T.null txt'
       then Left "No description provided"
       else Right txt'
  el "br" blank
  return desc

amountInput :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Maybe Amount -> Event t a -> m (ValidDynamic t Text Amount)
amountInput currentAmount addEv = do
  text "Amount: "
  amount <- validInput (T.pack . show) currentAmount addEv $ \txt ->
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

splitAllPurchaseForm
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Maybe Action
  -> Dynamic t [User]
  -> m (Event t Purchase)
splitAllPurchaseForm actionLabel currentAction users = do
  el "h3" . text $ actionLabel `T.append` " \"split all\" purchase"
  user <- userInput "Payer" (actionDebitUser <$> currentAction) users
  rec
    desc   <- descriptionInput
              (T.pack <$> (actionDesc =<< currentAction))
              addEv
    amount <- amountInput (actionAmount <$> currentAction) addEv
    addEv  <- button $ actionLabel `T.append` " purchase"
  let purchase
        = Purchase
        <$> user
        <*> fmap (T.unpack) desc
        <*> amount
        <*> pure SplitEquallyAll
  return $ tagValid purchase addEv

selectUserItem
  :: forall t m . (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => String -> Dynamic t (Bool, User) -> m (Dynamic t Bool)
selectUserItem idPrefix preselectedUser = unwrapDynWidget False $ do
  (preselected, user) <- preselectedUser
  let widget :: m (Dynamic t Bool) = do
        cb <- inputElement
              $ def
              & inputElementConfig_initialChecked .~ preselected
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
  => String -> Maybe [User] -> Dynamic t [User] -> m (Dynamic t [User])
selectUsers idPrefix currentUsers users = do
  let preselectedUsers
        = fmap
          ( map (\user -> (user `elem` (maybe [] id currentUsers), user)) )
          users
  selectedCbs :: Dynamic t [Bool]
    <- fmap (join . fmap distributeListOverDyn)
       $ simpleList preselectedUsers (selectUserItem idPrefix)
  return . fmap (map snd . filter fst)
    $ (zip <$> selectedCbs <*> users)

splitEquallyPurchaseForm
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Maybe Action
  -> Dynamic t [User]
  -> m (Event t Purchase)
splitEquallyPurchaseForm actionLabel currentAction users = do
  el "h3" . text $ actionLabel `T.append` " \"split equally\" purchase"
  user <- userInput "Payer" (actionDebitUser <$> currentAction) users
  rec
    desc          <- descriptionInput
                     (T.pack <$> (actionDesc =<< currentAction))
                     addEv
    amount        <- amountInput (actionAmount <$> currentAction) addEv
    selectedUsers <- assumeValidDynamic
                     <$> selectUsers
                         "split"
                         (actionSplitEquallyUsers =<< currentAction)
                         users
    addEv         <- button $ actionLabel `T.append` " purchase"
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
    desc   <- descriptionInput Nothing addEv
    amount <- amountInput Nothing addEv
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
        then actionLink "delete" onView
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
                    . twoUsersFromDifferentGroupsWidget
                      users groups Nothing never
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
  return . fmap (map splitItemOnView) $ splitItemsWithTips
      
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

tipsSplitType (SplitTipsEqually  _) = SplitTipsEquallyType
tipsSplitType (SplitTipsEquallyAll) = SplitTipsEquallyAllType
tipsSplitType (ItemizedSplitTips _)
  = error "ItemizedSplitTipsType not supported yet"
  -- ItemizedSplitTipsType
tipsSplitType (RelativeSplitTips  ) = RelativeSplitTipsType

manageSplitTipsItems
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User]
  -> m (Dynamic t [SplitTipsItem])
manageSplitTipsItems = do
  undefined

addTips
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Maybe Tips -> Dynamic t [User] -> m (Dynamic t (Maybe Tips))
addTips currentTips users = do
  text "Tip percentage: "
  tipsPercentageMaybe <- value
                         <$> ( dropdown
                               (tipsPercentage <$> currentTips)
                               (constDyn (Just 10 =: "10%" <> Just 20 =: "20%"))
                               def
                             )
  el "br" $ blank
  let tipsMaybeDynDyn :: Dynamic t (m (Dynamic t (Maybe Tips))) = do
        tipsPercentageMaybe <- tipsPercentageMaybe
        return $ maybe
          ( return . return $ Nothing )
          ( \tips -> do
              text "How to split tips? "
              splitType <- value <$> dropdown
                ( maybe
                  RelativeSplitTipsType
                  ( tipsSplitType . tipsSplit )
                  currentTips
                )
                ( constDyn
                  (    SplitTipsEquallyType    =: "Split equally"
                    <> SplitTipsEquallyAllType =: "Split equally all"
                    -- <> ItemizedSplitTipsType  =: "Itemized split"
                    <> RelativeSplitTipsType   =: "Split relatively"
                  )
                )
                def
              el "br" $ blank
              unwrapDynWidget Nothing $ do
                  splitType <- splitType
                  return case splitType of
                    SplitTipsEquallyType -> do
                      users <- selectUsers
                               "tipsUsers"
                               (tipsSplitEquallyUsers =<< currentTips)
                               users
                      let splitTos = fmap (map SplitToUser) users
                      return (Just
                              <$> (Tips
                                   <$> constDyn tips
                                   <*> (SplitTipsEqually <$> splitTos)
                                  )
                             )
                    SplitTipsEquallyAllType ->
                      return . return . Just $ Tips tips SplitTipsEquallyAll
                    RelativeSplitTipsType ->
                      return . return . Just $ Tips tips RelativeSplitTips
          )
          $ tipsPercentageMaybe
  unwrapDynWidget Nothing tipsMaybeDynDyn

itemizedSplitPurchaseForm
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Maybe Action
  -> Dynamic t [User] -> Dynamic t [Group]
  -> m (Event t Purchase)
itemizedSplitPurchaseForm actionLabel currentAction users groups = do
  el "h3" . text $ actionLabel `T.append` " \"itemized split\" purchase"
  user <- userInput "Payer" (actionDebitUser <$> currentAction) users
  rec
    desc       <- descriptionInput
                  (T.pack <$> (actionDesc =<< currentAction))
                  addEv
    text "Amount: "
    let amount = fmap (sum . map splitItemAmount) $ splitItems
    dynText . fmap (T.pack . show) $ amount
    tips <- nestedWidget "Tips: " (addTips (actionTips =<< currentAction) users)
    splitItems <- nestedWidget "Split items: "
                  $ manageSplitItems tips users groups
    addEv <- button $ actionLabel `T.append` " purchase"
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
  => Dynamic t [User] -> Dynamic t [Group] -> Maybe User -> a
  -> (User -> User -> m a)
  -> Dynamic t (m a)
twoUsersFromDifferentGroupsWidget
    users groups maybeFirstUser errorVal widget = do
  users  <- users
  groups <- groups
  if length users < 2
    then return $ do
      let msg = "At least two users required, "
                `T.append` "please add users in \"Manage users\""
      text msg
      return errorVal
    else
      let firstUser = maybe (head users) id maybeFirstUser
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
         
paymentTransactionForm0
  :: forall t m . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Maybe Action
  -> User -> User -> Dynamic t [User] -> Dynamic t [Group]
  -> m (Event t Action)
paymentTransactionForm0 actionLabel currentAction
                        firstUser secondUser users groups = mdo
  text "Debit user: "
  debitUser :: Dynamic t User
    <- notSelectedItemDropdown
       T.pack ""
       ( maybe (Just firstUser) (Just . actionDebitUser) currentAction )
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
  amount <- amountInput (actionAmount <$> currentAction) addEv
  addEv  <- button $ actionLabel `T.append` " payment"
  let action
        = PaymentAction
          <$> assumeValidDynamic debitUser
          <*> assumeValidDynamic creditUser
          <*> amount
  return $ tagValid action addEv

paymentTransactionForm
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Maybe Action
  -> Dynamic t [User] -> Dynamic t [Group]
  -> m (Event t Action)
paymentTransactionForm actionLabel currentAction users groups = do
  el "h3" . text $ actionLabel `T.append` " payment"
  unwrapEventWidget
    . twoUsersFromDifferentGroupsWidget
      users groups (actionDebitUser <$> currentAction) never
    $ \firstUser secondUser ->
        paymentTransactionForm0
          actionLabel currentAction
          firstUser secondUser users groups

actionTextPayedFor
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
actionTextPayedFor _
  = error "actionTextPayedFor: only implemented for purchases so far"

actionLink :: DomBuilder t m => Text -> a -> m (Event t a)
actionLink label item = do
  text " ["
  (el, _) <- elAttr' "a" ("class" =: "link") $ text label
  text "]"
  return (item <$ domEvent Click el)  

actionText
    action@( PurchaseAction
      ( Purchase { purchaseSplit = SplitEquallyAll } )
    ) = do
  actionTextPayedFor action
  text " split equally to all"
actionText
    action@( PurchaseAction
      ( Purchase { purchaseSplit = SplitEqually [splitTo] } )
    ) = do
  actionTextPayedFor action
  text " for "
  text . T.pack . printUsersList . splitToToUsers $ splitTo
actionText
    action@( PurchaseAction
      ( Purchase { purchaseSplit = SplitEqually splitTos } )
    ) = do
  actionTextPayedFor action
  text " split equally to "
  text . T.pack . printUsersList . splitTosUsers $ splitTos
actionText
    action@( PurchaseAction
      ( Purchase { purchaseSplit = ItemizedSplit Nothing splits } )
    ) = do
  actionTextPayedFor action
  text " split in "
  text . T.pack . show . length $ splits
  text " items"
  return ()
actionText
    action@( PurchaseAction
      ( Purchase { purchaseSplit = ItemizedSplit (Just (Tips tips _)) splits } )
    ) = do
  actionTextPayedFor action
  text " split in "
  text . T.pack . show . length $ splits
  text " items with "
  text . T.pack . show $ tips
  text "% tips"
actionText ( PaymentAction debitUser creditUser amount ) = do
  text . T.pack $ debitUser
  text " payed "
  text . T.pack $ creditUser
  text " "
  text . T.pack . show $ amount

data ActionState
  = ActionState
  { actionStateEdit   :: Bool
  , actionStateAction :: Action
  } deriving Eq

actionWidget
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Dynamic t [User] -> Dynamic t [Group] -> Int -> Dynamic t ActionState
  -> m (Event t (Map Int ActionState -> Map Int ActionState))
actionWidget users groups ix stateDyn = unwrapEventWidget $ stateDyn >>= \case
  st@(ActionState False action) -> return . el "li" $ do
    actionText action
    editEv   <- actionLink "edit" st
    deleteEv <- actionLink "delete" st
    return . leftmost $
      [ M.delete ix <$ deleteEv
      , M.update (\st -> Just st { actionStateEdit = True }) ix <$ editEv
      ]
  st@(ActionState True action) -> return . el "li" $ do
    actionText action
    cancelEditEv <- actionLink "cancel edit" st
    deleteEv <- actionLink "delete" st
    el "br" blank
    actionEv <- actionForm "Update" (Just action) users groups 
    return . leftmost $
      [ M.delete ix <$ deleteEv
      , (\action -> M.update (\_ -> Just (ActionState False action)) ix)
        <$> actionEv
      , M.update (\st -> Just st { actionStateEdit = False }) ix <$ cancelEditEv
      ]

addNew el m = case M.lookupMax m of
  Just (ix, _) -> M.insert (ix + 1) el m
  Nothing -> M.insert 0 el m

manageActions
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => [Action] -> Dynamic t [User] -> Dynamic t [Group] -> m (Dynamic t Actions)
manageActions actionsArr0 users groups = do
  el "h2" $ text "Manage actions"
  rec
    addActionEv <- actionForm "Add" Nothing users groups
    actionsMap :: Dynamic t (Map Int ActionState) <-
      foldDyn ($)
      ( M.fromAscList . zip [0..] . map (ActionState False) $ actionsArr0 )
      ( mergeWith (.)
        [ addNew . (ActionState False) <$> addActionEv
        , actionEv
        ]
      )
    let actions = Actions
                  <$> users
                  <*> groups
                  <*> (map actionStateAction . M.elems <$> actionsMap)
    el "h3" $ text "Actions list"
    actionEv <- el "ul" $ listWithKeyOneEvent actionsMap (actionWidget users groups)
  return actions

app
  :: ( Reflex t, DomBuilder t m, MonadHold t m
     , PostBuild t m, MonadFix m, ActionsStore s, MonadIO m )
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
