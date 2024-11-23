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

import           Control.Lens               (view)
import           Control.Monad              (join)
import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.List                  (delete, elemIndex, find, (\\))
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (isJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           MoneySplit                 hiding (addTips)
import           Reflex.Dom                 hiding (Group)
import           SplitReport
import           Text.Printf                (printf)
import           Text.Read                  (readMaybe)
import           ValidDynamic               (ValidDynamic, assumeValidDynamic,
                                             assumeValidValue, dropValidDynamic,
                                             errorDyn, errorWidget, fromDynamic,
                                             fromDynamicEither, fromEvent,
                                             tagPromptlyValid, tagValid,
                                             unwrapValidDynamicDynamicWidget,
                                             unwrapValidDynamicWidget)
import           WorkspaceStore             (WorkspaceName,
                                             WorkspaceStore (deleteWorkspace,
                                                             getActions,
                                                             getWorkspaces,
                                                             migrate,
                                                             putActions),
                                             defaultWorkspaceName)

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
          = tagPromptlyValid validInput submitOrEnterEv
  return (ev, validInput)

type NewWorkspaceStateEvent t = Event t (WorkspaceState)

data WorkspaceState
  = InitialWorkspaceState
  | ConfirmWipeInitialWorkspaceState
  | CreateSecondWorkspaceState
  | MultipleWorkspaceState      WorkspaceName [WorkspaceName]
  | CreateNewWorkspaceState     WorkspaceName [WorkspaceName]
  | ConfirmDeleteWorkspaceState WorkspaceName [WorkspaceName]
  | PerformDeleteWorkspaceState WorkspaceName WorkspaceName [WorkspaceName]
  | ConfirmWipeWorkspaceState   WorkspaceName [WorkspaceName]
  deriving Show

newWorkspace
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Bool -> WorkspaceName -> [WorkspaceName] -> m (NewWorkspaceStateEvent t)
newWorkspace initState defWs wss = do
  text "New workspace name: "
  rec
    (ev, userInput) <- resettableInput addWorkspaceButton $
      \workspaceName ->
        let workspaceNameStr = T.unpack . T.strip $ workspaceName
        in if null $ workspaceNameStr
           then Left $ "Empty workspace names are not allowed"
           else Right workspaceNameStr
    text " "
    addWorkspaceButton <- button "Add Workspace"
    cancelButton <- button "Cancel"
    dynText =<< (errorDyn "" addWorkspaceButton $ userInput)
  return . leftmost $
    [ fmap
      (\newWorkspaceName ->
         MultipleWorkspaceState
         newWorkspaceName
         (wss ++ [newWorkspaceName])
      )
      $ tagValid userInput ev
    , if initState
      then InitialWorkspaceState <$ cancelButton
      else MultipleWorkspaceState defWs wss <$ cancelButton
    ]

workspaceStateName InitialWorkspaceState              = defaultWorkspaceName
workspaceStateName ConfirmWipeInitialWorkspaceState   = defaultWorkspaceName
workspaceStateName CreateSecondWorkspaceState         = defaultWorkspaceName
workspaceStateName (MultipleWorkspaceState      ws _) = ws
workspaceStateName (CreateNewWorkspaceState     ws _) = ws
workspaceStateName (ConfirmDeleteWorkspaceState ws _) = ws
workspaceStateName (PerformDeleteWorkspaceState ws _ _) = ws
workspaceStateName (ConfirmWipeWorkspaceState   ws _) = ws

manageWorkspaces
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m
     , MonadIO m, WorkspaceStore workspaceStore)
  => workspaceStore -> WorkspaceState -> m (Dynamic t WorkspaceName)
manageWorkspaces workspaceStore initalWorkspaceState = do
  el "h2" $ text "Select workspace"
  rec
    workspaceStateDyn :: Dynamic t WorkspaceState
      <- foldDyn const initalWorkspaceState newWorkspaceStateEvent
    newWorkspaceStateEvent <- switchHold never =<< dyn do
      fmap manageWorkspacesState workspaceStateDyn
  return . fmap workspaceStateName $ workspaceStateDyn
  where
    manageWorkspacesState
      :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, MonadIO m)
      => WorkspaceState -> m (NewWorkspaceStateEvent t)
    manageWorkspacesState InitialWorkspaceState = el "p" $ do
      text . T.pack
        $  "Separate workspaces allow to track shared expenses "
        ++ "for different purpose and users. "
        ++ "You are working in the default workspace now, you can "
      (elCreate, _) <- elAttr' "a" ("class" =: "link")
                     $ text "create another workspace"
      text " or "
      (elWipe, _) <- elAttr' "a" ("class" =: "link")
                   $ text "wipe data of the default workspace"
      text "."
      return . leftmost $
        [ CreateSecondWorkspaceState <$ domEvent Click elCreate
        , ConfirmWipeInitialWorkspaceState <$ domEvent Click elWipe
        ]
    manageWorkspacesState ConfirmWipeInitialWorkspaceState = do
      initialEv <- manageWorkspacesState InitialWorkspaceState
      el "p" $ text "Are you sure you want to wipe the default workspace? "
      el "p" $ do
        confirmWipeEv <- button "Yes, wipe the default workspace"
        cancelEv <- button "Cancel"
        return . leftmost $
          [ initialEv
          , InitialWorkspaceState <$ confirmWipeEv
          , InitialWorkspaceState <$ cancelEv
          ]
    manageWorkspacesState CreateSecondWorkspaceState = do
      initialEv <- manageWorkspacesState InitialWorkspaceState
      newWsEv <- el "p" $ newWorkspace True defaultWorkspaceName [defaultWorkspaceName]
      return . leftmost $
        [ initialEv
        , newWsEv
        ]
    manageWorkspacesState (MultipleWorkspaceState defWs wss) = el "p" $ do
      text "Select workspace: "
      let newIdx = length wss
      wsEl :: Dropdown t Int <- dropdown
                                ( maybe
                                  ( error $ printf "%s workspace must be present")
                                  id
                                  ( elemIndex defWs wss )
                                )
                                ( constDyn
                                  . M.fromList
                                  $ zip
                                    [0..] -- Index is a key to preserve order
                                    (map T.pack wss ++ ["<New workspace>"])
                                )
                                def
      deleteEv <-
        case defWs of
          "Default" -> fmap (True <$) $ button "Wipe the default workspace"
          _ -> fmap (False <$) . button . T.pack
               $ printf "Delete the '%s' workspace" defWs
      return . leftmost $
        [ fmap ( \case
                   idx | idx == newIdx -> CreateNewWorkspaceState defWs      wss
                       | otherwise     -> MultipleWorkspaceState  (wss!!idx) wss
               )
          $ view dropdown_change wsEl
        , (\case
              True  -> ConfirmWipeWorkspaceState defWs wss
              False -> ConfirmDeleteWorkspaceState defWs wss
          ) <$> deleteEv
        ]
    manageWorkspacesState (CreateNewWorkspaceState defWs wss) = do
      selectEv <- manageWorkspacesState (MultipleWorkspaceState defWs wss)
      addEv <- newWorkspace False defWs wss
      return . leftmost $
        [ selectEv
        , addEv
        ]
    manageWorkspacesState (ConfirmDeleteWorkspaceState deleteWs wss) = do
      selectEv <- manageWorkspacesState (MultipleWorkspaceState deleteWs wss)
      el "p" . text . T.pack
        $ printf "Are you sure you want to delete the '%s' workspace? " deleteWs
      el "p" $ do
        confirmDeleteEv <- button . T.pack
          $ printf "Yes, delete the '%s' workspace" deleteWs
        cancelEv <- button "Cancel"
        let newWss = delete deleteWs wss
        return . leftmost $
          [ selectEv
          , PerformDeleteWorkspaceState (head newWss) deleteWs newWss <$ confirmDeleteEv
          , MultipleWorkspaceState deleteWs wss      <$ cancelEv
          ]
    manageWorkspacesState (PerformDeleteWorkspaceState ws deleteWs wss) = do
      deleteWorkspace workspaceStore deleteWs
      manageWorkspacesState $ MultipleWorkspaceState ws wss
    manageWorkspacesState (ConfirmWipeWorkspaceState wipeWs wss) = do
      selectEv <- manageWorkspacesState (MultipleWorkspaceState wipeWs wss)
      let wipeWsLabel = if wipeWs == defaultWorkspaceName
                        then "default" :: String
                        else printf "'%s'" wipeWs
      el "p" . text . T.pack
        $ printf "Are you sure you want to wipe the %s workspace? " wipeWsLabel
      el "p" $ do
        confirmWipeEv <- button . T.pack
          $ printf "Yes, wipe the %s workspace" wipeWsLabel
        cancelEv <- button "Cancel"
        return . leftmost $
          [ selectEv
          , MultipleWorkspaceState wipeWs wss <$ confirmWipeEv
          , MultipleWorkspaceState wipeWs wss <$ cancelEv
          ]

manageUsers
  :: forall t m .
     (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
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
              return $ tagPromptlyValid userInput ev
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
  return $ tagPromptlyValid deleteUserValid deleteUserEv

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
  let addGroupEv = tagPromptlyValid newGroupUsersValid addGroupButtonEv
  dynText
    =<< errorDyn "" addGroupButtonEv
    -- 'selectUsers' generates 2 unnecessary updates
    -- we drop them to prevent "" error text being replaced
    -- with an actual error before any actual update is done
    =<< dropValidDynamic addGroupButtonEv (Left "") 2 newGroupUsersValid
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
  return $ tagPromptlyValid deleteGroupValid deleteGroupEv

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
  = ExpenseSplitEquallyAllActionType
  | ExpenseSplitEquallyActionType
  | ExpenseItemizedSplitActionType
  | PaymentTransactionActionType deriving (Eq, Ord, Show)

actionToType (ExpenseAction (Expense { expenseSplit = SplitEquallyAll   }))
  = ExpenseSplitEquallyAllActionType
actionToType (ExpenseAction (Expense { expenseSplit = SplitEqually  _   }))
  = ExpenseSplitEquallyActionType
actionToType (ExpenseAction (Expense { expenseSplit = ItemizedSplit _ _ }))
  = ExpenseItemizedSplitActionType
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
       (maybe ExpenseSplitEquallyAllActionType actionToType currentAction)
       ( constDyn
         (    ExpenseSplitEquallyAllActionType
              =: "Expense Split Equally to All"
           <> ExpenseSplitEquallyActionType
              =: "Expense Split Equally"
           <> ExpenseItemizedSplitActionType
              =: "Expense Itemized Split"
           <> PaymentTransactionActionType
              =: "Payment"
         )
       )
       def
  let actionType = value dropdownEl
  unwrapEventWidget $ do
    actionType <- actionType
    return case actionType of
      ExpenseSplitEquallyAllActionType
        -> fmap (fmap ExpenseAction)
           $ splitAllExpenseForm
             actionLabel
             currentAction
             users
      ExpenseSplitEquallyActionType
        -> fmap (fmap ExpenseAction)
           $ splitEquallyExpenseForm
             actionLabel
             currentAction
             users
      ExpenseItemizedSplitActionType
        -> fmap (fmap ExpenseAction)
           $ itemizedSplitExpenseForm
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

splitAllExpenseForm
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Maybe Action
  -> Dynamic t [User]
  -> m (Event t Expense)
splitAllExpenseForm actionLabel currentAction users = do
  el "h3" . text $ actionLabel `T.append` " \"split equally to all\" expense"
  user <- userInput "Payer" (actionDebitUser <$> currentAction) users
  rec
    desc   <- descriptionInput
              (T.pack <$> (actionDesc =<< currentAction))
              addEv
    amount <- amountInput (actionAmount <$> currentAction) addEv
    addEv  <- button $ actionLabel `T.append` " expense"
  let expense
        = Expense
        <$> user
        <*> fmap (T.unpack) desc
        <*> amount
        <*> pure SplitEquallyAll
  return $ tagPromptlyValid expense addEv

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

splitEquallyExpenseForm
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Maybe Action
  -> Dynamic t [User]
  -> m (Event t Expense)
splitEquallyExpenseForm actionLabel currentAction users = do
  el "h3" . text $ actionLabel `T.append` " \"split equally\" expense"
  user <- userInput "Payer" (actionDebitUser <$> currentAction) users
  rec
    desc          <- descriptionInput
                     (T.pack <$> (actionDesc =<< currentAction))
                     addEv
    amount        <- amountInput (actionAmount <$> currentAction) addEv
    selectedUsers <- selectUsers
                     "split"
                     (actionSplitEquallyUsers =<< currentAction)
                     users
    let selectedUsersValid
          = fromDynamic selectedUsers
            $ \case
                [] -> Left $ "At least one user must be selected for the "
                             `T.append` "\"split equally\" action"
                us -> Right $ us
    addEv         <- button $ actionLabel `T.append` " expense"
    text " "
    -- The same reason to drop events as in addGroup
    dynText
      =<< errorDyn "" addEv
      =<< dropValidDynamic addEv (Left "") 2 selectedUsersValid
  let expense
        = Expense
        <$> user
        <*> fmap (T.unpack) desc
        <*> amount
        <*> (SplitEqually . map SplitToUser <$> selectedUsersValid)
  return $ tagPromptlyValid expense addEv

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
  return $ tagPromptlyValid splitItem addEv

moveUp ix item m = m3
  where
    Just (prevIx, prevItem) = M.lookupLT ix m
    m2 = M.insert prevIx item m
    m3 = M.insert ix prevItem m2

moveDown ix item m = m3
  where
    Just (nextIx, nextItem) = M.lookupGT ix m
    m2 = M.insert nextIx item m
    m3 = M.insert ix nextItem m2

splitItemWidget :: (DomBuilder t m, PostBuild t m, MonadHold t m)
  => Dynamic t Int -> Int -> Dynamic t SplitItemCanBeDeleted
  -> m (Event t (Map Int SplitItem -> Map Int SplitItem))
splitItemWidget maxIx ix itemDyn
  = el "li" . unwrapEventWidget $ do
    ItemCanBeDeleted deletable item <- itemDyn
    maxIx <- maxIx
    return $ do
      text . T.pack . printUsersList . splitItemUsers $ item
      text ", "
      text . T.pack . splitItemDesc $ item
      text " -- "
      text . T.pack . show . splitItemAmount $ item
      moveUpEv <- if ix /= 0 && deletable
        then actionLink "up" ix
        else return never
      moveDownEv <- if ix /= maxIx && deletable
        then actionLink "down" ix
        else return never
      deleteEv <- if deletable
        then actionLink "delete" ix
        else return never
      el "br" $ blank
      return . leftmost $
        [ M.delete ix <$ deleteEv
        , moveUp ix item <$ moveUpEv
        , moveDown ix item <$ moveDownEv
        ]

type CanBeDeleted = Bool
data ItemCanBeDeleted a
  = ItemCanBeDeleted CanBeDeleted a deriving Eq
type SplitItemCanBeDeleted = ItemCanBeDeleted SplitItem

unItemCanBeDeleted (ItemCanBeDeleted _ a) = a

tipItemsDyn
  :: Reflex t
  => Dynamic t [User] -> Dynamic t [Group] -> ValidDynamic t err (Maybe Tips)
  -> Dynamic t [SplitItem] -> Dynamic t [SplitItem]
tipItemsDyn users groups tips splitItems = do
  splitItems <- splitItems
  users <- users
  groups <- groups
  tips <- runExceptT tips >>= \case
    Right tips -> return tips
    Left  _    -> return Nothing
  return $ MoneySplit.tipItems users groups tips splitItems

manageSplitItems :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Maybe [SplitItem]
  -> ValidDynamic t Text (Maybe Tips) -> Dynamic t [User] -> Dynamic t [Group]
  -> m (Dynamic t [SplitItem])
manageSplitItems currentSplitItems tips users groups = do
  addSplitItemEv <- unwrapEventWidget
                    . twoUsersFromDifferentGroupsWidget
                      users groups Nothing never
                    $ \firstUser _ -> addSplitItem firstUser users groups
  rec
    splitItems :: Dynamic t (Map Int SplitItem) <-
      foldDyn ($) (maybe mempty (M.fromAscList . zip [0..]) currentSplitItems)
      ( mergeWith (.)
        [ addNew <$> addSplitItemEv
        , splitItemEv
        ]
      )
    let tipSplitItems
          = tipItemsDyn users groups tips . fmap M.elems $ splitItems
    let splitItemsWithTips
          = M.union
          <$> fmap (fmap (ItemCanBeDeleted True)) splitItems
          <*> do
                splitItems <- splitItems
                tipSplitItems <- tipSplitItems
                case M.lookupMax splitItems of
                  Just (k, _) ->
                    return . M.fromAscList . zip [k + 1 .. ]
                    . map (ItemCanBeDeleted False) $ tipSplitItems
                  Nothing -> mempty
    el "h5" $ text "Split items"
    splitItemEv <-
      el "ul"
      $ listWithKeyOneEvent
        splitItemsWithTips
        (splitItemWidget (maybe 0 id . fmap fst . M.lookupMax <$> splitItems))
  return . fmap M.elems $ splitItems
      
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

data TipsSelection
  = TenPercent
  | TwentyPercent
  | TwentyFivePercent
  | Custom
  deriving (Eq, Ord)

tipsSelection 10 = TenPercent
tipsSelection 20 = TwentyPercent
tipsSelection 25 = TwentyFivePercent
tipsSelection _  = Custom

tipSelectionToPercentage TenPercent = 10
tipSelectionToPercentage TwentyPercent = 20
tipSelectionToPercentage TwentyFivePercent = 25
tipSelectionToPercentage Custom
  = error "'tipSelectionToPercentage' doesn't work for 'Custom' tip selection"

addTips
  :: forall t m a . (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Event t a -> Maybe Tips -> Dynamic t [User]
  -> m (ValidDynamic t Text (Maybe Tips))
addTips submitEv currentTips users = do
  text "Tip percentage: "
  maybeTipsSelection <- value
                         <$> ( dropdown
                               ( tipsSelection . tipsPercentage
                                 <$> currentTips )
                               ( constDyn
                                 (    Nothing =: "No tips"
                                   <> Just TenPercent        =: "10%"
                                   <> Just TwentyPercent     =: "20%"
                                   <> Just TwentyFivePercent =: "25%"
                                   <> Just Custom            =: "Custom"
                                 )
                               )
                               def
                             )
  maybeTipsPercentageValid <- unwrapValidDynamicWidget Nothing $ do
    maybeTipsSelection <- maybeTipsSelection
    case maybeTipsSelection of
      Just Custom -> return $ do
        el "br" blank
        text "Custom tips percentage: "
        maybeTipsPercentageValid :: ValidDynamic t Text (Maybe Int)
          <- validInput (T.pack . maybe "" show)
             (Just . tipsPercentage <$> currentTips)
             submitEv $ \txt ->
          maybe
            (Left $ "Failed to read amount: " `T.append` txt) id
            $ do
                tipsPercentage <- readMaybe . T.unpack $ txt :: Maybe Int
                if tipsPercentage < 0
                  then return . Left $ "Negative tips not allowed"
                  else if tipsPercentage == 0
                       then return . Right $ Nothing
                       else return . Right . Just $ tipsPercentage
        return maybeTipsPercentageValid
      Just selection ->
        return . return
        . assumeValidValue . Just $ tipSelectionToPercentage selection
      Nothing ->
        return . return
        . assumeValidValue $ Nothing
  el "br" $ blank
  unwrapValidDynamicDynamicWidget Nothing $ do
    maybeTipsPercentage <- maybeTipsPercentageValid
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
      $ maybeTipsPercentage

itemizedSplitExpenseForm
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text -> Maybe Action
  -> Dynamic t [User] -> Dynamic t [Group]
  -> m (Event t Expense)
itemizedSplitExpenseForm actionLabel currentAction users groups = do
  el "h3" . text $ actionLabel `T.append` " \"itemized split\" expense"
  user <- userInput "Payer" (actionDebitUser <$> currentAction) users
  rec
    desc       <- descriptionInput
                  (T.pack <$> (actionDesc =<< currentAction))
                  addEv
    text "Amount: "
    let amount
          = fmap (sum . map splitItemAmount)
          $ (++) <$> splitItems <*> tipItemsDyn users groups tips splitItems
    dynText . fmap (T.pack . show) $ amount
    tips <- nestedWidget
            "Tips: "
            (addTips addEv (actionTips =<< currentAction) users)
    splitItems <- nestedWidget "Split items: "
                  $ manageSplitItems
                    (actionSplitItems =<< currentAction)
                    tips users groups
    addEv <- button $ actionLabel `T.append` " expense"
  let expense
        = Expense
        <$> user
        <*> fmap (T.unpack) desc
        <*> assumeValidDynamic amount
        <*> ( ItemizedSplit <$> tips <*> assumeValidDynamic splitItems )
  return $ tagValid expense addEv

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
  return $ tagPromptlyValid action addEv

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
    ( ExpenseAction
      ( Expense
        { expenseUser = expenseUser
        , expenseDesc = expenseDesc
        , expenseAmount = expenseAmount
        }
      )
    ) = do
  text . T.pack $ expenseUser
  text " payed "
  text . T.pack . show $ expenseAmount
  text " for \""
  text . T.pack $ expenseDesc
  text "\""
actionTextPayedFor _
  = error "actionTextPayedFor: only implemented for expenses so far"

actionLink :: DomBuilder t m => Text -> a -> m (Event t a)
actionLink label item = do
  text " ["
  (el, _) <- elAttr' "a" ("class" =: "link") $ text label
  text "]"
  return (item <$ domEvent Click el)

actionText
    action@( ExpenseAction
      ( Expense { expenseSplit = SplitEquallyAll } )
    ) = do
  actionTextPayedFor action
  text " split equally to all"
actionText
    action@( ExpenseAction
      ( Expense { expenseSplit = SplitEqually [splitTo] } )
    ) = do
  actionTextPayedFor action
  text " for "
  text . T.pack . printUsersList . splitToToUsers $ splitTo
actionText
    action@( ExpenseAction
      ( Expense { expenseSplit = SplitEqually splitTos } )
    ) = do
  actionTextPayedFor action
  text " split equally to "
  text . T.pack . printUsersList . splitTosUsers $ splitTos
actionText
    action@( ExpenseAction
      ( Expense { expenseSplit = ItemizedSplit Nothing splits } )
    ) = do
  actionTextPayedFor action
  text " split in "
  text . T.pack . show . length $ splits
  text " items"
  return ()
actionText
    action@( ExpenseAction
      ( Expense { expenseSplit = ItemizedSplit (Just (Tips tips _)) splits } )
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
  => Dynamic t Int
  -> Dynamic t [User] -> Dynamic t [Group] -> Int -> Dynamic t ActionState
  -> m (Event t (Map Int ActionState -> Map Int ActionState))
actionWidget maxIx users groups ix stateDyn = unwrapEventWidget $ do
  state <- stateDyn
  maxIx <- maxIx
  case state of
    st@(ActionState False action) -> return . el "li" $ do
      actionText action
      editEv   <- actionLink "edit" st
      moveUpEv <- if ix /= 0
        then actionLink "up" ix
        else return never
      moveDownEv <- if ix /= maxIx
        then actionLink "down" ix
        else return never
      deleteEv <- actionLink "delete" st
      return . leftmost $
        [ M.delete ix <$ deleteEv
        , M.update (\st -> Just st { actionStateEdit = True }) ix <$ editEv
        , moveUp ix st <$ moveUpEv
        , moveDown ix st <$ moveDownEv
        ]
    st@(ActionState True action) -> return . el "li" $ do
      actionText action
      cancelEditEv <- actionLink "cancel edit" st
      moveUpEv <- if ix /= 0
        then actionLink "up" ix
        else return never
      moveDownEv <- if ix /= maxIx
        then actionLink "down" ix
        else return never
      deleteEv <- actionLink "delete" st
      el "br" blank
      actionEv <- nestedWidget ""
                  $ actionForm "Update" (Just action) users groups 
      return . leftmost $
        [ M.delete ix <$ deleteEv
        , (\action -> M.update (\_ -> Just (ActionState False action)) ix)
          <$> actionEv
        , M.update (\st -> Just st { actionStateEdit = False }) ix
          <$ cancelEditEv
        , moveUp ix st <$ moveUpEv
        , moveDown ix st <$ moveDownEv
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
    actionEv <- el "ul"
                $ listWithKeyOneEvent
                  actionsMap
                  ( actionWidget
                    (maybe 0 id . fmap fst . M.lookupMax <$> actionsMap)
                    users groups
                  )
  return actions

app
  :: ( Reflex t, DomBuilder t m, MonadHold t m
     , PostBuild t m, MonadFix m, WorkspaceStore s, MonadIO m )
  => s -> m ()
app store = do
  migrate store
  workspaceNames <- getWorkspaces store
  let initialWorkspaceState = case workspaceNames of
        [] -> InitialWorkspaceState
        [ws] | ws == defaultWorkspaceName -> InitialWorkspaceState
             | otherwise
               -> error
                $ printf
                  "The last workspace must always be '%s'"
                  defaultWorkspaceName
        wss@(firstWs:_) -> MultipleWorkspaceState firstWs wss 
  workspaceNameDyn <- manageWorkspaces store initialWorkspaceState
  dyn_ . ffor workspaceNameDyn $ \workspaceName -> do
    actions0 <- getActions store workspaceName
    rec
      users <- manageUsers (actionsUsers actions0) actions
      groups <- manageGroups (actionsGroups actions0) actions users
      actions <- manageActions (actionsArr actions0) users groups
    dyn_ (actions >>= (return . putActions store workspaceName))
    let nullified = (nullifyBalances . actionsToTransactions) <$> actions
    el "h2" $ text "Report"
    dyn_ (report <$> actions <*> nullified)
    return ()
