{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module WorkspaceStore where

import Control.Monad          (filterM, forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Debug.Trace            (trace)
import MoneySplit             (Action (ExpenseAction), Actions (Actions),
                               Expense (Expense), Split (SplitEquallyAll),
                               actions1, actions2, actions3, actionsAreEmpty)
import Text.Printf            (printf)

newtype WorkspaceId = WorkspaceId String deriving (Show, Eq)
type WorkspaceName = String
data Workspace
  = Workspace
  { workspaceId :: WorkspaceId
  , workspaceName :: WorkspaceName
  } deriving (Show, Eq)

defaultWorkspaceName = "Default"

class Show s => WorkspaceStore s where
  createWorkspace :: MonadIO m => s -> WorkspaceName -> m Workspace
  renameWorkspace :: MonadIO m
    => s -> WorkspaceId -> WorkspaceName -> m Workspace
  copyWorkspace :: MonadIO m
    => s -> WorkspaceId -> WorkspaceName -> m Workspace
  copyWorkspace this wsId wsName = do
    newWs <- createWorkspace this wsName
    actions <- getActions this wsId
    putActions this (workspaceId newWs) actions
    return newWs
  putActions :: MonadIO m => s -> WorkspaceId -> Actions -> m ()
  getActions :: MonadIO m => s -> WorkspaceId -> m Actions
  deleteWorkspace :: MonadIO m => s -> WorkspaceId -> m ()
  wipeWorkspace :: MonadIO m => s -> WorkspaceId -> m ()
  getWorkspaces :: MonadIO m => s -> m [Workspace]
  migrate :: MonadIO m => s -> m ()

-- | Make sure that the 'Default' workspace exists
createDefaultWorkspace store = do
  wss <- getWorkspaces store
  if null . filter (\ws -> workspaceName ws == defaultWorkspaceName) $ wss
    then do
      liftIO . putStrLn
        $ printf
          "createDefaultWorkspace: %s: No default workspace found"
          (show store)
      ws <- createWorkspace store defaultWorkspaceName
      putActions store (workspaceId ws) (Actions [] [] [])
    else do
      liftIO . putStrLn
        $ printf
          "createDefaultWorkspace: %s: '%s' workspace found"
          (show store) defaultWorkspaceName

removeEmptyDefaultWorkspaces store = do
  wss <- getWorkspaces store
  let defaultWss
        = filter (\ws -> workspaceName ws == defaultWorkspaceName) $ wss
  if length defaultWss == 1
    then return ()
    else do
      emptyDefaultWss <- filterM
        ( \ws -> do
            actions <- getActions store (workspaceId ws)
            return $ actionsAreEmpty actions
        ) $ defaultWss
      let emptyDefaultWssToRemove
            = if length emptyDefaultWss == length defaultWss
                 && not (null emptyDefaultWss)
              then tail emptyDefaultWss
              else emptyDefaultWss
      forM_ emptyDefaultWssToRemove $ \ws -> do
        deleteWorkspace store (workspaceId ws)

workspaceStoreCleanup store finalMigrationStep = do
  when finalMigrationStep $ createDefaultWorkspace store
  removeEmptyDefaultWorkspaces store

copyWorkspaces oldStore newStore = do
  oldWss <- getWorkspaces oldStore
  newWss <- getWorkspaces newStore
  forM_ oldWss $ \oldWs -> do
    when (not $ oldWs `elem` newWss) $ do
      liftIO . putStrLn
        $ printf
          ( "copyWorkspaces: Old workspace '%s' from %s "
            ++ "doesn't exist in new WorkspaceStore %s"
          )
          (show oldWs) (show oldStore) (show newStore)
      newWs <- createWorkspace newStore (workspaceName oldWs)
      actions <- getActions oldStore (workspaceId oldWs)
      putActions newStore (workspaceId newWs) actions
    deleteWorkspace oldStore (workspaceId oldWs)

data StubWorkspaceStore = StubWorkspaceStore deriving Show

defaultActions
  = Actions
    ["Ilya", "Tasha", "Dima", "Alena", "Aigiza"]
    [["Ilya", "Tasha"]]
    [ ExpenseAction
      ( Expense "Ilya" "AirBnb" 442 SplitEquallyAll )
    ]

instance WorkspaceStore StubWorkspaceStore where
  createWorkspace _ workspaceName
    = trace ("createWorkspace: " ++ workspaceName)
    . return
    $ Workspace (WorkspaceId (workspaceName ++ " (id)")) workspaceName
  renameWorkspace _ workspaceId workspaceName
    = trace (printf "renameWorkspace: %s %s" (show workspaceId) workspaceName)
    . return
    $ Workspace (WorkspaceId (workspaceName ++ " (id)")) workspaceName
  putActions _ (WorkspaceId workspaceId) _
    = trace ("putActions: " ++ workspaceId) $ return ()
  getActions _ (WorkspaceId workspaceId)
    | workspaceId == "Default (id)" = return defaultActions
    | workspaceId == "Serge houseworming (id)" = return actions1
    | workspaceId == "Nick's birthday (id)" = return actions2
    | workspaceId == "Coimbra trip (id)" = return actions3
    | otherwise = return $ Actions [] [] []
  deleteWorkspace _ (WorkspaceId workspace)
    = trace ("deleteWorkspace: " ++ show workspace) $ return ()
  wipeWorkspace _ (WorkspaceId workspace)
    = trace ("wipeWorkspace: " ++ show workspace) $ return ()
  getWorkspaces _
    = return
    [ Workspace (WorkspaceId "Default (id)") defaultWorkspaceName
    , Workspace (WorkspaceId "Coimbra trip (id)") "Coimbra trip"
    ]

  migrate _ = trace "migrate" $ return ()
