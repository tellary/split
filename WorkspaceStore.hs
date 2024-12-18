{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module WorkspaceStore where

import Control.Monad.IO.Class (MonadIO)
import Debug.Trace            (trace)
import MoneySplit             (Action (ExpenseAction), Actions (Actions),
                               Expense (Expense), Split (SplitEquallyAll),
                               actions1, actions2, actions3)

newtype WorkspaceId = WorkspaceId String deriving (Show, Eq)
type WorkspaceName = String
data Workspace
  = Workspace
  { workspaceId :: WorkspaceId
  , workspaceName :: WorkspaceName
  } deriving (Show, Eq)

defaultWorkspaceName = "Default"

class WorkspaceStore s where
  createWorkspace :: MonadIO m => s -> WorkspaceName -> m Workspace
  putActions :: MonadIO m => s -> WorkspaceId -> Actions -> m ()
  getActions :: MonadIO m => s -> WorkspaceId -> m Actions
  deleteWorkspace :: MonadIO m => s -> WorkspaceId -> m ()
  wipeWorkspace :: MonadIO m => s -> WorkspaceId -> m ()
  getWorkspaces :: MonadIO m => s -> m [Workspace]
  migrate :: MonadIO m => s -> m ()

data StubWorkspaceStore = StubWorkspaceStore

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
