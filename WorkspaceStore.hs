{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module WorkspaceStore where

import Control.Monad.IO.Class (MonadIO)
import Debug.Trace            (trace)
import MoneySplit             (Action (ExpenseAction), Actions (Actions),
                               Expense (Expense), Split (SplitEquallyAll),
                               actions1, actions2, actions3)

type WorkspaceName = String

defaultWorkspaceName = "Default"

class WorkspaceStore s where
  putActions :: MonadIO m => s -> WorkspaceName -> Actions -> m ()
  getActions :: MonadIO m => s -> WorkspaceName -> m Actions
  deleteWorkspace :: MonadIO m => s -> WorkspaceName -> m ()
  getWorkspaces :: MonadIO m => s -> m [WorkspaceName]

data StubWorkspaceStore = StubWorkspaceStore

defaultActions
  = Actions
    ["Ilya", "Tasha", "Dima", "Alena", "Aigiza"]
    [["Ilya", "Tasha"]]
    [ ExpenseAction
      ( Expense "Ilya" "AirBnb" 442 SplitEquallyAll )
    ]

instance WorkspaceStore StubWorkspaceStore where
  putActions _ workspaceName _
    = trace ("putActions: " ++ workspaceName) $ return ()
  getActions _ workspaceName
    | workspaceName == defaultWorkspaceName = return defaultActions
    | workspaceName == "Serge houseworming" = return actions1
    | workspaceName == "Nick's birthday" = return actions2
    | workspaceName == "Coimbra trip" = return actions3
    | otherwise = return $ Actions [] [] []
  deleteWorkspace _ workspace
    = trace ("deleteWorkspace: " ++ show workspace) $ return ()
  getWorkspaces _ = return [defaultWorkspaceName, "Coimbra trip"]
