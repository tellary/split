{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module WorkspaceStore where

import Control.Monad.IO.Class (MonadIO)
import Debug.Trace            (trace)
import MoneySplit             (Action (ExpenseAction), Actions (Actions),
                               Expense (Expense), Split (SplitEquallyAll),
                               actions3)

type WorkspaceName = String

defaultWorkpsaceName = "Default"

class WorkspaceStore s where
  putActions :: MonadIO m => s -> WorkspaceName -> Actions -> m ()
  getActions :: MonadIO m => s -> WorkspaceName -> m Actions
  putWorkspaces :: MonadIO m => s -> [WorkspaceName] -> m ()
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
    | workspaceName == defaultWorkpsaceName = return defaultActions
    | workspaceName == "Coimbra trip" = return actions3
    | otherwise = error $ "Unexpected workspace: " ++ workspaceName
  putWorkspaces _ workspaces = trace ("putWorkspaces: " ++ show workspaces) $ return ()
  getWorkspaces _ = return [defaultWorkpsaceName, "Coimbra trip"]
