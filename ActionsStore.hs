{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ActionsStore where

import Control.Monad.IO.Class (MonadIO)
import Debug.Trace            (trace)
import MoneySplit             (Action (PurchaseAction), Actions (Actions),
                               Purchase (Purchase), Split (SplitEquallyAll))

class ActionsStore s where
  putActions :: MonadIO m => s -> Actions -> m ()
  getActions :: MonadIO m => s -> m Actions

data StubActionsStore = StubActionsStore

instance ActionsStore StubActionsStore where
  putActions _ _ = trace "putActions" $ return ()
  getActions _
    = return
      $ Actions
        ["Ilya", "Tasha", "Dima", "Alena", "Aigiza"]
        []
        [ PurchaseAction
          ( Purchase "Ilya" "AirBnb" 442 SplitEquallyAll )
        ]
