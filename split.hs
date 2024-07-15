{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import           Control.Monad       (forM_)
import           Control.Monad.Fix   (MonadFix)
import           Control.Monad.ListM (scanM)
import           Data.Function       ((&))
import qualified Data.Text           as T
import           ExpandableEl        (expandableContentLi)
import           MoneySplit
import           Reflex.Dom          (DomBuilder, MonadHold, PostBuild, blank,
                                      el, mainWidget, text)
import           Text.Printf         (printf)

main :: IO ()
main = mainWidget $ do
  report actions3 nullify3
  text "------"; el "br" blank
  report actions2 nullify2
  text "------"; el "br" blank
  report actions1 nullify1

reportAccountStatusOwesTo
  :: DomBuilder t m => Account -> Amount -> [Transaction] -> m ()
reportAccountStatusOwesTo acc balance [tx]
  = text . T.pack $ printAccountStatusOwesTo acc balance [tx]
reportAccountStatusOwesTo acc balance txs = do
  text . T.pack
    $ printf "%s %s %s"
      ( printAccount acc)
      ( verbForm acc "owe" Present Active Affirmative )
      ( show . abs $ balance )
  el "ul" $ do
    forM_ txs $ \tx -> do
      el "li" . text . T.pack
        $ printf "%s to %s"
          ( show . txAmount $ tx )
          ( printAccount . txCreditAccount $ tx )

reportAccountStatusOwedBy acc balance [tx]
  = text . T.pack $ printAccountStatusOwedBy acc balance [tx]
reportAccountStatusOwedBy acc balance txs = do
  text . T.pack
    $ printf "%s %s %s"
      ( printAccount acc )
      ( verbForm acc "owe" Present Passive Affirmative )
      ( show balance )
  el "ul" $ do
    forM_ txs $ \tx -> do
      el "li" . text . T.pack
        $ printf "%s by %s"
          ( show . txAmount $ tx )
          ( printAccount . txDebitAccount $ tx )

-- | Creates an HTML snippet showing status who owes to an account,
-- or who the account owes to.
reportAccountStatus acc txs
  | b < 0     = reportAccountStatusOwesTo acc b
                (debitAccountTransactions acc txs)
  | otherwise = reportAccountStatusOwedBy acc b
                (creditAccountTransactions acc txs)
  where b = balance acc txs

reportAccountSingleReasonDetails
  :: DomBuilder t m => (TxReason, [Transaction]) -> m ()
reportAccountSingleReasonDetails (reason, txs) = do
  (el "p" $ text "More details")
reportAccountSingleReason
  :: forall t m . (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Actions
  -> Account
  -> Bool
  -> Amount
  -> (TxReason, [Transaction]) -> m Amount
reportAccountSingleReason actions acc owes total reasonGroup
  | total ==  0  = do
      printSummaryBySingleReason actions acc reasonGroup & \case
        Just summary -> do
          expandableContentLi
            (el "li" .  text . T.pack $ summary)
            (el "li" .  text . T.pack $ summary)
            (reportAccountSingleReasonDetails reasonGroup)
          return total'
        Nothing -> return total'
  | otherwise = do
      printSummaryBySingleReason actions acc reasonGroup & \case
        Just summary -> do
          let summaryAndTotal :: m () = do
                text . T.pack $ summary
                text ", total: "
                text . T.pack . show $ if owes then total' else -total'
          expandableContentLi
            summaryAndTotal
            summaryAndTotal
            (reportAccountSingleReasonDetails reasonGroup)
          return total'
        Nothing -> return total'
  where
    total' = total + (balance acc . snd $ reasonGroup)

reportAccountReasons actions acc txs = do
  let owes = balance acc txs > 0
  el "ul" $ do
    _ :: [Amount] <- scanM
      (reportAccountSingleReason actions acc owes)
      0
      (groupTransactionsByReason txs)
    return ()

report actions (txsNew, txsOld) = do
  el "ul" . forM_ (actionsAccounts actions) $ \acc -> do
    expandableContentLi
      ( reportAccountStatus acc txsNew )
      ( reportAccountStatus acc txsNew )
      ( el "p" $ do
          text . T.pack . printAccount $ acc
          el "br" blank
          reportAccountReasons actions acc txsOld
      )
