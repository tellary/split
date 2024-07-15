{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import           Control.Monad       (forM_)
import           Control.Monad.ListM (scanM)
import           Data.Function       ((&))
import qualified Data.Text           as T
import           ExpandableEl        (expandableContentLi)
import           MoneySplit
import           Reflex.Dom          (DomBuilder, blank, el, mainWidget, text)
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

reportAccountPurchases actions acc txs = do
  let (firstReason:reasonsAndTxs) = groupTransactionsByReason txs
  el "ul" $ do
    printSummaryBySingleReason actions acc firstReason & \case
      Just summary -> el "li" .  text . T.pack $ summary
      Nothing -> return ()
    _ :: [Amount] <- scanM
      (\total reasonAndTxs -> do
          let total' = total + (balance acc . snd $ reasonAndTxs)
          printSummaryBySingleReason actions acc reasonAndTxs & \case
            Just summary -> el "li" $ do
              text . T.pack $ summary
              text ", total: "
              text . T.pack . show $
                if owes
                then total'
                else -total'
            Nothing -> return ()
          return total'
      )
      (balance acc . snd $ firstReason)
      reasonsAndTxs
    return ()
  where owes = balance acc txs > 0

report actions (txsNew, txsOld) = do
  el "ul" . forM_ (actionsAccounts actions) $ \acc -> do
    expandableContentLi
      ( reportAccountStatus acc txsNew )
      ( reportAccountStatus acc txsNew )
      ( el "p" $ do
          text . T.pack . printAccount $ acc
          el "br" blank
          reportAccountPurchases actions acc txsOld
      )
