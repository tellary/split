{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM_)
import qualified Data.Text     as T
import           MoneySplit
import           Reflex.Dom    (el, mainWidget, text, DomBuilder)
import           Text.Printf   (printf)

main :: IO ()
main = mainWidget $ do
  report actions3 nullify3
  report actions2 nullify2
  report actions1 nullify1

printAccountBalance acc txs
  = printf "%s %s %s"
    ( printAccount acc )
    ( verbForm acc "owe" Present voice Affirmative )
    ( show . abs $ b )
  where
    b     = balance acc txs
    voice = if b < 0 then Active else Passive

reportAccountStatusOwesTo :: DomBuilder t m => Account -> Amount -> [Transaction] -> m ()
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

report actions (txNew, txOld) = do
  el "ul" . forM_ (actionsAccounts actions) $ \acc -> do
    el "li" $ reportAccountStatus acc txNew
