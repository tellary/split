{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module SplitReport where

import           Control.Monad       (forM_)
import           Control.Monad.Fix   (MonadFix)
import           Control.Monad.ListM (scanM)
import           Data.Function       ((&))
import qualified Data.Text           as T
import           ExpandableEl        (expandableContentLi)
import           MoneySplit          (Account, Actions (Actions), Amount,
                                      GramiticTime (Present),
                                      Negation (Affirmative),
                                      Purchase (Purchase, purchaseAmount,
                                                purchaseDesc, purchaseSplit,
                                                purchaseUser),
                                      Split (ItemizedSplit, SplitEqually),
                                      SplitItem (splitItemAmount,
                                                 splitItemDesc),
                                      Transaction (txAmount, txCreditAccount,
                                                   txDebitAccount),
                                      TxReason (TxReasonPayment,
                                                TxReasonPurchase),
                                      Voice (Active, Passive), accountUsers,
                                      actionsAccounts, addTips, balance,
                                      creditAccountTransactions,
                                      debitAccountTransactions,
                                      groupTransactionsByReason, groupsByUsers,
                                      printAccount, printAccountList,
                                      printAccountStatusOwedBy,
                                      printAccountStatusOwesTo,
                                      printSummaryBySingleReason,
                                      printUsersList, splitItemUsers,
                                      splitItemsForAccount, splitItemsUsers,
                                      splitTosToAccounts, userToAccount,
                                      usersToAccounts, verbForm)
import           Reflex.Dom          (DomBuilder, MonadHold, PostBuild, blank,
                                      el, text)
import           Text.Printf         (printf)

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

reportAccountSplitItems splitItems acc = do
  el "ul" . forM_ (splitItemsForAccount splitItems acc) $ \item -> do
    el "li" $ do
      text . T.pack . printUsersList . splitItemUsers $ item
      text ", "
      text . T.pack . splitItemDesc $ item
      text " -- "
      text . T.pack . show . splitItemAmount $ item

reportSplitItemsList actions splitItems  = do
  el "ul"
    . forM_
      (usersToAccounts actions . splitItemsUsers $ splitItems)
    $ \acc -> do
      el "li" $ do
        text . T.pack . printAccount $ acc
        text " -- "
        text . T.pack . show
          . sum . map splitItemAmount . splitItemsForAccount splitItems $ acc
        text ":"
        reportAccountSplitItems splitItems acc

reportAccountSingleReasonDetails
  :: DomBuilder t m => Actions -> (TxReason, [Transaction]) -> m ()
reportAccountSingleReasonDetails
      actions@( Actions _ groups _ )
      ( TxReasonPurchase
        ( Purchase
          { purchaseUser = purchaseUser
          , purchaseDesc = purchaseDesc
          , purchaseAmount = purchaseAmount
          , purchaseSplit = SplitEqually splitTos
          }
        )
      , txs) = do
  el "p" $ do
    text . T.pack
      . printAccountList
      . (\acc -> [acc])
      . userToAccount groupsByUsersVal
      $ purchaseUser
    text " payed "
    text . T.pack . show $ purchaseAmount
    text " for \""
    text . T.pack $ purchaseDesc
    text "\" split equally:"
    el "ul" . forM_ (splitTosToAccounts actions splitTos) $ \acc -> do
      el "li" $ do
        let amount = if purchaseUser `elem` accountUsers acc
                     then purchaseAmount + (balance acc $ txs)
                     else balance acc $ txs
        text . T.pack . show $ amount
        text " for "
        text . T.pack . printAccountList $ [acc]
  where groupsByUsersVal = groupsByUsers groups
reportAccountSingleReasonDetails
      actions@( Actions users groups _ )
      ( TxReasonPurchase
        ( Purchase
          { purchaseUser = purchaseUser
          , purchaseDesc = purchaseDesc
          , purchaseAmount = purchaseAmount
          , purchaseSplit = ItemizedSplit tips splitItems
          }
        )
      , _)
  = case accounts of
      [acc] -> reportAccountSplitItems splitItemsWithTips acc
      (_:_:_) -> el "p" $ do
        text . T.pack
          . printAccountList
          . (\acc -> [acc])
          . userToAccount groupsByUsersVal
          $ purchaseUser
        text " payed "
        text . T.pack . show $ purchaseAmount
        text " for \""
        text . T.pack $ purchaseDesc
        text "\" split as follows:"
        reportSplitItemsList actions splitItemsWithTips
      [] -> error "'ItemizedSplit' should split amoung at least 1 account"
  where
    splitItemsWithTips = addTips users tips splitItems
    groupsByUsersVal = groupsByUsers groups
    accounts = usersToAccounts actions . splitItemsUsers $ splitItems
reportAccountSingleReasonDetails _ (reason, _) = do
  el "p" $ do
    text "More details TBD, tx group reason: "
    text . T.pack . show $ reason

reportAccountSingleReasonWithSummary _ summary (TxReasonPayment, _)
  = (el "li" .  text . T.pack $ summary)
reportAccountSingleReasonWithSummary
      _ summary
      ( TxReasonPurchase
        ( Purchase { purchaseSplit = SplitEqually [_] } )
        ,
        _
      )
  = (el "li" .  text . T.pack $ summary)
reportAccountSingleReasonWithSummary
      actions summary reasonGroup@(TxReasonPurchase _, _)
  = expandableContentLi
    (text . T.pack $ summary)
    (text . T.pack $ summary)
    (reportAccountSingleReasonDetails actions reasonGroup)

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
          reportAccountSingleReasonWithSummary
            actions summary reasonGroup
          return total'
        Nothing -> return total'
  | otherwise = do
      printSummaryBySingleReason actions acc reasonGroup & \case
        Just summary -> do
          let summaryAndTotal
                = summary
                  ++ ", total: "
                  ++ show (if owes then total' else -total')
          reportAccountSingleReasonWithSummary
            actions summaryAndTotal reasonGroup
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
