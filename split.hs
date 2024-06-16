{-# LANGUAGE LambdaCase #-}

import Data.Decimal       (Decimal)
import Data.List          (find, group, groupBy, sort, sortBy, sortOn)
import Data.List.Extra    (groupOn)
import Data.Maybe         (fromJust, isJust)
import Text.Pretty.Simple (pPrint)

type Amount = Decimal
type User = String
type Group = [User]
data Account = UserAccount User | GroupAccount Group deriving (Show, Eq, Ord)
type DebitAccount = Account
type CreditAccount = Account

round2 :: Decimal -> Decimal
round2 d = fromIntegral (round (d*100))/100

divAmounts :: Decimal -> Int -> [Decimal]
divAmounts d n = loop d n []
  where
    part :: Decimal
    part = round2 (d/fromIntegral n)
    loop :: Decimal -> Int -> [Decimal] -> [Decimal]
    loop left 1 result = left:result
    loop left n result = loop (left - part) (n - 1) (part:result)

data Transaction
  = Transaction
  { txDebitAccount :: DebitAccount
  , txCreditAccount :: CreditAccount
  , txAmount :: Amount
  } deriving (Show, Eq, Ord)

mkTransaction :: Account -> Account -> Amount -> Transaction
mkTransaction creditAcc debitAcc amount
  | amount >= 0 = Transaction creditAcc debitAcc   amount
  | otherwise   = Transaction debitAcc creditAcc (-amount)

isCredit acc (Transaction _ creditAccount _) = acc == creditAccount
isDebit acc (Transaction debitAccount _ _) = acc == debitAccount
creditAmount acc tx@(Transaction _ _ amount)
  | isCredit acc tx = Just amount
  | otherwise = Nothing
debitAmount acc tx@(Transaction _ _ amount)
  | isDebit acc tx = Just amount
  | otherwise = Nothing
debitTransactions :: Account -> [Transaction] -> [Transaction]
debitTransactions account = filter ((account ==) . txDebitAccount)
creditTransactions :: Account -> [Transaction] -> [Transaction]
creditTransactions account = filter ((account ==) . txCreditAccount)
transactionAccountsDirectional :: Transaction -> (DebitAccount, CreditAccount)
transactionAccountsDirectional (Transaction debitAccount creditAccount _)
  = (debitAccount, creditAccount)
transactionAccountsDirectionalArr :: Transaction -> [Account]
transactionAccountsDirectionalArr (Transaction debitAccount creditAccount _)
  = [debitAccount, creditAccount]
transactionAccounts tx
  = let [a1, a2] = sort . transactionAccountsDirectionalArr $ tx
    in (a1, a2)
transactionsAccounts
  = map head . group . sort . concat .  map transactionAccountsDirectionalArr
twoAccounts :: [Transaction] -> (Account, Account)
twoAccounts txs
  | length accs == 2 = (head accs, head . tail $ accs)
  | otherwise = error $ "Only two accounts expected, but found " ++ show accs
  where accs = transactionsAccounts txs
  
sameAccountsDirectional :: Transaction -> Transaction -> Bool
sameAccountsDirectional tx1 tx2
  = transactionAccountsDirectional tx1 == transactionAccountsDirectional tx2
sameAccounts :: Transaction -> Transaction -> Bool
sameAccounts tx1 tx2 = transactionAccounts tx1 == transactionAccounts tx2

data Purchase = Purchase User Amount Split deriving (Show, Eq)
data Split
  = SplitEqually [User]
  | SplitEquallyAll
  deriving (Show, Eq)

data Action = PurchaseAction Purchase | PaymentAction Transaction deriving Show
data Actions = Actions [User] [Group] [Action] deriving Show

actionsToTransactions :: Actions -> [Transaction]
actionsToTransactions actions@(Actions _ _ actionsArr)
  = concat . map (toTransactions actions) $ actionsArr

groupsByUsers :: [Group] -> [(User, Group)]
groupsByUsers
  = foldl
    (\result g ->
        foldl
        (\groupByUsers user -> (user, g):groupByUsers) [] g ++ result)
    []

t4 = groupsByUsers [["Ilya", "Tasha"], ["Alena", "Dima"]]

userToAccount groupsByUsers user
  = case lookup user groupsByUsers of
      Nothing -> UserAccount user
      Just group -> GroupAccount group

toTransactions :: Actions -> Action -> [Transaction]
toTransactions (Actions _ groups _) (PurchaseAction
                  (Purchase debitUser amount (SplitEqually users)))
  = map (
      \(user, amount) ->
        Transaction
        (userToAccount groupsByUsersVal debitUser)
        (userToAccount groupsByUsersVal user)
        amount
    )
  . filter (\(user, _) -> user /= debitUser)
  $ zip users (divAmounts amount (length users))
  where groupsByUsersVal = groupsByUsers groups
toTransactions
  actions@(Actions users _ _)
  (PurchaseAction
    (Purchase debitUser amount SplitEquallyAll))
  = toTransactions actions
    (PurchaseAction
     (Purchase debitUser amount (SplitEqually users)))

balance :: Account -> [Transaction] -> Amount
balance account transactions
  = ( sum . fromJust . sequence . filter isJust . map (creditAmount account)
      $ transactions )
  - ( sum . fromJust . sequence . filter isJust . map (debitAmount account)
      $ transactions )

accountsBalances txs
  = map (\acc -> (balance acc txs, acc)) accs
  where
    accs = transactionsAccounts txs

collapseTwoAccountsTransactions txs
  | balanceAmount >  0 = Just $ Transaction a1 a2 balanceAmount
  | balanceAmount == 0 = Nothing
  | otherwise          = Just $ Transaction a2 a1 (- balanceAmount)
  where (a1, a2) = twoAccounts txs
        balanceAmount = balance a2 txs

collapseTransactions key
  = map fromJust
  . filter isJust
  . map collapseTwoAccountsTransactions
  . groupOn key
  . sortOn key
  . filter (\tx -> txDebitAccount tx /= txCreditAccount tx)

collapseSameAccountsDirectional
  = collapseTransactions transactionAccountsDirectional

collapseSameAccounts
  = collapseTransactions transactionAccounts

decreaseBalance balances
  = case (asc, desc) of
      ((lowest, lowestAcc):_, (highest, highestAcc):_)
        | abs lowest >= highest -> Transaction highestAcc lowestAcc  highest
        | otherwise             -> Transaction highestAcc lowestAcc (abs lowest)
  where
    asc  = sort balances
    desc = sortBy (flip compare) balances

eqZero amt = abs amt < 0.001

nullifyBalances0 newTxs txs
  = case find (not . eqZero . fst) $ balances of
      Just _  -> nullifyBalances0 (decreaseBalance balances:newTxs) txs
      Nothing -> (newTxs, txs)
  where
    balances = accountsBalances allTxs
    allTxs = newTxs ++ txs

nullifyBalances = nullifyBalances0 []

users1 = ["Serge", "Sasha", "Pasha", "Ilya", "Tasha", "Kolya", "Alena", "Dima"]

actions1
  = Actions users1 [["Dima", "Alena"], ["Sasha", "Pasha"]]
    [ PurchaseAction (Purchase "Serge" 100.25 SplitEquallyAll)
    , PurchaseAction (Purchase "Serge" 14.05 (SplitEqually ["Ilya"]))
    , PurchaseAction (Purchase "Dima"  21.64  SplitEquallyAll)
    , PurchaseAction (Purchase "Ilya"  14     SplitEquallyAll)
    , PurchaseAction
      (Purchase "Ilya" 24
       (SplitEqually ["Ilya", "Kolya", "Alena", "Dima", "Tasha"]))
    ]

t1
  = groupOn transactionAccounts
  . sortOn transactionAccounts
  . actionsToTransactions
  $ actions1

nullify1 = nullifyBalances . actionsToTransactions $ actions1

users2 = ["Tasha", "Ilya", "Alena", "Niki", "Dmitry", "Serge"]
actions2
  = Actions users2 [["Dmitry", "Alena"]]
    [ PurchaseAction (Purchase "Tasha" 9     SplitEquallyAll)
    , PurchaseAction (Purchase "Ilya"  140   SplitEquallyAll)
    , PurchaseAction (Purchase "Alena" 9.6   SplitEquallyAll)
    , PurchaseAction (Purchase "Alena" 13.75 SplitEquallyAll)
    ]

nullify2 = nullifyBalances . actionsToTransactions $ actions2

users3 = ["Tasha", "Ilya", "Alena", "Dima", "Aigiza"]
actions3
  = Actions users3 [["Dima", "Alena"], ["Tasha", "Ilya"]]
    [ -- AirBnB, USD/EUR = 0.9211 as of May 25th
      PurchaseAction
      ( Purchase "Ilya" (round2 (478.40/0.9211)) SplitEquallyAll )
    , -- Pingo Doce Óbidos
      PurchaseAction (Purchase "Ilya" 178.47   SplitEquallyAll)
    , -- Gasoline
      PurchaseAction (Purchase "Ilya" 58.83    SplitEquallyAll)
    , -- Road tolls x2
      PurchaseAction (Purchase "Ilya" (2*13.8) SplitEquallyAll)
    , -- Pingo Doce Coimbra
      PurchaseAction (Purchase "Ilya" 41.86    SplitEquallyAll)
    , -- Padaria Flor de Aveiro
      PurchaseAction
      ( Purchase "Dima" 19.45
        ( SplitEqually ["Dima", "Alena", "Tasha", "Ilya"] )
      )
    , -- Cafe Papa
      PurchaseAction (Purchase "Ilya" 77.5     SplitEquallyAll)
    , -- Cafe Trazarte in Óbidos
      PurchaseAction (Purchase "Aigiza" 65.4   SplitEquallyAll)
    ]

nullify3 = nullifyBalances . actionsToTransactions $ actions3

printNullify3 :: IO ()
printNullify3 = pPrint nullify3
