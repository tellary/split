{-# LANGUAGE LambdaCase #-}

module MoneySplit where

import Data.Decimal       (Decimal)
import Data.List          (find, group, groupBy, intercalate, nub, sort, sortBy,
                           sortOn)
import Data.List.Extra    (groupOn)
import Data.Maybe         (fromJust, isJust)
import Text.Pretty.Simple (pPrint)
import Text.Printf        (printf)

type Amount = Decimal
type User = String
type Desc = String
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

data TxReason = TxReasonPurchase Purchase | TxReasonPayment
  deriving (Show, Eq, Ord)

data Transaction
  = Transaction
  { txDebitAccount :: DebitAccount
  , txCreditAccount :: CreditAccount
  , txAmount :: Amount
  , txReason :: TxReason
  } deriving (Show, Eq, Ord)

isCredit acc (Transaction _ creditAccount _ _) = acc == creditAccount
isDebit acc (Transaction debitAccount _ _ _) = acc == debitAccount
creditAmount acc tx@(Transaction _ _ amount _)
  | isCredit acc tx = Just amount
  | otherwise = Nothing
debitAmount acc tx@(Transaction _ _ amount _)
  | isDebit acc tx = Just amount
  | otherwise = Nothing
accountTransactions :: Account -> [Transaction] -> [Transaction]
accountTransactions account
  = filter
    (\tx -> account == txDebitAccount tx || account == txCreditAccount tx)
debitAccountTransactions :: Account -> [Transaction] -> [Transaction]
debitAccountTransactions account = filter ((account ==) . txDebitAccount)
creditAccountTransactions :: Account -> [Transaction] -> [Transaction]
creditAccountTransactions account = filter ((account ==) . txCreditAccount)
transactionAccountsDirectional :: Transaction -> (DebitAccount, CreditAccount)
transactionAccountsDirectional (Transaction debitAccount creditAccount _ _)
  = (debitAccount, creditAccount)
transactionAccountsDirectionalArr :: Transaction -> [Account]
transactionAccountsDirectionalArr (Transaction debitAccount creditAccount _ _)
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

sameReason :: [Transaction] -> TxReason
sameReason txs
  | length reasonGroups == 1 = head reasonGroups
  | otherwise
  = error $ "Only one transaction reason expected, but found "
    ++ show reasonGroups
  where reasonGroups = map head . group . sort . map txReason $ txs

sameAccountsDirectional :: Transaction -> Transaction -> Bool
sameAccountsDirectional tx1 tx2
  = transactionAccountsDirectional tx1 == transactionAccountsDirectional tx2
sameAccounts :: Transaction -> Transaction -> Bool
sameAccounts tx1 tx2 = transactionAccounts tx1 == transactionAccounts tx2

data Purchase = Purchase User Desc Amount Split deriving (Show, Eq, Ord)
data Split
  = SplitEqually [User]
  | SplitEquallyAll
  deriving (Show, Eq, Ord)

data Action = PurchaseAction Purchase | PaymentAction Transaction deriving Show
data Actions = Actions [User] [Group] [Action] deriving Show

actionsAccounts :: Actions -> [Account]
actionsAccounts (Actions users groups _)
  = nub . map (userToAccount groupsByUsersVal) $ users
  where
    groupsByUsersVal = groupsByUsers groups

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
                  purchase@(Purchase debitUser _ amount (SplitEqually users)))
  = collapseSameAccounts
  . filter (\tx -> txDebitAccount tx /= txCreditAccount tx)
  . map (
      \(user, amount) ->
        Transaction
        (userToAccount groupsByUsersVal debitUser)
        (userToAccount groupsByUsersVal user)
        amount
        (TxReasonPurchase purchase)
    )
  $ zip users (divAmounts amount (length users))
  where groupsByUsersVal = groupsByUsers groups
toTransactions
  actions@(Actions users _ _)
  (PurchaseAction
    (Purchase debitUser desc amount SplitEquallyAll))
  = toTransactions actions
    (PurchaseAction
     (Purchase debitUser desc amount (SplitEqually users)))
toTransactions _ (PaymentAction tx) = [tx]

balance :: Account -> [Transaction] -> Amount
balance account transactions
  = ( sum . fromJust . sequence . filter isJust . map (creditAmount account)
      $ transactions )
  - ( sum . fromJust . sequence . filter isJust . map (debitAmount account)
      $ transactions )

accountsBalances :: [Transaction] -> [(Amount, Account)]
accountsBalances txs
  = map (\acc -> (balance acc txs, acc)) accs
  where
    accs = transactionsAccounts txs

collapseSameTransactions :: [Transaction] -> Maybe Transaction
collapseSameTransactions txs
  | balanceAmount >  0 = Just $ Transaction a1 a2 balanceAmount reason
  | balanceAmount == 0 = Nothing
  | otherwise          = Just $ Transaction a2 a1 (- balanceAmount) reason
  where (a1, a2) = twoAccounts txs
        balanceAmount = balance a2 txs
        reason = sameReason txs

-- | Collapses transactions with the same debit account, credit account and reason
collapseTransactions key
  = map fromJust
  . filter isJust
  . map collapseSameTransactions
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
        | abs lowest >= highest
          -> Transaction highestAcc lowestAcc  highest     TxReasonPayment
        | otherwise
          -> Transaction highestAcc lowestAcc (abs lowest) TxReasonPayment
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

printAccount (UserAccount user) = user
printAccount (GroupAccount [user1, user2]) = user1 ++ " and " ++ user2

data GramiticTime = Present | Past
data Voice = Active | Passive
data Negation = Affirmative | Negative

verbForm  acc             "owe" tense   Passive Affirmative
  = verbForm acc "ow" tense Passive Affirmative
verbForm (UserAccount _)  "do"  tense   Active  Negative = "doesn't"
verbForm (GroupAccount _) "do"  tense   Active  Negative = "don't"
verbForm (UserAccount _)  verb  Present Active  Affirmative
  = verb ++ "s"
verbForm (GroupAccount _) verb  Present Active  Affirmative
  = verb
verbForm (UserAccount _)  verb  Present Active  Negative
  = "is " ++ verb ++ "ed"
verbForm (GroupAccount _) verb  Present Active  Negative
  = "are " ++ verb ++ "ed"
verbForm (UserAccount _)  verb  Present Passive Affirmative
  = "is " ++ verb ++ "ed"
verbForm (GroupAccount _) verb  Present Passive Affirmative
  = "are " ++ verb ++ "ed"
verbForm (UserAccount _)  verb  Past    Passive Affirmative
  = "was " ++ verb ++ "ed"
verbForm (GroupAccount _) verb  Past    Passive Affirmative
  = "were " ++ verb ++ "ed"

printAccountStatusOwesTo acc _ [tx]
  = printf "%s %s %s to %s"
    ( printAccount acc )
    ( verbForm acc "owe" Present Active Affirmative )
    ( show . txAmount $ tx )
    ( printAccount . txCreditAccount $ tx )
printAccountStatusOwesTo acc balance txs
  = printf "%s %s %s\n\n%s"
    ( printAccount acc)
    ( verbForm acc "owe" Present Active Affirmative )
    ( show . abs $ balance )
    ( intercalate "\n"
      . map
        (\tx -> printf "- %s to %s"
                (show . txAmount $ tx)
                (printAccount . txCreditAccount $ tx)
        )
      $ txs
    )

printAccountStatusOwedBy acc _ [tx]
  = printf "%s %s %s by %s"
    ( printAccount acc )
    ( verbForm acc "owe" Present Passive Affirmative )
    ( show . txAmount $ tx )
    ( printAccount . txDebitAccount $ tx )
printAccountStatusOwedBy acc balance txs
  = printf "%s %s %s\n\n%s"
    ( printAccount acc )
    ( verbForm acc "owe" Present Passive Affirmative )
    ( show balance )
    ( intercalate "\n"
      . map
        (\tx -> printf "- %s by %s"
                (show . txAmount $ tx)
                (printAccount . txDebitAccount $ tx)
        )
      $ txs
    )

-- | Prints status of the account
-- 
-- - First how much the account owes or is owed
-- - To whom or by whom
printAccountStatus :: Account -> [Transaction] -> String
printAccountStatus acc []
  = printf "%s %s owe anything"
    (printAccount acc)
    (verbForm acc "do" Present Active Negative)
printAccountStatus acc txs
  | b < 0     = printAccountStatusOwesTo acc b txs
  | otherwise = printAccountStatusOwedBy acc b txs
  where b = balance acc txs

printAmount :: Transaction -> String
printAmount
      ( Transaction
        { txAmount = amount
        , txReason = TxReasonPurchase
                     (Purchase _ _ purchaseAmount _)
        }
      )
  | amount == purchaseAmount = show amount
  | otherwise = printf "%s out of %s" (show amount) (show purchaseAmount)

printTransaction :: Transaction -> String
printTransaction
      tx@( Transaction
           { txReason = TxReasonPurchase
                        (Purchase _ purchaseDesc purchaseAmount _)
           }
         )
  = printf "%s payed %s for %s for %s"
    (printAccount . txDebitAccount $ tx)
    (printAmount tx)
    (printAccount . txCreditAccount $ tx)
    purchaseDesc
printTransaction tx@( Transaction { txReason = TxReasonPayment } )
  = printf "%s payed %s to %s"
    (printAccount . txDebitAccount $ tx)
    (show . txAmount $ tx)
    (printAccount . txCreditAccount $ tx)

printTransactions :: [Transaction] -> String
printTransactions (tx:txs)
  = printf "- %s%s"
    ( printTransaction tx )
    ( intercalate "\n"
      . map snd
      . scanl
        ( \(total, result) tx ->
            let total' = total + txAmount tx
            in
              ( total'
              , printf "- %s, total: %s"
                (printTransaction tx)
                (show total')
              )
        )
        (txAmount tx, "")
      $ txs
    )

printAccountPayed :: Account -> [Transaction] -> String
printAccountPayed acc txs
  = printf "%s payed %s\n\n%s"
    (printAccount acc)
    (show . sum . map txAmount $ txs)
    (printTransactions txs)

printAccountWasPayed :: Account -> [Transaction] -> String
printAccountWasPayed acc txs
  = printf "%s %s %s\n\n%s"
    (printAccount acc)
    (verbForm acc "pay" Past Passive Affirmative)
    (show . sum . map txAmount $ txs)
    (printTransactions txs)

printAccountReport0
  :: Account -> [Transaction] -> [Transaction] -> [Transaction] -> String
printAccountReport0 acc [tx] [] txsWasPayed
  = printf "%s\n\n%s"
    (printAccountStatus acc [tx])
    (printTransactions txsWasPayed)
printAccountReport0 acc txsOwesTo [] txsWasPayed
  = printf "%s\n\n%s"
    (printAccountStatus acc txsOwesTo)
    (printAccountWasPayed acc txsWasPayed)
printAccountReport0 acc txsOwesTo txsPayed txsWasPayed
  = printf "%s\n\n%s\n\n%s"
    (printAccountStatus acc txsOwesTo)
    (printAccountPayed acc txsPayed)
    (printAccountWasPayed acc txsWasPayed)

printAccountReport (txNew, txOld) acc 
  = printAccountReport0 acc
    (accountTransactions  acc txNew)
    (debitAccountTransactions  acc txOld)
    (creditAccountTransactions acc txOld)

-- | Prints the full report
--
-- One report per each account.
printReport txsNewOld actions
  = intercalate divider reports
  where
    reports = map (printAccountReport txsNewOld) (actionsAccounts actions)
    maxLen
      = maximum
        . map (\report -> maximum . map length . lines $ report)
        $ reports
    divider = "\n\n" ++ (take maxLen $ cycle "-") ++ "\n\n"

users1 = ["Serge", "Sasha", "Pasha", "Ilya", "Tasha", "Kolya", "Alena", "Dima"]

actions1
  = Actions users1 [["Dima", "Alena"], ["Sasha", "Pasha"]]
    [ PurchaseAction (Purchase "Serge" "Pizza"
                      100.25 SplitEquallyAll)
    , PurchaseAction (Purchase "Serge" "Salad"
                      14.05 (SplitEqually ["Ilya"]))
    , PurchaseAction (Purchase "Dima"  "Cheese and wine"
                      21.64  SplitEquallyAll)
    , PurchaseAction (Purchase "Ilya"  "Berries"
                      14     SplitEquallyAll)
    , PurchaseAction
      (Purchase "Ilya" "Glasses" 24
       (SplitEqually ["Ilya", "Kolya", "Alena", "Dima", "Tasha"]))
    ]

t1
  = groupOn transactionAccounts
  . sortOn transactionAccounts
  . actionsToTransactions
  $ actions1

nullify1 = nullifyBalances . actionsToTransactions $ actions1
printNullify1 :: IO ()
printNullify1 = pPrint nullify1

printSergeReport1
  = putStrLn . printAccountReport nullify1 $ UserAccount "Serge"
printSashaReport1
  = putStrLn . printAccountReport nullify1 $ GroupAccount ["Sasha", "Pasha"]
printIlyaReport1
  = putStrLn . printAccountReport nullify1 $ UserAccount "Ilya"
printTashaReport1
  = putStrLn . printAccountReport nullify1 $ UserAccount "Tasha"
printKolyaReport1
  = putStrLn . printAccountReport nullify1 $ UserAccount "Kolya"
printDimaReport1
  = putStrLn . printAccountReport nullify1 $ GroupAccount ["Dima", "Alena"]

users2 = ["Tasha", "Ilya", "Alena", "Niki", "Dmitry", "Serge"]
actions2
  = Actions users2 [["Dmitry", "Alena"]]
    [ PurchaseAction (Purchase "Tasha" "Printing faces"
                      9     SplitEquallyAll)
    , PurchaseAction (Purchase "Ilya"  "Keyboard"
                      140   SplitEquallyAll)
    , PurchaseAction (Purchase "Alena" "Baking hardware"
                      9.6   SplitEquallyAll)
    , PurchaseAction (Purchase "Alena" "Baking groceries"
                      13.75 SplitEquallyAll)
    ]

nullify2 = nullifyBalances . actionsToTransactions $ actions2
printNullify2 :: IO ()
printNullify2 = pPrint nullify2

printTashaReport2
  = putStrLn . printAccountReport nullify2 $ UserAccount "Tasha"
printIlyaReport2
  = putStrLn . printAccountReport nullify2 $ UserAccount "Ilya"
printAlenaReport2
  = putStrLn . printAccountReport nullify2 $ (GroupAccount ["Dmitry", "Alena"])
printNikiReport2
  = putStrLn . printAccountReport nullify2 $ (UserAccount "Niki")
printSergeReport2
  = putStrLn . printAccountReport nullify2 $ (UserAccount "Serge")

users3 = ["Tasha", "Ilya", "Alena", "Dima", "Aigiza"]
actions3
  = Actions users3 [["Dima", "Alena"], ["Tasha", "Ilya"]]
    [ -- USD/EUR = 0.9211 as of May 25th, 478.40*0.9211 = 440.65
      -- But Alena thinks it's 442: 176.8*5/2
      PurchaseAction
      ( Purchase "Ilya" "AirBnb" 442 SplitEquallyAll )
    , PurchaseAction (Purchase "Ilya" "Pingo Doce in Óbidos"
                      178.47   SplitEquallyAll)
    , PurchaseAction (Purchase "Ilya" "Gasoline"
                      58.83    SplitEquallyAll)
    , PurchaseAction (Purchase "Ilya" "Road tolls"
                      (2*13.8) SplitEquallyAll)
    , PurchaseAction (Purchase "Ilya" "Pingo Doce in Coimbra"
                      41.86    SplitEquallyAll)
    , PurchaseAction
      ( Purchase "Dima" "Padaria Flor de Aveiro"
        19.45
        ( SplitEqually ["Dima", "Alena", "Tasha", "Ilya"] )
      )
    , PurchaseAction (Purchase "Ilya" "Cafe Papa in Coimbra"
                      77.5     SplitEquallyAll)
    , PurchaseAction (Purchase "Aigiza" "Cafe Trazarte in Óbidos"
                      65.4     SplitEquallyAll)
    , PurchaseAction (Purchase "Dima" "Large Ginjinha"
                      17       SplitEquallyAll)
    , PurchaseAction (Purchase "Dima" "Two small Ginjinhas"
                      8       (SplitEqually ["Aigiza"]))
    , PaymentAction
      ( Transaction
        ( GroupAccount ["Dima", "Alena"] )
        ( GroupAccount ["Tasha", "Ilya"] )
        (200 - 12.44)
        TxReasonPayment
      )
    ]

nullify3 = nullifyBalances . actionsToTransactions $ actions3

printNullify3 :: IO ()
printNullify3 = pPrint nullify3

printAigizaReport3
  = putStrLn . printAccountReport nullify3 $ UserAccount "Aigiza"

printDimaReport3
  = putStrLn . printAccountReport nullify3 $ GroupAccount ["Dima", "Alena"]

printIlyaReport3
  = putStrLn . printAccountReport nullify3 $ GroupAccount ["Tasha", "Ilya"]

printReport3 = putStrLn $ printReport nullify3 actions3