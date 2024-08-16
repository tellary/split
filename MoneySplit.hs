{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module MoneySplit where

import Data.Char          (toUpper)
import Data.Decimal       (Decimal)
import Data.List          (find, group, intercalate, nub, sort, sortBy, sortOn)
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

findUserNotInTheSameGroup :: [User] -> [Group] -> User -> Maybe User
findUserNotInTheSameGroup users groups currentUser
  = case group of
      Nothing -> find (/= currentUser) users
      Just group
        -> find (\user -> (lookup user groupsByUsersVal) /= (Just group)) users
  where
    groupsByUsersVal = groupsByUsers groups
    group = lookup currentUser groupsByUsersVal

currentGroup groups user
  = lookup user groupsByUsersVal
  where
    groupsByUsersVal = groupsByUsers groups

currentGroupOrUser groups user = maybe [user] id $ currentGroup groups user

accountUsers (UserAccount user  ) = [user]
accountUsers (GroupAccount group) = group

round2 :: Decimal -> Decimal
round2 d = fromIntegral (round (d*100) :: Integer) / 100

divAmounts :: Decimal -> [Decimal] -> [Decimal]
divAmounts d weights = reverse $ loop d weights []
  where
    n :: Decimal
    n = sum weights
    part :: Decimal
    part = round2 (d/n)
    loop :: Decimal -> [Decimal] -> [Decimal] -> [Decimal]
    loop _    []  _      = error "Recursion must end at [_]"
    loop left [_] result = left:result
    loop left (w:ws) result
      = loop (left - weightedPart) ws (weightedPart:result)
      where weightedPart = round2 $ w * part

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
transactionsAccounts accF = map head . group . sort . concat .  map accF
transactionsAllAccounts = transactionsAccounts transactionAccountsDirectionalArr
transactionsDebitAccounts = transactionsAccounts (pure . txDebitAccount)
transactionsCreditAccounts = transactionsAccounts (pure . txCreditAccount)

twoAccounts :: [Transaction] -> (Account, Account)
twoAccounts txs
  | length accs == 2 = (head accs, head . tail $ accs)
  | otherwise = error $ "Only two accounts expected, but found " ++ show accs
  where accs = transactionsAllAccounts txs

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

data SplitItem
  = SplitItem
  { splitItemTo     :: SplitTo
  , splitItemDesc   :: Desc
  , splitItemAmount :: Amount
  } deriving (Show, Eq, Ord)

data Purchase
  = Purchase
  { purchaseUser   :: User
  , purchaseDesc   :: Desc
  , purchaseAmount :: Amount
  , purchaseSplit  :: Split
  } deriving (Show, Eq, Ord)

data SplitTo = SplitToUser User | SplitToGroup Group
  deriving (Eq, Ord, Show)

splitToToUsers (SplitToUser  user ) = [user]
splitToToUsers (SplitToGroup group) = group

splitToToAccount groupsByUsersVal (SplitToUser user)
  = userToAccount groupsByUsersVal user
splitToToAccount groupsByUsersVal (SplitToGroup (user:_))
  = userToAccount groupsByUsersVal user
splitToToAccount _                (SplitToGroup [])
  = error "'SplitToGroup' can't be empty"

data SplitTipsItem
  = SplitTipsItem
  { splitTipsTo :: SplitTo
  , splitTipsAmount :: Amount
  } deriving (Show, Eq, Ord)

data SplitTips
  = SplitTipsEqually [SplitTo]
  | SplitTipsEquallyAll
  | ItemizedSplitTips [SplitTipsItem]
  | RelativeSplitTips
  deriving (Show, Eq, Ord)

data Tips = Tips Int SplitTips deriving (Show, Eq, Ord)

data Split
  = SplitEqually [SplitTo]
  | SplitEquallyAll
  -- TODO: Validate ItemizedSplit sum == purchaseAmount
  | ItemizedSplit (Maybe Tips) [SplitItem]
  deriving (Show, Eq, Ord)

isUserPurchase user = (== user) . purchaseUser

maybeSplitItems (ItemizedSplit _ items) = Just items
maybeSplitItems  _                    = Nothing

splitItemsForAccount :: [SplitItem] -> Account -> [SplitItem]
splitItemsForAccount itemizedSplits acc
  = filter (\item -> (head . splitToToUsers . splitItemTo $ item) `elem` accountUsers acc) itemizedSplits

splitTosUsers = nub . concat . map (splitToToUsers)
splitItemsUsers = splitTosUsers . map (splitItemTo)

splitItemUsers = splitToToUsers . splitItemTo

splitUsers actions  SplitEquallyAll      = actionsUsers actions
splitUsers _       (SplitEqually splitTos ) = splitTosUsers splitTos
splitUsers _       (ItemizedSplit _ items) = splitItemsUsers $ items

purchaseSplitUsers actions (Purchase { purchaseSplit = split })
  = splitUsers actions split

data Action
  = PurchaseAction Purchase | PaymentAction User User Amount
  deriving (Eq, Show)

maybePurchase (PurchaseAction purchase) = Just purchase
maybePurchase _ = Nothing

isUserAction user (PaymentAction u1 u2 _) = user == u1 || user == u2
isUserAction user (PurchaseAction p) = isUserPurchase user p

data Actions
  = Actions
  { actionsUsers :: [User]
  , actionsGroups :: [Group]
  , actionsArr :: [Action]
  } deriving Show

splitTosToAccounts :: Actions -> [SplitTo] -> [Account]
splitTosToAccounts actions
  = usersToAccounts actions . map (head . splitToToUsers)

actionsAccounts :: Actions -> [Account]
actionsAccounts actions@(Actions users _ _) = usersToAccounts actions users

usersToAccounts :: Actions -> [User] -> [Account]
usersToAccounts (Actions _ groups _) users
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

tipsAmount tips
  = round2
    . (((fromIntegral tips)/100) *)
    . sum
    . map splitItemAmount

addTips :: [User] -> [Group] -> Maybe Tips -> [SplitItem] -> [SplitItem]
addTips users groups tips items = items ++ tipItems users groups tips items

tipItems :: [User] -> [Group] -> Maybe Tips -> [SplitItem] -> [SplitItem]
tipItems _ _  Nothing _ = []
tipItems _ _ (Just (Tips tips (SplitTipsEqually [splitTo]))) items
  = [SplitItem splitTo ( printf "%d%% tips" tips ) (tipsAmount tips items)]
tipItems _ _ (Just (Tips tips (SplitTipsEqually splitTos))) items
  = map
    (\(splitTo, tipAmount)
       -> SplitItem
          splitTo
          ( printf "%d%% tips shared equally to %d people"
            tips (length splitTos)
          )
          tipAmount
    ) $ zip splitTos tipsPerSplitTo
  where
    tipsPerSplitTo
      = divAmounts
        (tipsAmount tips items)
        (map (fromIntegral . length . splitToToUsers) splitTos)
tipItems users groups (Just (Tips tips SplitTipsEquallyAll)) items
  = tipItems
    users
    groups
    (Just (Tips tips (SplitTipsEqually (map SplitToUser users))))
    items
tipItems _ _ (Just (Tips tips (ItemizedSplitTips tipItems))) _
  = map toItem tipItems
  where
    toItem tipItem
      = SplitItem
        ( splitTipsTo tipItem )
        ( printf "%d%% tips split exactly" tips )
        ( splitTipsAmount tipItem )
tipItems _ groups (Just (Tips tips RelativeSplitTips)) items
  = map
    (\(splitTo, tipAmount)
       -> SplitItem
          splitTo
          ( printf "%d%% tips" tips )
          tipAmount
    ) $ zip splitTos tipsPerSplitTo
  where
    (splitTos, weights) = unzip . sumSplitAmountByTo groups $ items
    tipsPerSplitTo
      = divAmounts
        (tipsAmount tips items)
        weights

replaceUsersWithGroupsInSplitTos groups splitItems
  = map maybeReplaceTo splitItems
  where
    maybeReplaceTo item@(SplitItem (SplitToUser user) _ _)
      = case lookup user groupsByUsersVal of
          Nothing -> item
          Just group -> item { splitItemTo = SplitToGroup group }
    maybeReplaceTo item@(SplitItem (SplitToGroup _) _ _) = item
    groupsByUsersVal = groupsByUsers groups

-- | Sums all split item amounts by 'SplitTo'
sumSplitAmountByTo :: [Group] -> [SplitItem] -> [(SplitTo, Decimal)]
sumSplitAmountByTo groups
  = map (\grp -> (splitItemTo . head $ grp, sum . map splitItemAmount $ grp))
  . groupOn splitItemTo
  . sortOn splitItemTo
  . replaceUsersWithGroupsInSplitTos groups

toTransactions :: Actions -> Action -> [Transaction]
toTransactions (Actions _ groups _) (PurchaseAction
                  purchase@(Purchase debitUser _ amount (SplitEqually splitTos)))
  = collapseSameAccounts
  . filter (\tx -> txDebitAccount tx /= txCreditAccount tx)
  . map (
      \(splitTo, amount) ->
        Transaction
        (userToAccount groupsByUsersVal debitUser)
        (splitToToAccount groupsByUsersVal splitTo)
        amount
        (TxReasonPurchase purchase)
    )
  $ zip
    splitTos
    ( divAmounts
      amount
      ( map (fromIntegral . length . splitToToUsers) splitTos )
    )
  where groupsByUsersVal = groupsByUsers groups
toTransactions
  actions@(Actions users _ _)
  (PurchaseAction
    (Purchase debitUser desc amount SplitEquallyAll))
  = toTransactions actions
    (PurchaseAction
     (Purchase debitUser desc amount (SplitEqually (map SplitToUser users))))
toTransactions
  (Actions users groups _)
  (PurchaseAction
    purchase@(Purchase debitUser _ _ (ItemizedSplit tips splitItems)))
  = filter (\tx -> txDebitAccount tx /= txCreditAccount tx)
    . map userSplitItemsToTx
    . map (\grp -> (fst . head $ grp, map snd grp))
    . groupOn fst
    . map (\item -> ( splitToToAccount groupsByUsersVal . splitItemTo $ item
                    , item
                    )
          )
    $ addTips users groups tips splitItems
  where
    userSplitItemsToTx :: (Account, [SplitItem]) -> Transaction
    userSplitItemsToTx (creditAccount, userSplitItems)
      = Transaction
        (userToAccount groupsByUsersVal debitUser)
        creditAccount
        (sum . map splitItemAmount $ userSplitItems)
        (TxReasonPurchase purchase)
    groupsByUsersVal = groupsByUsers groups
toTransactions
  (Actions _ groups _)
  (PaymentAction debitUser creditUser amount)
  = [ Transaction
      (userToAccount groupsByUsersVal debitUser)
      (userToAccount groupsByUsersVal creditUser)
      amount
      TxReasonPayment
    ]
  where
    groupsByUsersVal = groupsByUsers groups

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
    accs = transactionsAllAccounts txs

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

groupTransactionsByReason :: [Transaction] -> [(TxReason, [Transaction])]
groupTransactionsByReason
  = map (\grp -> (txReason . head $ grp, grp)) . groupOn txReason

capitaize [] = []
capitaize (c:cs) = toUpper c:cs

printAccountBalance acc txs
  = printf "%s %s %s"
    ( printAccount acc )
    ( verbForm acc "owe" Present voice Affirmative )
    ( show . abs $ b )
  where
    b     = balance acc txs
    voice = if b < 0 then Active else Passive

printSummaryBySingleReason
  :: Actions -> Account -> (TxReason, [Transaction]) -> Maybe String
printSummaryBySingleReason actions  acc reasonGroup
  | b > 0
  = Just $ printSummaryBySingleReasonWasPayed actions acc b reasonGroup
  | b < 0
  = Just $ printSummaryBySingleReasonPayed    actions acc b reasonGroup
  | otherwise = Nothing
  where
    txs = snd reasonGroup
    b = balance acc txs
-- printSummaryBySingleReason actions3 (GroupAccount ["Tasha","Ilya"]) (TxReasonPayment,[Transaction {txDebitAccount = GroupAccount ["Dima","Alena"], txCreditAccount = GroupAccount ["Tasha","Ilya"], txAmount = 187.56, txReason = TxReasonPayment}])
-- printSummaryBySingleReason actions3 (GroupAccount ["Dima","Alena"]) (TxReasonPayment,[Transaction {txDebitAccount = GroupAccount ["Dima","Alena"], txCreditAccount = GroupAccount ["Tasha","Ilya"], txAmount = 187.56, txReason = TxReasonPayment}])
-- printSummaryBySingleReason actions3 (GroupAccount ["Dima","Alena"]) (TxReasonPurchase (Purchase "Dima" "Large Ginjinha" 17 (SplitEqually ["Tasha","Ilya","Alena","Dima","Aigiza"])),[Transaction {txDebitAccount = GroupAccount ["Dima","Alena"], txCreditAccount = UserAccount "Aigiza", txAmount = 3.4, txReason = TxReasonPurchase (Purchase "Dima" "Large Ginjinha" 17 (SplitEqually ["Tasha","Ilya","Alena","Dima","Aigiza"]))},Transaction {txDebitAccount = GroupAccount ["Dima","Alena"], txCreditAccount = GroupAccount ["Tasha","Ilya"], txAmount = 6.8, txReason = TxReasonPurchase (Purchase "Dima" "Large Ginjinha" 17 (SplitEqually ["Tasha","Ilya","Alena","Dima","Aigiza"]))}])
-- printSummaryBySingleReason actions3 (UserAccount "Aigiza") (TxReasonPurchase (Purchase "Dima" "Large Ginjinha" 17 (SplitEqually ["Tasha","Ilya","Alena","Dima","Aigiza"])),[Transaction {txDebitAccount = GroupAccount ["Dima","Alena"], txCreditAccount = UserAccount "Aigiza", txAmount = 3.4, txReason = TxReasonPurchase (Purchase "Dima" "Large Ginjinha" 17 (SplitEqually ["Tasha","Ilya","Alena","Dima","Aigiza"]))},Transaction {txDebitAccount = GroupAccount ["Dima","Alena"], txCreditAccount = GroupAccount ["Tasha","Ilya"], txAmount = 6.8, txReason = TxReasonPurchase (Purchase "Dima" "Large Ginjinha" 17 (SplitEqually ["Tasha","Ilya","Alena","Dima","Aigiza"]))}])
-- printSummaryBySingleReason actions1 (UserAccount "Ilya") (TxReasonPurchase (Purchase {purchaseUser = "Serge", purchaseDesc = "Salad", purchaseAmount = 14.05, purchaseSplit = SplitEqually ["Ilya"]}),[Transaction {txDebitAccount = UserAccount "Serge", txCreditAccount = UserAccount "Ilya", txAmount = 14.05, txReason = TxReasonPurchase (Purchase {purchaseUser = "Serge", purchaseDesc = "Salad", purchaseAmount = 14.05, purchaseSplit = SplitEqually ["Ilya"]})}])

printSummaryBySingleReasonWasPayed _ acc balance (TxReasonPayment, txs)
  = printf "%s %s by %s"
    ( verbForm acc "pay" Past Passive Affirmative )
    ( show balance )
    ( printAccountList $ transactionsDebitAccounts txs )
printSummaryBySingleReasonWasPayed
      actions acc balance (TxReasonPurchase purchase, txs)
  = case purchaseSplitUsers actions purchase of
      [_] -> printf "%s %s for \"%s\" by %s"
             ( verbForm acc "pay" Past Passive Affirmative )
             ( show $ purchaseAmount purchase )
             ( purchaseDesc purchase )
             ( printAccountList (transactionsDebitAccounts txs) )
      (_) -> printf "%s %s out of %s for \"%s\" by %s"
             ( verbForm acc "pay" Past Passive Affirmative )
             ( show balance )
             ( show $ purchaseAmount purchase )
             ( purchaseDesc purchase )
             ( printAccountList (transactionsDebitAccounts txs) )

printSummaryBySingleReasonPayed _ acc balance (TxReasonPayment, txs)
  = printf "%s %s to %s"
    ( verbForm acc "pay" Past Active Affirmative )
    ( show . abs $ balance )
    ( printAccountList $ transactionsCreditAccounts txs )
printSummaryBySingleReasonPayed
      actions acc balance (TxReasonPurchase purchase, txs)
  = case purchaseSplitUsers actions purchase of
      [_] -> printf "%s %s for \"%s\" for %s"
             ( verbForm acc "pay" Past Active Affirmative )
             ( show $ purchaseAmount purchase )
             ( purchaseDesc purchase )
             ( printAccountList (transactionsCreditAccounts txs) )
      (_) -> printf "%s %s out of %s for \"%s\" for %s"
             ( verbForm acc "pay" Past Active Affirmative )
             ( show . abs $ balance )
             ( show $ purchaseAmount purchase )
             ( purchaseDesc purchase )
             ( printAccountList (transactionsCreditAccounts txs) )

decreaseBalance balances
  = case (asc, desc) of
      ((lowest, lowestAcc):_, (highest, highestAcc):_)
        | abs lowest >= highest
          -> Transaction highestAcc lowestAcc  highest     TxReasonPayment
        | otherwise
          -> Transaction highestAcc lowestAcc (abs lowest) TxReasonPayment
      _ -> error "'balances' should never be '[]' in 'decreaseBalance'"
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

printAccount :: Account -> String
printAccount (UserAccount user) = user
printAccount (GroupAccount users) = printUsersList users

printUsersList :: [User] -> String
printUsersList users
  = case users of
      [user] -> user
      users  -> printf "%s and %s"
                (intercalate ", " $ init users)
                (last users)

printAccountList :: [Account] -> String
printAccountList accs
  = printUsersList users
  where
    users :: [String] = concat . map accountUsers $ accs
-- printAccountList [(UserAccount "Aigiza"), (GroupAccount ["Dima", "Alena"])]
-- printAccountList [(UserAccount "Aigiza")]
-- printAccountList [(GroupAccount ["Dima", "Alena"])]

data GramiticTime = Present | Past deriving Show
data Voice = Active | Passive deriving Show
data Negation = Affirmative | Negative deriving Show

verbForm  acc             "owe" tense   Passive Affirmative
  = verbForm acc "ow" tense Passive Affirmative
verbForm (UserAccount _)  "do"  _       Active  Negative = "doesn't"
verbForm (GroupAccount _) "do"  _       Active  Negative = "don't"
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
verbForm (UserAccount _)  verb  Past    Active  Affirmative
  = verb ++ "ed"
verbForm (GroupAccount _) verb  Past    Active  Affirmative
  = verb ++ "ed"
verbForm (UserAccount _)  verb  Past    Passive Affirmative
  = "was " ++ verb ++ "ed"
verbForm (GroupAccount _) verb  Past    Passive Affirmative
  = "were " ++ verb ++ "ed"
verbForm _ verb tense active affirmative
  = error
    $ printf "Verb form is not defined for verb '%s' in %s, %s, %s"
      verb (show tense) (show active) (show affirmative)

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
printAmount
      ( Transaction
        { txAmount = amount
        , txReason = TxReasonPayment
        }
      )
  = show amount

printTransaction :: Transaction -> String
printTransaction
      tx@( Transaction
           { txReason = TxReasonPurchase
                        (Purchase _ purchaseDesc _ _)
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
        ( \(total, _) tx ->
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
printTransactions []
  = error "'printTransactions' shouldn't be called with '[]'"

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
printAccountReport0 acc _ [] []
  = printf "%s didn't make any transactions"
    (printAccount acc)
printAccountReport0 acc txsOwesTo@[_] [] txsWasPayed
  = printf "%s\n\n%s"
    (printAccountStatus acc txsOwesTo)
    (printTransactions txsWasPayed)
printAccountReport0 acc txsOwesTo@[_] txsPayed []
  = printf "%s\n\n%s"
    (printAccountStatus acc txsOwesTo)
    (printTransactions txsPayed)
printAccountReport0 acc txsOwesTo [] txsWasPayed
  = printf "%s\n\n%s"
    (printAccountStatus acc txsOwesTo)
    (printAccountWasPayed acc txsWasPayed)
printAccountReport0 acc txsOwesTo txsPayed []
  = printf "%s\n\n%s"
    (printAccountStatus acc txsOwesTo)
    (printAccountWasPayed acc txsPayed)
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
                      14.05 (SplitEqually [SplitToUser "Ilya"]))
    , PurchaseAction (Purchase "Dima"  "Cheese and wine"
                      21.64  SplitEquallyAll)
    , PurchaseAction (Purchase "Ilya"  "Berries"
                      14     SplitEquallyAll)
    , PurchaseAction
      ( Purchase "Ilya" "Glasses" 24
        ( SplitEqually
          [ SplitToUser "Ilya"
          , SplitToUser "Kolya"
          , SplitToUser "Alena"
          , SplitToUser "Dima"
          , SplitToUser "Tasha"
          ]
        )
      )
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
        ( SplitEqually
          [ SplitToUser "Dima"
          , SplitToUser "Alena"
          , SplitToGroup ["Tasha", "Ilya"]
          ]
        )
      )
    , PurchaseAction (Purchase "Ilya" "Cafe Papa in Coimbra"
                      77.5     SplitEquallyAll)
    , PurchaseAction (Purchase "Aigiza" "Cafe Trazarte in Óbidos"
                      65.4     SplitEquallyAll)
    , PurchaseAction (Purchase "Dima" "Large Ginjinha"
                      17       SplitEquallyAll)
    , PurchaseAction (Purchase "Dima" "Two small Ginjinhas"
                      8       (SplitEqually [SplitToUser "Aigiza"]))
    , PaymentAction "Dima" "Ilya" (200 - 12.44)
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

users4 = ["Tasha", "Ilya", "Alena", "Dima"]
berriesSplit4
  = ItemizedSplit Nothing
    [ SplitItem (SplitToUser "Tasha") "1kg blackberry" 6
    , SplitItem (SplitToUser "Tasha") "1kg strawberry" 4
    , SplitItem (SplitToUser "Tasha") "1kg blueberry"  9
    , SplitItem (SplitToUser "Ilya" ) "1kg blackberry" 6
    , SplitItem (SplitToUser "Ilya" )  "1kg strawberry" 4
    , SplitItem (SplitToUser "Ilya" )  "1kg blueberry"  9
    , SplitItem (SplitToUser "Ilya" )  "1kg raspberry"  6
    , SplitItem (SplitToGroup ["Dima", "Alena"]) "1kg blackberry" 6
    , SplitItem (SplitToGroup ["Dima", "Alena"]) "1kg strawberry" 4
    , SplitItem (SplitToGroup ["Dima", "Alena"]) "1kg blueberry"  9
    ]

actions4
  = Actions users4 [["Dima", "Alena"], ["Tasha", "Ilya"]]
    [ PurchaseAction (Purchase "Ilya" "PILS" 181 SplitEquallyAll )
    , PurchaseAction
      ( Purchase "Ilya" "Berries" 63 berriesSplit4 )
    , PurchaseAction
      ( Purchase "Dima" "Andrey Sakhov, MB Way on 15-07-2024" 140
        (SplitEqually [SplitToGroup ["Tasha", "Ilya"]])
      )
    , PurchaseAction
      ( Purchase "Dima" "W Padel, MB Way on 04-07-2024" 31
        (SplitEqually [SplitToUser "Ilya"])
      )
    , PurchaseAction
      ( Purchase "Alena" "Berries in June" 9.5
        ( ItemizedSplit Nothing
          [ SplitItem (SplitToUser "Ilya") "1kg strawberry" 3.5
          , SplitItem (SplitToUser "Ilya") "1kg raspberry" 6
          ]
        )
      )
    ]

nullify4 = nullifyBalances . actionsToTransactions $ actions4

printNullify4 :: IO ()
printNullify4 = pPrint nullify4

printReport4 = putStrLn $ printReport nullify4 actions4

actions5
  = Actions
    ["Dima", "Alena F.", "Tasha", "Ilya", "Vlad", "Alena L.", "Unknown"]
    [["Dima", "Alena F."], ["Tasha", "Ilya"], ["Vlad", "Alena L."]]
    [ PurchaseAction
      ( Purchase "Ilya" "Oysters & Margarita" 194.7
        ( ItemizedSplit (Just (Tips 10 RelativeSplitTips))
          [ SplitItem
            ( SplitToGroup ["Tasha", "Ilya"] )
            "2x White Sauvignon"
            12
          , SplitItem
            ( SplitToGroup ["Tasha", "Ilya"] )
            "1x Bruschetta"
            (15/2)
          , SplitItem
            ( SplitToGroup ["Dima", "Alena F."] )
            "1x Bruschetta"
            (15/2)
          , SplitItem
            ( SplitToGroup ["Tasha", "Ilya"] )
            "1x Oysters 12 un"
            28.5
          , SplitItem
            ( SplitToGroup ["Dima", "Alena F."] )
            "1x BRLO cerveja"
            5
          , SplitItem
            ( SplitToGroup ["Tasha", "Ilya"] )
            "1x Dorado warm"
            (38/2)
          , SplitItem
            ( SplitToGroup ["Vlad", "Alena L."] )
            "1x Dorado warm"
            (38/2)
          , SplitItem
            ( SplitToGroup ["Vlad", "Alena L."] )
            "1x Verde Paterna"
            (22/4)
          , SplitItem
            ( SplitToGroup ["Dima", "Alena F."] )
            "3x Verde Paterna"
            (22*3/4)
          , SplitItem
            ( SplitToGroup ["Dima", "Alena F."] )
            "1x Ice cream"
            5
          , SplitItem
            ( SplitToGroup ["Vlad", "Alena L."] )
            "1x Tuna tataki N"
            13.5
          , SplitItem
            ( SplitToGroup ["Dima", "Alena F."] )
            "1x Rose da Casa"
            5
          , SplitItem
            ( SplitToGroup ["Tasha", "Ilya"] )
            "1x Burrata"
            9.5
          , SplitItem
            ( SplitToGroup ["Tasha", "Ilya"] )
            "1x Margarita Mez"
            11.5
          , SplitItem
            ( SplitToGroup ["Tasha", "Ilya"] )
            "1x Sexy Pear"
            6.5
          , SplitItem
            ( SplitToGroup ["Tasha", "Ilya"] )
            "1x Sparkling RS"
            5.5
          ]
        )
      )
    ]
-- λ> sum . map splitItemAmount . fromJust . maybeSplitItems . purchaseSplit . head . fromJust . sequence $ maybePurchase <$> actionsArr actions5
-- 177.0

nullify5 = nullifyBalances . actionsToTransactions $ actions5

printNullify5 :: IO ()
printNullify5 = pPrint nullify5

printReport5 = putStrLn $ printReport nullify5 actions5

actions6
  = Actions
  { actionsUsers
    = ["Dima", "Alena F.", "Tasha", "Ilya", "Vlad", "Alena L.", "Asya"]
  , actionsGroups
    = [["Dima", "Alena F."], ["Tasha", "Ilya"], ["Vlad", "Alena L."]]
  , actionsArr =
      actionsArr actions5 ++
      [ PurchaseAction
        ( Purchase "Dima" "Crouton" (1.1*172.5)
          ( ItemizedSplit (Just (Tips 10 RelativeSplitTips))
            [ SplitItem
              ( SplitToGroup ["Dima", "Alena F."] )
              "2x Aqua das Pedras"
              (10/2)
            , SplitItem
              ( SplitToGroup ["Tasha", "Ilya"] )
              "2x Aqua das Pedras"
              (10/2)
            , SplitItem
              ( SplitToGroup ["Dima", "Alena F."] )
              "3x Cerveja SG"
              13.5
            , SplitItem
              ( SplitToGroup ["Tasha", "Ilya"] )
              "3x Sparkling Brut"
              27
            , SplitItem
              ( SplitToGroup ["Dima", "Alena F."] )
              "1x Vinho a Copo"
              6
            , SplitItem
              ( SplitToGroup ["Dima", "Alena F."] )
              "1x Carpaccio Atum"
              18
            , SplitItem
              ( SplitToGroup ["Dima", "Alena F."] )
              "1x Pizza Especial"
              (72/3)
            , SplitItem
              ( SplitToGroup ["Tasha", "Ilya"] )
              "2x Pizza Especial"
              (72*2/3)
            , SplitItem
              ( SplitToUser "Asya" )
              "1x Refrigerante Bio"
              4.5
            , SplitItem
              ( SplitToUser "Asya" )
              "1x Sobremesa"
              (19/2)
            , SplitItem
              ( SplitToGroup ["Dima", "Alena F."] )
              "1x Sobremesa"
              (19/2)
            , SplitItem
              ( SplitToGroup ["Dima", "Alena F."] )
              "1x Cafe & Petit-four"
              2.5
            ]
          )
        )
      , PaymentAction "Vlad" "Ilya" 41.8
      ]
  }

nullify6 = nullifyBalances . actionsToTransactions $ actions6

printNullify6 :: IO ()
printNullify6 = pPrint nullify6

printReport6 = putStrLn $ printReport nullify5 actions5
