{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module StatelessValidDynamic where

import Control.Monad              (join)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Reflex.Dom

type ValidDynamic t err a = ExceptT err (Dynamic t) a

fromDynamic :: Reflex t => Dynamic t a -> (a -> Either err b) -> ValidDynamic t err b
fromDynamic d validation = ExceptT . fmap validation $ d

fromEvent
  :: (Reflex t, MonadHold t m)
  => a -> Event t a -> (a -> Either err b) -> m (ValidDynamic t err b)
fromEvent initVal ev validation = do
  d <- holdDyn initVal ev
  return $ fromDynamic d validation

fromValue :: Reflex t => a -> (a -> Either err b) -> ValidDynamic t err b
fromValue a validation = ExceptT . fmap validation . pure $ a

assumeValidValue :: Reflex t => a -> ValidDynamic t err a
assumeValidValue = ExceptT . fmap Right . pure

maybeValidValue = either (const Nothing) Just

tagValid :: Reflex t => ValidDynamic t err a -> Event t b -> Event t a
tagValid (ExceptT validInput) submitEvent
  = mapMaybe maybeValidValue
  . (tag . current) validInput
  $ submitEvent

errorDyn
  :: (Show a, Show err, Show b, Reflex t, MonadHold t m)
  => err -> Event t a -> ValidDynamic t err b -> m (Dynamic t err)
errorDyn noErrorVal submitEvent (ExceptT validDynamic) = do
  let error = fmap (either id (const noErrorVal)) validDynamic
  holdDyn noErrorVal
    ( tagPromptlyDyn error
      ( leftmost [() <$ submitEvent, () <$ updated validDynamic] )
    )
  
unwrapValidDynamicWidget :: (DomBuilder t m, MonadHold t m, PostBuild t m)
  => a -> Dynamic t (m (ValidDynamic t err a)) -> m (ValidDynamic t err a)
unwrapValidDynamicWidget initVal dynWidget
  = fmap (ExceptT . join . fmap runExceptT)
    (holdDyn (assumeValidValue initVal) =<< dyn dynWidget)

dropE :: (Reflex t, MonadHold t m) => Int -> Event t a -> m (Event t a)
dropE 0     e = return e
dropE count e = do
  skipped <- tailE e
  dropE (count - 1) skipped

dropValidDynamic
  :: (Reflex t, MonadHold t m)
  => Either err a -> Int -> ValidDynamic t err a -> m (ValidDynamic t err a)
dropValidDynamic initVal count d
  = fmap ExceptT (holdDyn initVal =<< (dropE count . updated . runExceptT $ d))
