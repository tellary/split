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

fromDynamic :: Reflex t => (a -> Either err b) -> Dynamic t a -> ValidDynamic t err b
fromDynamic validation = ExceptT . fmap validation 

fromValue :: Reflex t => (a -> Either err b) -> a -> ValidDynamic t err b
fromValue validation = ExceptT . fmap validation . pure

assumeValidValue :: Reflex t => a -> ValidDynamic t err a
assumeValidValue = ExceptT . fmap Right . pure

maybeValidValue = either (const Nothing) Just

tagValid :: Reflex t => ValidDynamic t err a -> Event t b -> Event t a
tagValid (ExceptT validInput) submitEvent
  = mapMaybe maybeValidValue
  . tagPromptlyDyn validInput
  $ submitEvent

errorDyn
  :: (Reflex t, MonadHold t m)
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

