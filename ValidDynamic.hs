{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module ValidDynamic where

import Control.Monad              (join)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Reflex.Dom

type ValidDynamic t err a = ExceptT err (Dynamic t) a

traceEnabled = False

traceEventIfEnabled :: (Show a, Reflex t) => String -> Event t a -> Event t a
traceEventIfEnabled = if traceEnabled then traceEvent else flip const

fromDynamic
  :: Reflex t
  => Dynamic t a -> (a -> Either err b) -> ValidDynamic t err b
fromDynamic d validation = ExceptT . fmap validation $ d

fromDynamicEither
  :: Reflex t
  => Dynamic t (Either err a) -> ValidDynamic t err a
fromDynamicEither d = ExceptT d

fromEvent
  :: (Reflex t, MonadHold t m)
  => a -> Event t a -> (a -> Either err b) -> m (ValidDynamic t err b)
fromEvent initVal ev validation = do
  d <- holdDyn initVal ev
  return $ fromDynamic d validation

fromValue :: Reflex t => a -> (a -> Either err b) -> ValidDynamic t err b
fromValue a validation = ExceptT . fmap validation . pure $ a

assumeValidDynamic :: Reflex t => Dynamic t a -> ValidDynamic t err a
assumeValidDynamic = ExceptT . fmap Right

assumeValidValue :: Reflex t => a -> ValidDynamic t err a
assumeValidValue = ExceptT . fmap Right . pure

maybeValidValue = either (const Nothing) Just

tagValid
  :: (Show a, Show err, Reflex t)
  => ValidDynamic t err a -> Event t b -> Event t a
tagValid (ExceptT validInput) submitEvent
  = mapMaybe maybeValidValue
  . traceEventIfEnabled "tagValid" . tagPromptlyDyn validInput
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

errorWidget
  :: forall t m a b c err . (Reflex t, Adjustable t m, MonadHold t m)
  => Event t b -> ValidDynamic t err a -> m c -> (err -> m c) -> m (Dynamic t c)
errorWidget
    submitEvent
    (ExceptT validDynamic)
    noErrorWidget errorWidget = do
  let error :: Dynamic t (m c) = fmap (either errorWidget (const noErrorWidget)) validDynamic
  widgetHold
    noErrorWidget
    ( tagPromptlyDyn error
       ( leftmost [() <$ submitEvent, () <$ updated validDynamic] )
    )

unwrapValidDynamicWidget :: (DomBuilder t m, MonadHold t m, PostBuild t m)
  => a -> Dynamic t (m (ValidDynamic t err a)) -> m (ValidDynamic t err a)
unwrapValidDynamicWidget initVal dynWidget
  = fmap (ExceptT . join . fmap runExceptT)
    (holdDyn (assumeValidValue initVal) =<< dyn dynWidget)

dynamicValidDynamic
  :: (Reflex t, Applicative m)
  => ValidDynamic t err (m (Dynamic t a))
  -> Dynamic t (m (ValidDynamic t err a))
dynamicValidDynamic dynWidget
  = ffor (runExceptT dynWidget) $ \case
      Right dynWidget -> fmap (ExceptT . fmap Right) dynWidget
      Left err -> pure . ExceptT . pure . Left $ err

unwrapValidDynamicDynamicWidget
  :: (DomBuilder t m, MonadHold t m, PostBuild t m)
  => a -> ValidDynamic t err (m (Dynamic t a)) -> m (ValidDynamic t err a)
unwrapValidDynamicDynamicWidget initVal dynWidget
  = unwrapValidDynamicWidget initVal . dynamicValidDynamic $ dynWidget
    
dropE :: (Reflex t, MonadHold t m) => Int -> Event t a -> m (Event t a)
dropE 0     e = return e
dropE count e = do
  skipped <- tailE e
  dropE (count - 1) skipped

dropValidDynamic
  :: (Reflex t, MonadHold t m)
  => Event t b -> Either err a
  -> Int -> ValidDynamic t err a
  -> m (ValidDynamic t err a)
dropValidDynamic submitEvent initVal count validDyn
  = fmap
    ExceptT
    ( holdDyn initVal =<< (
        dropE count
        . leftmost
        $ [ updated d
          , tagPromptlyDyn d submitEvent
          ]
        )
    )
  where d = runExceptT validDyn
