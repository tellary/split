{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module ValidDynamic where

import Control.Monad.Fix (MonadFix)
import Data.Either       (isRight)
import Data.Text         (Text)
import Debug.Trace       (trace)
import Reflex.Dom        (Dynamic, Event, MonadHold, Reflex, foldDyn, holdDyn,
                          leftmost, mapMaybe, tagPromptlyDyn, traceEvent,
                          updated)
import Text.Printf       (printf)

data ErrorState = Reset Int | Submitted Int | DataChanged deriving Show
data ValidValue a
  = ValidValue ErrorState (Either Text a)
  deriving Show

type ValidDynamic t a = Dynamic t (ValidValue a)

traceEnabled = True

traceEventIfEnabled :: (Show a, Reflex t) => String -> Event t a -> Event t a
traceEventIfEnabled = if traceEnabled then traceEvent else flip const

traceIfEnabled = if traceEnabled then trace else flip const

traceUpdate :: Show a => String -> a -> a -> a
traceUpdate tag old new
  = traceIfEnabled (printf "%s: %s -> %s" tag (show old) (show new)) new

onSubmit skipUpdateCount v@(ValidValue DataChanged e@(Right _))
  = traceUpdate "onSubmit" v $ ValidValue (Reset skipUpdateCount) e
onSubmit _ v@(ValidValue DataChanged (Left _))
  = traceUpdate "onSubmit" v $ v
onSubmit _ v@(ValidValue (Submitted skipUpdateCount) e@(Right _))
  = traceUpdate "onSubmit" v $ ValidValue (Reset skipUpdateCount) e
onSubmit _ v@(ValidValue (Submitted skipUpdateCount) e@(Left _))
  = traceUpdate "onSubmit" v $ ValidValue (Submitted skipUpdateCount) e
onSubmit skipUpdateCount v@(ValidValue (Reset _) e)
  = traceUpdate "onSubmit" v $ ValidValue (Submitted skipUpdateCount) e
  
onReset skipUpdateCount v
  = traceUpdate "onReset" v
    $ ValidValue (Reset skipUpdateCount) (validValueEither v)

onUpdate validation newVal v@(ValidValue DataChanged _)
  = traceUpdate "onUpdate" v $ ValidValue DataChanged (validation newVal)
onUpdate validation newVal v@(ValidValue (Submitted i) _)
  = traceUpdate "onUpdate" v
    $ if i == 0
      then ValidValue DataChanged (validation newVal)
      else ValidValue (Submitted (i - 1)) (validation newVal)
onUpdate validation newVal v@(ValidValue (Reset i) _)
  = traceUpdate "onUpdate" v
    $ if i == 0
      then ValidValue DataChanged (validation newVal)
      else ValidValue (Reset (i - 1)) (validation newVal)

-- | Create "valid dynamic" with stateful errors
--
-- The "valid dynamic" holds an a valid value if the `validation` function
-- returns `Right`. Or else, it holds an error. But the error is stateful,
-- it holds information if the input data is changed or a submit event was
-- sent. The related `errorDyn` function uses this information to avoid
-- showing errors before submit event occurs.
--
-- For example, given a form with certain input fields with empty inital values
-- and a submit button, we don't show an error that the field is empty before
-- we a submit event occurs.
--
-- 'submitEvent' and 'resetEvent' carry "skip update count". This counts tells
-- the 'validDyn' function how many updates should be skipped after an event.
-- For example, when we add a user group, we need to skip one update because
-- addition of a group updates users selected for a new group -- users included
-- in the group are no longer part of the available selection.
validDyn
  :: (Show a, Reflex t, MonadHold t m, MonadFix m)
  => a -> Int -> Event t Int -> Event t Int -> Event t a -> (a -> Either Text a)
  -> m (ValidDynamic t a)
validDyn
      initialValue initialSkipUpdateCount
      submitEvent resetEvent updateEvent
      validation
  = foldDyn ($)
    (ValidValue (Reset initialSkipUpdateCount) (validation initialValue))
    ( leftmost
      [ onSubmit            <$> submitEvent
      , onReset             <$> resetEvent
      , onUpdate validation <$> updateEvent
      ]
    )

validValueEither (ValidValue _ e) = e

isValidValue = isRight . validValueEither

maybeValidValue = either (const Nothing) Just . validValueEither

type ErrorDynamic t = Dynamic t Text

errorStr = either id (const "")

errorDyn
  :: forall t m a b . (Reflex t, MonadHold t m)
  => Event t b -> ValidDynamic t a -> m (ErrorDynamic t)
errorDyn submitEvent validDynamic = do
  let error :: Dynamic t Text = validDynamic >>= \case
        ValidValue (Submitted _) e -> return . errorStr $ e
        ValidValue (DataChanged) e -> return . errorStr $ e
        ValidValue (Reset     _) _ -> return ""
  holdDyn ""
    ( tagPromptlyDyn error
      ( leftmost [() <$ submitEvent, () <$ updated validDynamic] )
    )

tagValid :: (Show a, Reflex t) => ValidDynamic t a -> Event t b -> Event t a
tagValid validInput submitEvent
  = mapMaybe maybeValidValue
  . traceEventIfEnabled "tagValid"
  . tagPromptlyDyn validInput
  $ submitEvent
