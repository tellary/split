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

data ValidValue a
  = Submitted Int (Either Text a)
  | DataChanged (Either Text a)
  deriving Show

type ValidDynamic t a = Dynamic t (ValidValue a)

traceEnabled = False

traceEventIfEnabled :: (Show a, Reflex t) => String -> Event t a -> Event t a
traceEventIfEnabled = if traceEnabled then traceEvent else flip const

traceIfEnabled = if traceEnabled then trace else flip const

traceUpdate :: Show a => String -> a -> a -> a
traceUpdate tag old new
  = traceIfEnabled (printf "%s: %s -> %s" tag (show old) (show new)) new

onSubmit v@(DataChanged (Left _))
  = traceUpdate "onSubmit" v $ v
onSubmit v@(DataChanged (Right a))
  = traceUpdate "onSubmit" v $ Submitted 0 (Right a)
onSubmit v@(Submitted i a)
  = traceUpdate "onSubmit" v $ Submitted (i + 1) a

onReset v@(DataChanged (Left _))
  = traceUpdate "onReset" v $ v
onReset v@(DataChanged (Right a))
  = traceUpdate "onReset" v $ Submitted 0 (Right a)
onReset v@(Submitted _ a)
  = traceUpdate "onReset" v $ Submitted 1 a

onUpdate validation newVal v@(DataChanged _)
  = traceUpdate "onUpdate" v $ DataChanged (validation newVal)
onUpdate validation newVal v@(Submitted i _)
  = traceUpdate "onUpdate" v $ if i == 0
                               then Submitted 1 (validation newVal)
                               else DataChanged (validation newVal)

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
validDyn
  :: (Show a, Reflex t, MonadHold t m, MonadFix m)
  => a -> Event t b -> Event t c -> Event t a -> (a -> Either Text a)
  -> m (ValidDynamic t a)
validDyn initialValue submitEvent resetEvent updateEvent validation
  = foldDyn ($) (Submitted 1 (validation initialValue))
    ( leftmost
      [ onSubmit <$ submitEvent
      , onReset <$ resetEvent
      , onUpdate validation <$> updateEvent
      ]
    )

eitherValidValue (Submitted _ e) = e
eitherValidValue (DataChanged e) = e

isValidValue = isRight . eitherValidValue

maybeValidValue = either (const Nothing) Just . eitherValidValue

isDataChanged (DataChanged _) = True
isDataChanged (Submitted _ _) = False

type ErrorDynamic t = Dynamic t Text

errorDyn
  :: forall t m a b . (Reflex t, MonadHold t m)
  => Event t b -> ValidDynamic t a -> m (ErrorDynamic t)
errorDyn submitEvent validDynamic = do
  let error :: Dynamic t Text = validDynamic >>= \case
        Submitted i errorOrValue
          | i < 2 -> return ""
          | otherwise -> return . either id (const "") $ errorOrValue
        DataChanged errorOrValue -> return . either id (const "") $ errorOrValue
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
