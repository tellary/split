{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ExpandableEl where

import Control.Monad.Fix (MonadFix)
import Data.Function     ((&))
import Reflex.Dom        (DomBuilder, Event, EventName (Click), MonadHold,
                          PostBuild, accum, domEvent, el', switchDyn, text,
                          widgetHold)

data ElState = ElCollapsed | ElExpanded deriving (Eq, Show)

swapEl current
  | ElCollapsed == current = ElExpanded
  | otherwise              = ElCollapsed

-- | Creates expandable widget using the `content` function
--
-- The `content` function transforms `ElState` to a corresponding widget.
-- The monadic value of the widget is an `Event` that triggers state change.
expandable :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => (ElState -> m (Event t ())) -> m ()
expandable content = do
  rec
    clickEvDyn <- widgetHold (content ElCollapsed) (fmap content stateEv)
    stateEv <- accum (&) ElCollapsed (swapEl <$ switchDyn clickEvDyn)
  return ()

expandableContent tagName collapsed expanded content = do
  rec
    (containerEl, _) <- el' tagName $
      expandable $ \case
        ElCollapsed -> do
          collapsed
          text " (expand)"
          return $ domEvent Click containerEl
        ElExpanded -> do
          (spanEl, _) <- el' "span" $ do
            expanded
            text " (collapse)"
          content
          return $ domEvent Click spanEl
  return ()

expandableContentLi collapsed expanded content
  = expandableContent "li" collapsed expanded content

expandableLi :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => (ElState -> m ()) -> m ()
expandableLi content = do
  rec
    (liEl, _) <- el' "li" $
      expandable (\state -> content state >> return (() <$ ev))
    let ev = domEvent Click liEl
  return ()
  
