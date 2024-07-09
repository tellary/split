{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module ExpandableEl where

import Control.Monad.Fix (MonadFix)
import Data.Text         (Text)
import Reflex.Dom        (DomBuilder, EventName (Click), MonadHold, PostBuild,
                          domEvent, dyn, el', foldDyn)

data ElState = ElCollapsed | ElExpanded deriving (Eq, Show)

swapEl current
  | ElCollapsed == current = ElExpanded
  | otherwise              = ElCollapsed

expandableEl :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => Text -> (ElState -> m ()) -> m ()
expandableEl elName content = do
  rec
    (element, _) <- el' elName $ dyn (fmap content stateEv)
    let clickEv = domEvent Click element
    stateEv <- foldDyn ($) ElCollapsed (swapEl <$ clickEv)
  return ()

expandableLi :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => (ElState -> m ()) -> m ()
expandableLi = expandableEl "li"
  
