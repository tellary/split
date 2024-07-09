{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Control.Monad.Fix (MonadFix)
import ExpandableEl      (ElState (ElCollapsed, ElExpanded), expandableLi)
import Reflex.Dom        (DomBuilder, MonadHold, PostBuild, el, mainWidget,
                          text)

testWidget :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => m ()
testWidget = do
  expandableLi $ \case
    ElCollapsed -> text "Collapsed 1"
    ElExpanded -> do
      text "Expanded 1"
      el "ul" $ do
        el "li" $ text "list 1 - 1"
        el "li" $ text "list 1 - 2"
        el "li" $ text "list 1 - 3"

  expandableLi $ \case
    ElCollapsed -> text "Collapsed 2"
    ElExpanded -> do
      text "Expanded 2"
      el "ul" $ do
        el "li" $ text "list 2 - 1"
        el "li" $ text "list 2 - 2"
        el "li" $ text "list 2 - 3"

  expandableLi $ \case
    ElCollapsed -> text "Collapsed 3"
    ElExpanded -> do
      text "Expanded 3"
      el "ul" $ do
        el "li" $ text "list 3 - 1"
        el "li" $ text "list 3 - 2"
        el "li" $ text "list 3 - 3"

main :: IO ()
main = mainWidget testWidget
