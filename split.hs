{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import           Automerge                          (AutomergeUrl (AutomergeUrl))
import           AutomergeWorkspaceStore            (AutomergeWorkspaceStore (AutomergeWorkspaceStore),
                                                     addExternalWorkspace)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Data.FileEmbed                     (embedFile)
import           Data.Functor                       (($>))
import           Data.JSString                      (JSString, pack)
import qualified Data.Text                          as T
import           GHCJS.DOM                          (currentDocumentUnchecked)
import           GHCJS.DOM.JSFFI.Generated.Document (getBodyUnchecked,
                                                     getLocationUnchecked)
import           GHCJS.DOM.JSFFI.Generated.Element  (setInnerHTML)
import           GHCJS.DOM.JSFFI.Generated.Location (Location, getHash, getHref,
                                                     setHash)
import           GHCJS.DOM.Types                    (MonadJSM)
import           Reflex.Dom                         (EventName (Click),
                                                     MonadWidget, blank,
                                                     domEvent, el, elAttr',
                                                     mainWidgetWithCss, text,
                                                     widgetHold_, (=:))
import           SplitUI                            (app)
import           Text.Printf                        (printf)
import           WorkspaceStore                     (Workspace (workspaceId),
                                                     WorkspaceId (WorkspaceId))

main :: IO ()
main = do
  doc <- currentDocumentUnchecked
  location <- getLocationUnchecked doc
  maybeWsId <- getHash location >>= \case
    "" -> return Nothing
    hrefVal -> do
      let url = tail $ hrefVal
      body <- getBodyUnchecked doc
      let msg
            = printf
              ( "Importing workspace '%s'...<br><br>"
                ++ "Click <a href='https://split.apps.tellary.ru'>here</a> "
                ++ "to load the Money Split Application without workspace, "
                ++ "if the '%s' workspace is not loading."
              )
              url url
      setInnerHTML body (pack msg)
      maybeWs <- addExternalWorkspace (AutomergeUrl url)
      return . fmap workspaceId $ maybeWs
  putStrLn $ "wsId: " ++ show maybeWsId
  mainWidgetWithCss
    $(embedFile "split.css")
    ( app
      AutomergeWorkspaceStore
      ( setWsHash location )
      copyShareWorkspaceLink
      maybeWsId
    )

copyShareWorkspaceLink :: MonadWidget t m => m ()
copyShareWorkspaceLink = el "p" $ do
  doc <- currentDocumentUnchecked
  location <- getLocationUnchecked doc
  fullUrl <- getHref location
  (copyEl, _) <- elAttr' "a" ("href" =: T.pack fullUrl) $ text "Copy"
  text . T.pack
    $ " the workspace URL to use it on another device "
    ++ " or to share it with friends, "
    ++ " so that they can view and edit the workspace"
  widgetHold_ (return()) $ domEvent Click copyEl $> do
    liftIO $ copyToClipboard fullUrl    
    el "br" blank
    text "(Link copied)"

setWsHash :: MonadJSM m => Location -> Workspace -> m ()
setWsHash location ws = do
  let (WorkspaceId wsId) = workspaceId ws
  setHash location wsId

foreign import javascript unsafe "navigator.clipboard.writeText($1);"
  js_copyToClipboard :: JSString -> IO ()

copyToClipboard :: String -> IO ()
copyToClipboard text = js_copyToClipboard (pack text)
