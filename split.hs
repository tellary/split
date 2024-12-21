{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import Control.Monad.IO.Class (MonadIO(liftIO))
import Automerge                          (AutomergeUrl (AutomergeUrl))
import AutomergeWorkspaceStore            (AutomergeWorkspaceStore (AutomergeWorkspaceStore),
                                           addExternalWorkspace)
import Data.FileEmbed                     (embedFile)
import Data.JSString                      (JSString, pack)
import GHCJS.DOM                          (currentDocumentUnchecked)
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.JSFFI.Generated.Document (getBodyUnchecked,
                                           getLocationUnchecked)
import GHCJS.DOM.JSFFI.Generated.Element  (setInnerHTML)
import GHCJS.DOM.JSFFI.Generated.Location (Location, getHash, setHash, getHref)
import Reflex.Dom                         (mainWidgetWithCss, performEvent_)
import SplitUI                            (app)
import Text.Printf                        (printf)
import WorkspaceStore                     (Workspace (workspaceId),
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
      ( \ev -> performEvent_ (setWsHash location <$> ev) )
      ( \doCopy -> do
          liftIO . putStrLn $ "doCopy: " ++ show doCopy
          if doCopy
            then do
              liftIO . putStrLn $ "Yass!!! Do the copy!"
              url <- getHref location
              liftIO . copyToClipboard $ url
            else return ()
          getHash location
      )
      maybeWsId
    )

setWsHash :: MonadJSM m => Location -> Workspace -> m ()
setWsHash location ws = do
  let (WorkspaceId wsId) = workspaceId ws
  liftIO . putStrLn $ "Set hash: " ++ wsId
  setHash location wsId

foreign import javascript unsafe "navigator.clipboard.writeText($1);"
  js_copyToClipboard :: JSString -> IO ()

copyToClipboard :: String -> IO ()
copyToClipboard text = js_copyToClipboard (pack text)
