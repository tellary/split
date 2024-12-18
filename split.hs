{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

import Automerge                          (AutomergeUrl (AutomergeUrl))
import AutomergeWorkspaceStore            (AutomergeWorkspaceStore (AutomergeWorkspaceStore),
                                           addExternalWorkspace)
import Data.FileEmbed                     (embedFile)
import Data.JSString                      (pack)
import GHCJS.DOM                          (currentDocumentUnchecked)
import GHCJS.DOM.JSFFI.Generated.Document (getBodyUnchecked,
                                           getLocationUnchecked)
import GHCJS.DOM.JSFFI.Generated.Element  (setInnerHTML)
import GHCJS.DOM.JSFFI.Generated.Location (getHash)
import Reflex.Dom                         (mainWidgetWithCss)
import SplitUI                            (app)
import Text.Printf                        (printf)
import WorkspaceStore                     (Workspace (workspaceId))

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
                ++ "Remove '#%s' from the URL if the workspace is not loading."
              )
              url url
      setInnerHTML body (pack msg)
      maybeWs <- addExternalWorkspace (AutomergeUrl url)
      return . fmap workspaceId $ maybeWs
  putStrLn $ "wsId: " ++ show maybeWsId
  mainWidgetWithCss
    $(embedFile "split.css")
    (app AutomergeWorkspaceStore maybeWsId)
