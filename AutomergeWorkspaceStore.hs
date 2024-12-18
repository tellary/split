{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module AutomergeWorkspaceStore where

import Automerge              (AutomergeUrl (AutomergeUrl), createDocument,
                               findDocument, updateDocument)
import BrowserWorkspaceStore  (BrowserWorkspaceStore (BrowserWorkspaceStore))
import Control.Monad          (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.JSString          (JSString, pack, unpack)
import Data.List              (isPrefixOf)
import Data.Maybe             (fromJust, isJust)
import JavaScript.Web.Storage (getIndex, getItem, getLength, localStorage,
                               removeItem, setItem)
import MoneySplit             (Actions (Actions))
import WorkspaceStore         (Workspace (Workspace, workspaceId,
                                          workspaceName),
                               WorkspaceId (WorkspaceId),
                               WorkspaceStore (createWorkspace, deleteWorkspace,
                                               getActions, getWorkspaces,
                                               migrate, putActions,
                                               wipeWorkspace))

data AutomergeWorkspaceStore = AutomergeWorkspaceStore

workspaceKeyPrefix = "automerge_workspace:"
workspaceKeyPrefixLen = length workspaceKeyPrefix
workspaceKeyStr url = workspaceKeyPrefix ++ url
workspaceKey :: WorkspaceId -> JSString
workspaceKey (WorkspaceId url) = pack . workspaceKeyStr $ url

getIndexStr :: MonadIO m => Int -> m (Maybe String)
getIndexStr i = liftIO $ do
  jsStrMaybe <- getIndex i localStorage
  return $ fmap unpack jsStrMaybe

workspaceNameByUrl :: String -> IO String
workspaceNameByUrl url = do
  jsStrMaybe <- getItem (workspaceKey . WorkspaceId $ url) localStorage
  return
    . maybe (error "workspaceNameByUrl: missing key: " ++ workspaceKeyStr url) unpack
    $ jsStrMaybe

addExternalWorkspace (AutomergeUrl url) = do
  let wsId = WorkspaceId url
  wss <- getWorkspaces AutomergeWorkspaceStore
  case filter (\ws -> workspaceId ws == wsId) wss of
    [existingWs] -> return . Just $ existingWs
    [] -> do
      maybeWsName <- findDocument (AutomergeUrl url) "workspaceName"
      case maybeWsName of
        Just wsName -> do
          setItem (workspaceKey wsId) (pack $ wsName) localStorage
          return . Just $ Workspace wsId wsName
        Nothing -> return Nothing
    (_:_) -> error "addExternalWorkspace: workspaces are unique"

-- | 'automerge.org' based implementation of persistance
--
-- This initial implementation sets entire `actions` JSON instead of making fine
-- grained changes. This will result in overriding concurrent edits. But it is
-- simple to implement and is better than nothing.
instance WorkspaceStore AutomergeWorkspaceStore where
  createWorkspace _ workspaceName = liftIO $ do
    AutomergeUrl url <- createDocument "workspaceName" workspaceName
    setItem (workspaceKey (WorkspaceId url)) (pack $ workspaceName) localStorage
    return $ Workspace (WorkspaceId url) workspaceName
  putActions _ (WorkspaceId url) actions
    = liftIO $ updateDocument (AutomergeUrl url) "actions" actions
  getActions _ (WorkspaceId url) = liftIO $ do
    maybeActions <- findDocument (AutomergeUrl url) "actions"
    if null maybeActions
      then return $ Actions [] [] []
      else return . fromJust $ maybeActions
  deleteWorkspace _ workspaceId = liftIO $ do
    -- Temprorary, we don't call 'deleteDocument'
    -- Will figure out later if it makes sense.
    -- Currently, deleting the workspace only detaches it in a specific browser.
    removeItem (workspaceKey workspaceId) localStorage
  wipeWorkspace _ (WorkspaceId url)
    = liftIO $ updateDocument (AutomergeUrl url) "actions" (Actions [] [] [])
  getWorkspaces _ = liftIO $ do
    len <- getLength localStorage
    urls <- map (drop workspaceKeyPrefixLen)
            . filter (workspaceKeyPrefix `isPrefixOf`)
            . map fromJust
            . filter isJust
            <$> mapM getIndexStr [0..len - 1]
    names <- mapM workspaceNameByUrl urls
    return $ zipWith Workspace (map WorkspaceId urls) names
  migrate this = do
    migrate BrowserWorkspaceStore
    oldWss <- getWorkspaces BrowserWorkspaceStore
    forM_ oldWss $ \oldWs -> do
      newWs <- createWorkspace this (workspaceName oldWs)
      actions <- getActions BrowserWorkspaceStore (workspaceId oldWs)
      putActions this (workspaceId newWs) actions
      deleteWorkspace BrowserWorkspaceStore (workspaceId oldWs)
