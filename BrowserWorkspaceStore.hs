{-# LANGUAGE OverloadedStrings #-}

module BrowserWorkspaceStore where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Aeson                (FromJSON, decode, encode)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.JSString             (JSString, pack, unpack)
import           Data.List                 (isPrefixOf)
import           Data.Maybe                (fromJust, isJust)
import           JavaScript.Web.Storage    (getIndex, getItem, getLength,
                                            localStorage, removeItem, setItem)
import           MoneySplit                (Actions (Actions))
import           Text.Printf               (printf)
import           WorkspaceStore

data BrowserWorkspaceStore = BrowserWorkspaceStore

workspaceKey workspaceName = pack $ "workspace_" ++ workspaceName

setJson key value = liftIO $ do
  setItem
    key
    (pack . UTF8.toString . encode $ value)
    localStorage

getJson :: (MonadIO m, FromJSON a) => String -> JSString -> a -> m a
getJson valueType key defaultValue = liftIO $ do
  strMaybe <- getItem key localStorage
  case strMaybe of
    Just str -> do
      let bs = UTF8.fromString . unpack $ str
      case decode bs of
        Just result -> return result
        Nothing -> error
                   $ printf "Failed to read %s from browser storage" valueType
    Nothing -> return defaultValue

getIndexStr :: MonadIO m => Int -> m (Maybe String)
getIndexStr i = liftIO $ do
  jsStrMaybe <- getIndex i localStorage
  return $ fmap unpack jsStrMaybe

instance WorkspaceStore BrowserWorkspaceStore where
  putActions _ workspaceName actions
    = setJson (workspaceKey workspaceName) actions
  getActions _ workspaceName
    = getJson "actions" (workspaceKey workspaceName) (Actions [] [] [])
  deleteWorkspace _ workspaceName
    = liftIO $ removeItem (workspaceKey workspaceName) localStorage
  getWorkspaces _ = liftIO $ do
    len <- getLength localStorage
    let prefix = "workspace_"
    keys <- filter (prefix `isPrefixOf`)
            . map fromJust
            . filter isJust
            <$> mapM getIndexStr [0..len - 1]
    let prefixLength = length prefix
    return $ map (drop prefixLength) keys
  migrate this = liftIO $ do
    strMaybe <- getItem (pack . UTF8.toString $ "splitActions") localStorage
    case strMaybe of
      Just str -> do
        setItem (workspaceKey defaultWorkspaceName) str localStorage
        removeItem (pack . UTF8.toString $ "splitActions") localStorage
      Nothing -> return ()
    
