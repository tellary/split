{-# LANGUAGE OverloadedStrings #-}

module BrowserWorkspaceStore where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Aeson                (FromJSON, decode, encode)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.JSString             (JSString, pack, unpack)
import           JavaScript.Web.Storage    (getItem, localStorage, setItem)
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

instance WorkspaceStore BrowserWorkspaceStore where
  putActions _ workspaceName actions
    = setJson (workspaceKey workspaceName) actions
  getActions _ workspaceName
    = getJson "actions" (workspaceKey workspaceName) (Actions [] [] [])
  putWorkspaces _ workspaces
    = setJson "workspaces" workspaces
  getWorkspaces _
    = getJson "workspaces" "workspaces" [defaultWorkpsaceName]
