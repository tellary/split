{-# LANGUAGE OverloadedStrings #-}

module BrowserActionsStore where

import           ActionsStore
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (decode, encode)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.JSString             (pack, unpack)
import           JavaScript.Web.Storage    (getItem, localStorage, setItem)
import           MoneySplit                (Actions (Actions))

data BrowserActionsStore = BrowserActionsStore

instance ActionsStore BrowserActionsStore where
  putActions _ actions = liftIO $ do
    setItem
      "splitActions"
      (pack . UTF8.toString . encode $ actions)
      localStorage
  getActions _ = liftIO $ do
    strMaybe <- getItem "splitActions" localStorage
    case strMaybe of
      Just str -> do
        let bs = UTF8.fromString . unpack $ str
        case decode bs of
          Just actions -> return actions
          Nothing -> error "Failed to read actions from browser storage"
      Nothing -> return $ Actions [] [] []

