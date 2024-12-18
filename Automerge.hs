{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module Automerge
  ( AutomergeUrl(AutomergeUrl)
  , createDocument
  , deleteDocument
  , updateDocument
  , findDocument) where

import           Data.Aeson                (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.JSString             (JSString, pack, unpack)
import           GHCJS.Marshal             (fromJSValUnchecked)
import           GHCJS.Types               (JSVal, isNull)
newtype AutomergeUrl = AutomergeUrl String deriving Show
  
createDocument :: ToJSON a => String -> a -> IO AutomergeUrl
createDocument prop value = do
  url <- js_createDocument (pack prop) (pack . UTF8.toString . encode $ value)
  return . AutomergeUrl . unpack $ url

updateDocument :: ToJSON a => AutomergeUrl -> String -> a -> IO ()
updateDocument (AutomergeUrl url) prop value
  = js_updateDocument
    (pack url) (pack prop) (pack . UTF8.toString . encode $ value)

findDocument :: FromJSON a => AutomergeUrl -> String -> IO (Maybe a)
findDocument (AutomergeUrl url) prop = do
  json <- js_findDocument (pack url) (pack prop)
  if isNull json
    then return Nothing
    else do
      jsonStr <- fromJSValUnchecked $ json
      let bs = UTF8.fromString . unpack $ jsonStr
      case decode bs of
        Just result -> return . Just $ result
        Nothing -> error "Failed to decode Automerge document"

deleteDocument :: AutomergeUrl -> IO ()
deleteDocument (AutomergeUrl url) = js_deleteDocument (pack url)

foreign import javascript interruptible "createDocument($1, $2).then($c);"
  js_createDocument :: JSString -> JSString -> IO JSString

foreign import javascript interruptible "updateDocument($1, $2, $3).then($c);"
  js_updateDocument :: JSString -> JSString -> JSString -> IO ()

foreign import javascript interruptible "findDocument($1, $2).then($c);"
  js_findDocument :: JSString -> JSString -> IO JSVal

foreign import javascript unsafe "deleteDocument($1);"
  js_deleteDocument :: JSString -> IO ()
