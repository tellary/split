{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}

module Automerge where

import           Data.Aeson                (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.JSString             (JSString, pack, unpack)

newtype AutomergeUrl = AutomergeUrl String deriving Show
  
createDocument :: ToJSON a => String -> a -> IO AutomergeUrl
createDocument prop value = do
  url <- js_createDocument (pack prop) (pack . UTF8.toString . encode $ value)
  return . AutomergeUrl . unpack $ url

updateDocument :: ToJSON a => AutomergeUrl -> String -> a -> IO ()
updateDocument (AutomergeUrl url) prop value
  = js_updateDocument
    (pack url) (pack prop) (pack . UTF8.toString . encode $ value)

findDocument :: FromJSON a => AutomergeUrl -> String -> IO a
findDocument (AutomergeUrl url) prop = do
  json <- js_findDocument (pack url) (pack prop)
  let bs = UTF8.fromString . unpack $ json
  case decode bs of
    Just result -> return result
    Nothing -> error "Failed to decode Automerge document"
    
foreign import javascript interruptible "createDocument($1, $2).then($c);"
  js_createDocument :: JSString -> JSString -> IO JSString

foreign import javascript interruptible "updateDocument($1, $2, $3).then($c);"
  js_updateDocument :: JSString -> JSString -> JSString -> IO ()

foreign import javascript interruptible "findDocument($1, $2).then($c);"
  js_findDocument :: JSString -> JSString -> IO JSString
