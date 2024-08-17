{-# LANGUAGE OverloadedStrings #-}

import JavaScript.Web.Storage
import MoneySplit
import Data.Aeson
import Data.JSString
import  Data.ByteString.Lazy.UTF8

main :: IO ()
main = do
  setItem "testKey" (pack . toString . encode $ actions1) localStorage
