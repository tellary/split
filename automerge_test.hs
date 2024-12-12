{-# LANGUAGE DeriveGeneric #-}

import Automerge

import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics        (Generic)

data TestData
  = TestData
  { testStr :: String
  , testInt :: Int
  } deriving (Generic, Show)

instance ToJSON TestData
instance FromJSON TestData

main = do
  let d = TestData "val" 2
  url <- createDocument "testData" d
  putStrLn $ "automerge_test: Created: " ++ show url
  d2 <- findDocument url "testData" :: IO TestData
  putStrLn $ "automerge_test: Found: " ++ show d2
  updateDocument url "testData" d { testInt = 4 }
  putStrLn $ "automerge_test: Updated: " ++ show url
  d3 <- findDocument url "testData" :: IO TestData
  putStrLn $ "automerge_test: Found: " ++ show d3
