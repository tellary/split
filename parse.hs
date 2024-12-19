import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           MoneySplit

main = do
  jsonStr <- readFile "20241229_safari_workspace_default.json"
  let actions = eitherDecode . UTF8.fromString $ jsonStr :: Either String Actions
  putStrLn . show $ actions
