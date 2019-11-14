module Main where
import Data.Aeson (Value(..))
import qualified Data.ByteString as BS
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Yaml (decodeFileThrow, encode)
import Options.Applicative

main :: IO ()
main = do
  filename <- execParser $ info filenameP mempty
  val <- decodeFileThrow filename
  let stripped = stripNulls val
  BS.putStr $ encode stripped

filenameP :: Parser FilePath
filenameP = strArgument (metavar "FILE" <> help "YAML file to load")

stripNulls :: Value -> Value
stripNulls (Object o) = Object $ stripInMap o
stripNulls (Array a) = Array $ fmap stripNulls a
stripNulls a = a

stripInMap :: (Eq k, Hashable k) => HashMap k Value -> HashMap k Value
stripInMap = Map.foldlWithKey' step mempty
  where
    step accum key val = case val of
      Null -> accum
      _ -> Map.insert key (stripNulls val) accum
