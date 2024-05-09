module AgentProfile.Domain.Types (Attributes, toSnakeCase) where

import Data.Aeson.Types (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Text.Casing (fromAny, toQuietSnake)

-- TODO: allow [{"key": key, "value": value}]? instead of {"key": value}? from JSON
type Attributes = Maybe (Map Text Value)

toSnakeCase :: String -> String
toSnakeCase = toQuietSnake . fromAny
