{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AgentProfile.Domain.Function.Types where

import Data.Aeson
import Data.Map.Strict qualified as M
import Data.Text (Text)
import GHC.Generics

data Function = Function
    { name :: Text
    , desc :: Text
    , content :: Text
    , inputData :: Text
    , outputData :: Text
    , attributes :: M.Map Text Value
    }
    deriving (Eq, Show, Generic)

instance FromJSON Function
instance ToJSON Function
