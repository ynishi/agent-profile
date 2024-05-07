{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AgentProfile.Domain.Agent.Types where

import Data.Aeson
import Data.Map.Strict qualified as M
import Data.Text (Text)
import GHC.Generics

data Agent = Agent
    { name :: Text
    , desc :: Text
    , version :: Text
    , profileId :: String
    , flowId :: String
    , functionIds :: [String]
    , attributes :: M.Map Text Value
    }
    deriving (Eq, Show, Generic)

instance FromJSON Agent
instance ToJSON Agent
