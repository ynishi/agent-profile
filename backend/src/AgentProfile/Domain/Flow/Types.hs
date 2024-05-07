{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AgentProfile.Domain.Flow.Types where

import Data.Aeson
import Data.Map.Strict qualified as M
import Data.Text (Text)
import GHC.Generics

data Flow = Flow
    { flowId :: String
    , name :: Text
    , desc :: Text
    , steps :: [Step]
    , attributes :: M.Map Text Value
    , errorPolicy :: Text
    }
    deriving (Eq, Show, Generic)

data Step = Step
    { stepId :: String
    , name :: Text
    , desc :: Text
    , content :: Text
    , condition :: Text
    , functionIds :: [String]
    , attributes :: M.Map Text Value
    , errorStepId :: String
    }
    deriving (Eq, Show, Generic)

instance FromJSON Flow
instance ToJSON Flow

instance FromJSON Step
instance ToJSON Step
