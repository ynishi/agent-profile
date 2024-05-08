{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module AgentProfile.Domain.Agent.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics

import AgentProfile.Domain.Flow.Types (FlowId)
import AgentProfile.Domain.Function.Types (FunctionId)
import AgentProfile.Domain.Profile.Types (ProfileId)
import AgentProfile.Domain.Types (Attributes, toSnakeCase)

type AgentId = String

data Agent = Agent
    { agentId :: AgentId
    , name :: Text
    , description :: Maybe Text
    , version :: Text
    , profileId :: ProfileId
    , flowId :: FlowId
    , functionIds :: [FunctionId]
    , attributes :: Attributes
    }
    deriving (Eq, Show, Generic)

deriveJSON defaultOptions{fieldLabelModifier = toSnakeCase} ''Agent
