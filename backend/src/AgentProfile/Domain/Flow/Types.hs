{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module AgentProfile.Domain.Flow.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics

import AgentProfile.Domain.Types (Attributes, toSnakeCase)

type StepId = String

data Step = Step
    { stepId :: StepId
    , name :: Maybe Text
    , description :: Maybe Text
    , content :: Text
    , condition :: Maybe Text
    , functionIds :: [String]
    , attributes :: Attributes
    , errorStepId :: Maybe String
    }
    deriving (Eq, Show, Generic)

deriveJSON defaultOptions{fieldLabelModifier = toSnakeCase} ''Step

type FlowId = String

data Flow = Flow
    { flowId :: FlowId
    , name :: Text
    , description :: Maybe Text
    , stepIds :: [StepId]
    , attributes :: Attributes
    , errorPolicy :: Maybe Text
    }
    deriving (Eq, Show, Generic)

deriveJSON defaultOptions{fieldLabelModifier = toSnakeCase} ''Flow
