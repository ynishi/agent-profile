{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module AgentProfile.Domain.Function.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics

import AgentProfile.Domain.Types (Attributes, toSnakeCase)

type FunctionId = String

data Function = Function
    { functionId :: String
    , name :: Text
    , description :: Maybe Text
    , content :: Text
    , inputData :: Maybe Text
    , outputData :: Maybe Text
    , attributes :: Attributes
    }
    deriving (Eq, Show, Generic)

deriveJSON defaultOptions{fieldLabelModifier = toSnakeCase} ''Function
