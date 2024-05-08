{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AgentProfile.Domain.Profile.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Map.Strict qualified as M
import Data.Text (Text)
import GHC.Generics

import AgentProfile.Domain.Types (Attributes, toSnakeCase)

data Mission = Mission
    { statement :: Text
    , purpose :: Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON Mission
instance ToJSON Mission

data Policy = Policy
    { name :: Text
    , description :: Maybe Text
    , content :: Text
    , priority :: Double
    , attributes :: Attributes
    }
    deriving (Eq, Show, Generic)

instance FromJSON Policy
instance ToJSON Policy

data Personality = Personality
    { name :: Text
    , description :: Maybe Text
    , content :: Text
    , weight :: Double
    }
    deriving (Eq, Show, Generic)

instance FromJSON Personality
instance ToJSON Personality

data Expertise = Expertise
    { domain :: Text
    , level :: Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON Expertise
instance ToJSON Expertise

data Behavior = Behavior
    { stance :: Text
    , style :: Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON Behavior
instance ToJSON Behavior

type ProfileId = String

data Profile = Profile
    { profileId :: ProfileId
    , name :: Text
    , description :: Maybe Text
    , personalities :: Maybe [Personality]
    , expertise :: Maybe Expertise
    , behavior :: Maybe Behavior
    , mission :: Maybe Mission
    , policies :: Maybe [Policy]
    }
    deriving (Eq, Show, Generic)

deriveJSON defaultOptions{fieldLabelModifier = toSnakeCase} ''Profile

convert :: Profile -> Text -> Text
convert profile llm = "Converted"
