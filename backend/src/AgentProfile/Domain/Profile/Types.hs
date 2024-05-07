{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module AgentProfile.Domain.Profile.Types where

import Data.Aeson
import Data.Map.Strict qualified as M
import Data.Text (Text)
import GHC.Generics

data Profile = Profile
    { profileId :: String
    , name :: Text
    , desc :: Text
    , version :: Text
    , personalities :: [Personality]
    , expertise :: [Expertise]
    , behaviors :: [Behavior]
    , mission :: Mission
    , policies :: [Policy]
    }
    deriving (Eq, Show, Generic)

data Mission = Mission
    { statement :: Text
    , purpose :: Text
    }
    deriving (Eq, Show, Generic)

data Policy = Policy
    { name :: Text
    , desc :: Text
    , content :: Text
    , priority :: Double
    , attributes :: M.Map Text Value
    }
    deriving (Eq, Show, Generic)

data Personality = Personality
    { name :: Text
    , desc :: Text
    , content :: Text
    , weight :: Double
    }
    deriving (Eq, Show, Generic)

data Expertise = Expertise
    { domain :: Text
    , level :: Text
    }
    deriving (Eq, Show, Generic)

data Behavior = Behavior
    { stance :: Text
    , style :: Text
    }
    deriving (Eq, Show, Generic)

convert :: Profile -> Text -> Text
convert profile llm = "Converted"
