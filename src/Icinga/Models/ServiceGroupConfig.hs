{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.ServiceGroupConfig where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map, fromList, union)
import Icinga.Models.Core

defServiceGroupConfig :: ServiceGroupConfig
defServiceGroupConfig =
  ServiceGroupConfig
  { displayName = Nothing
  , groups = Nothing
  , vars = Nothing
  }

mkServiceGroupConfig :: String -> [String] -> [(String, String)] -> ServiceGroupConfig
mkServiceGroupConfig displayName groups vars =
  ServiceGroupConfig
  { displayName = Just displayName
  , groups = Just groups
  , vars = Just $ fromList vars
  }

data ServiceGroupConfig
  = ServiceGroupConfig
  { displayName :: Maybe String                         -- Optional. A short description of the host (e.g. displayed by external interfaces instead of the name if set).
  , groups :: Maybe [String]                            -- Optional. A list of host groups this host belongs to.
  , vars :: Maybe (Map String String)                   -- Optional. A dictionary containing custom attributes that are specific to this host.
  }
  deriving (Show, Eq)

instance FromJSON ServiceGroupConfig where
  parseJSON (Object v) =
    ServiceGroupConfig <$> v .:? "display_name"
                       <*> v .:? "groups"
                       <*> v .:? "vars"
  parseJSON _ = mzero

instance ToJSON ServiceGroupConfig where
  toJSON ServiceGroupConfig {..} =
    object' [ "display_name" .= displayName
            , "groups" .= groups
            , "vars" .= vars
            ]

updateRoutingKey :: ServiceGroupConfig -> Maybe String -> ServiceGroupConfig
updateRoutingKey x Nothing = x
updateRoutingKey x@ServiceGroupConfig { vars = vars, .. } (Just routingKey) =
  case vars of
    Nothing -> x { vars = Just (fromList [("routing_key", routingKey)]) }
    Just map -> x { vars = Just (fromList [("routing_key", routingKey)] `union` map) }
