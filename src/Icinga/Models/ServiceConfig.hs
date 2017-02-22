{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.ServiceConfig where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map, fromList, union)
import Icinga.Models.Core
import qualified Icinga.Models.Endpoint as E

defServiceConfig :: ServiceConfig
defServiceConfig =
  ServiceConfig
  { actionUrl = Nothing
  , checkCommand = Nothing
  , checkInterval = Nothing
  , commandEndpoint = Nothing
  , displayName = Nothing
  , enableActiveChecks = Nothing
  , enableEventHandler = Nothing
  , enableFlapping = Nothing
  , enableNotifications = Nothing
  , enablePassiveChecks = Nothing
  , enablePerfData = Nothing
  , flappingThreshold = Nothing
  , groups = Nothing
  , hostname = Nothing
  , iconImage = Nothing
  , iconImageAlt = Nothing
  , maxCheckAttempts = Nothing
  , notes = Nothing
  , notesUrl = Nothing
  , retryInterval = Nothing
  , vars = Nothing
  , volatile = Nothing
  , zone = Nothing
  }

mkServiceConfig :: String -> String -> [String] -> String -> [(String, String)] -> ServiceConfig
mkServiceConfig displayName hostname groups checkCommand vars =
  ServiceConfig
  { actionUrl = Nothing
  , checkCommand = Just checkCommand
  , checkInterval = Nothing
  , commandEndpoint = Nothing
  , displayName = Just displayName
  , enableActiveChecks = Just True
  , enableEventHandler = Just False
  , enableFlapping = Just False
  , enableNotifications = Just True
  , enablePassiveChecks = Just True
  , enablePerfData = Just True
  , flappingThreshold = Nothing
  , groups = case groups of [] -> Nothing; _ -> Just groups
  , hostname = Just hostname
  , iconImage = Nothing
  , iconImageAlt = Nothing
  , maxCheckAttempts = Nothing
  , notes = Nothing
  , notesUrl = Nothing
  , retryInterval = Nothing
  , vars = Just $ fromList vars
  , volatile = Nothing
  , zone = Nothing
  }

data ServiceConfig
  = ServiceConfig
  { actionUrl :: Maybe String
  , checkCommand :: Maybe String
  , checkInterval :: Maybe Int
  , commandEndpoint :: Maybe E.Endpoint
  , displayName :: Maybe String
  , enableActiveChecks :: Maybe Bool
  , enableEventHandler :: Maybe Bool
  , enableFlapping :: Maybe Bool
  , enableNotifications :: Maybe Bool
  , enablePassiveChecks :: Maybe Bool
  , enablePerfData :: Maybe Bool
  , flappingThreshold :: Maybe Int
  , groups :: Maybe [String]
  , hostname :: Maybe String
  , iconImage :: Maybe String
  , iconImageAlt :: Maybe String
  , maxCheckAttempts :: Maybe Int
  , notes :: Maybe String
  , notesUrl :: Maybe String
  , retryInterval :: Maybe Int
  , vars :: Maybe (Map String String)
  , volatile :: Maybe Bool
  , zone :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON ServiceConfig where
  parseJSON (Object v) =
    ServiceConfig <$> v .:? "action_url"
                  <*> v .:? "check_command"
                  <*> v .:? "check_interval"
                  <*> v .:? "command_endpoint"
                  <*> v .:? "display_name"
                  <*> v .:? "enable_active_checks"
                  <*> v .:? "enable_event_handler"
                  <*> v .:? "enable_flapping"
                  <*> v .:? "enable_notifications"
                  <*> v .:? "enable_passive_checks"
                  <*> v .:? "enable_perfdata"
                  <*> v .:? "flapping_threshold"
                  <*> v .:? "groups"
                  <*> v .:? "host_name"
                  <*> v .:? "icon_image"
                  <*> v .:? "icon_image_alt"
                  <*> v .:? "max_check_attempts"
                  <*> v .:? "notes"
                  <*> v .:? "notes_url"
                  <*> v .:? "retry_interval"
                  <*> v .:? "vars"
                  <*> v .:? "volatile"
                  <*> v .:? "zone"
  parseJSON _ = mzero

instance ToJSON ServiceConfig where
  toJSON ServiceConfig {..} =
    object' [ "action_url" .= actionUrl
            , "check_command" .= checkCommand
            , "check_interval" .= checkInterval
            , "command_endpoint" .= commandEndpoint
            , "display_name" .= displayName
            , "enable_active_checks" .= enableActiveChecks
            , "enable_event_handler" .= enableEventHandler
            , "enable_flapping" .= enableFlapping
            , "enable_notifications" .= enableNotifications
            , "enable_passive_checks" .= enablePassiveChecks
            , "enable_perfdata" .= enablePerfData
            , "flapping_threshold" .= flappingThreshold
            , "groups" .= groups
            , "host_name" .= hostname
            , "icon_image" .= iconImage
            , "icon_image_alt" .= iconImageAlt
            , "max_check_attempts" .= maxCheckAttempts
            , "notes" .= notes
            , "notes_url" .= notesUrl
            , "retry_interval" .= retryInterval
            , "vars" .= vars
            , "volatile" .= volatile
            , "zone" .= zone
            ]

updateRoutingKey :: ServiceConfig -> Maybe String -> ServiceConfig
updateRoutingKey x Nothing = x
updateRoutingKey x@ServiceConfig { vars = vars, .. } (Just routingKey) =
  case vars of
    Nothing -> x { vars = Just (fromList [("routing_key", routingKey)]) }
    Just map -> x { vars = Just (fromList [("routing_key", routingKey)] `union` map) }
