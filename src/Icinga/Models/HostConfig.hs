{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.HostConfig where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map, fromList, union)
import Icinga.Models.Core
import qualified Icinga.Models.Endpoint as E

defHostConfig :: HostConfig
defHostConfig =
  HostConfig
  { actionUrl = Nothing
  , address = Nothing
  , address6 = Nothing
  , checkCommand = Nothing
  , checkInterval = Nothing
  , checkPeriod = Nothing
  , commandEndpoint = Nothing
  , displayName = Nothing
  , enableActiveChecks = Nothing
  , enableEventHandler = Nothing
  , enableFlapping = Nothing
  , enableNotifications = Nothing
  , enablePassiveChecks = Nothing
  , enablePerfData = Nothing
  , eventCommand = Nothing
  , flappingThreshold = Nothing
  , groups = Nothing
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

mkHostConfig :: String -> [String] -> String -> String -> [(String, String)] -> HostConfig
mkHostConfig displayName groups address checkCommand vars =
  HostConfig
  { actionUrl = Nothing
  , address = Just address
  , address6 = Nothing
  , checkCommand = Just checkCommand
  , checkInterval = Nothing
  , checkPeriod = Nothing
  , commandEndpoint = Nothing
  , displayName = Just displayName
  , enableActiveChecks = Just True
  , enableEventHandler = Just False
  , enableFlapping = Just False
  , enableNotifications = Just True
  , enablePassiveChecks = Just True
  , enablePerfData = Just True
  , eventCommand = Nothing
  , flappingThreshold = Nothing
  , groups = case groups of [] -> Nothing; _ -> Just groups
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

data HostConfig
  = HostConfig
  { actionUrl :: Maybe String
  , address :: Maybe String
  , address6 :: Maybe String
  , checkCommand :: Maybe String
  , checkInterval :: Maybe Int
  , checkPeriod :: Maybe String
  , commandEndpoint :: Maybe E.Endpoint
  , displayName :: Maybe String
  , enableActiveChecks :: Maybe Bool
  , enableEventHandler :: Maybe Bool
  , enableFlapping :: Maybe Bool
  , enableNotifications :: Maybe Bool
  , enablePassiveChecks :: Maybe Bool
  , enablePerfData :: Maybe Bool
  , eventCommand :: Maybe String
  , flappingThreshold :: Maybe Int
  , groups :: Maybe [String]
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

instance FromJSON HostConfig where
  parseJSON (Object v) =
    HostConfig <$> v .:? "action_url"
               <*> v .:? "address"
               <*> v .:? "address6"
               <*> v .:? "check_command"
               <*> v .:? "check_interval"
               <*> v .:? "check_period"
               <*> v .:? "command_endpoint"
               <*> v .:? "display_name"
               <*> v .:? "enable_active_checks"
               <*> v .:? "enable_event_handler"
               <*> v .:? "enable_flapping"
               <*> v .:? "enable_notifications"
               <*> v .:? "enable_passive_checks"
               <*> v .:? "enable_perfdata"
               <*> v .:? "event_command"
               <*> v .:? "flapping_threshold"
               <*> v .:? "groups"
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

instance ToJSON HostConfig where
  toJSON HostConfig {..} =
    object' [ "action_url" .= actionUrl
            , "address" .= address
            , "address6" .= address6
            , "check_command" .= checkCommand
            , "check_interval" .= checkInterval
            , "check_period" .= checkPeriod
            , "command_endpoint" .= commandEndpoint
            , "display_name" .= displayName
            , "enable_active_checks" .= enableActiveChecks
            , "enable_event_handler" .= enableEventHandler
            , "enable_flapping" .= enableFlapping
            , "enable_notifications" .= enableNotifications
            , "enable_passive_checks" .= enablePassiveChecks
            , "enable_perfdata" .= enablePerfData
            , "event_command" .= eventCommand
            , "flapping_threshold" .= flappingThreshold
            , "groups" .= groups
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

updateRoutingKey :: HostConfig -> Maybe String -> HostConfig
updateRoutingKey x Nothing = x
updateRoutingKey x@HostConfig { vars = vars, .. } (Just routingKey) =
  case vars of
    Nothing -> x { vars = Just (fromList [("routing_key", routingKey)]) }
    Just map -> x { vars = Just (fromList [("routing_key", routingKey)] `union` map) }
