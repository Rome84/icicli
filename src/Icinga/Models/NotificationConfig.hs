{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.NotificationConfig where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map, fromList, union)
import Icinga.Models.Core

defNotificationConfig :: NotificationConfig
defNotificationConfig =
  NotificationConfig
  { command = Nothing
  , hostName = Nothing
  , interval = Nothing
  , period = Nothing
  , serviceName = Nothing
  , states = Nothing
  , times = Nothing
  , types = Nothing
  , userGroups = Nothing
  , users = Nothing
  , vars = Nothing
  , zone = Nothing
  }

mkNotificationConfig :: String -> Maybe String -> String -> NotificationConfig
mkNotificationConfig hostname servicename command =
  NotificationConfig
  { command = Just command
  , hostName = Just hostname
  , interval = Nothing
  , period = Nothing
  , serviceName = servicename
  , states = Nothing
  , times = Nothing
  , types = Nothing
  , userGroups = Nothing
  , users = Nothing
  , vars = Nothing
  , zone = Nothing
  }

{-
Configuration Attributes:

host_name	Required. The name of the host this notification belongs to.
service_name	Optional. The short name of the service this notification belongs to. If omitted this notification object is treated as host notification.
vars	Optional. A dictionary containing custom attributes that are specific to this notification object.
users	Optional. A list of user names who should be notified.
user_groups	Optional. A list of user group names who should be notified.
times	Optional. A dictionary containing begin and end attributes for the notification.
command	Required. The name of the notification command which should be executed when the notification is triggered.
interval	Optional. The notification interval (in seconds). This interval is used for active notifications. Defaults to 30 minutes. If set to 0, re-notifications are disabled.
period	Optional. The name of a time period which determines when this notification should be triggered. Not set by default.
zone	Optional. The zone this object is a member of.
types	Optional. A list of type filters when this notification should be triggered. By default everything is matched.
states	Optional. A list of state filters when this notification should be triggered. By default everything is matched.

Available notification state filters for Service:

OK
Warning
Critical
Unknown

Available notification state filters for Host:

Up
Down

Available notification type filters:

DowntimeStart
DowntimeEnd
DowntimeRemoved
Custom
Acknowledgement
Problem
Recovery
FlappingStart
FlappingEnd
-}

data NotificationConfig
  = NotificationConfig
  { command :: Maybe String                             -- Required. The name of the notification command which should be executed when the notification is triggered.
  , hostName :: Maybe String                            -- Required. The name of the host this notification belongs to.
  , interval :: Maybe Int                               -- Optional. The notification interval (in seconds). This interval is used for active notifications. Defaults to 30 minutes. If set to 0, re-notifications are disabled.
  , period :: Maybe String                              -- Optional. The name of a time period which determines when this notification should be triggered. Not set by default.
  , serviceName :: Maybe String                         -- Optional. The short name of the service this notification belongs to. If omitted this notification object is treated as host notification.
  , states :: Maybe [Int]                               -- Optional. A list of state filters when this notification should be triggered. By default everything is matched.
  , times :: Maybe (Map String Double)                  -- Optional. A dictionary containing begin and end attributes for the notification.
  , types :: Maybe [Int]                                -- Optional. A list of type filters when this notification should be triggered. By default everything is matched.
  , userGroups :: Maybe [String]                        -- Optional. A list of user group names who should be notified.
  , users :: Maybe [String]                             -- Optional. A list of user names who should be notified.
  , vars :: Maybe (Map String String)                   -- Optional. A dictionary containing custom attributes that are specific to this notification object.
  , zone :: Maybe String                                -- Optional. The zone this object is a member of.
  }
  deriving (Show, Eq)

instance FromJSON NotificationConfig where
  parseJSON (Object v) =
    NotificationConfig <$> v .:? "command"
                       <*> v .:? "host_name"
                       <*> v .:? "interval"
                       <*> v .:? "period"
                       <*> v .:? "service_name"
                       <*> v .:? "states"
                       <*> v .:? "times"
                       <*> v .:? "types"
                       <*> v .:? "user_groups"
                       <*> v .:? "users"
                       <*> v .:? "vars"
                       <*> v .:? "zone"
  parseJSON _ = mzero

instance ToJSON NotificationConfig where
  toJSON NotificationConfig {..} =
    object' [ "command" .= command
            , "host_name" .= hostName
            , "interval" .= interval
            , "period" .= period
            , "service_name" .= serviceName
            , "states" .= states
            , "times" .= times
            , "types" .= types
            , "user_groups" .= userGroups
            , "users" .= users
            , "vars" .= vars
            , "zone" .= zone
            ]

updateRoutingKey :: NotificationConfig -> Maybe String -> NotificationConfig
updateRoutingKey x Nothing = x
updateRoutingKey x@NotificationConfig { vars = vars, .. } (Just routingKey) =
  case vars of
    Nothing -> x { vars = Just (fromList [("routing_key", routingKey)]) }
    Just map -> x { vars = Just (fromList [("routing_key", routingKey)] `union` map) }
