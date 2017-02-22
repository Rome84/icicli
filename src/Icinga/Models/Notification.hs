{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.Notification where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map, fromList, union)
import Icinga.Models.Core

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

data Notification
  = Notification
  { name :: String                                      -- Object is active (e.g. a service being checked).
  , active :: Bool                                      -- Object is active (e.g. a service being checked).
  , command :: String                                   -- Required. The name of the notification command which should be executed when the notification is triggered.
  , commandEndpoint :: Maybe String
  , haMode :: Maybe Int
  , hostName :: String                                  -- Required. The name of the host this notification belongs to.
  , interval :: Maybe Int                               -- Optional. The notification interval (in seconds). This interval is used for active notifications. Defaults to 30 minutes. If set to 0, re-notifications are disabled.
  , lastNotification :: Maybe Double                    -- When the last notification was sent for this Notification object (as a UNIX timestamp).
  , lastProblemNotification :: Maybe Double             -- When the last notification was sent for a problem (as a UNIX timestamp).
  , nextNotification :: Maybe Double                    -- When the next notification is going to be sent for this assuming the associated host/service is still in a non-OK state (as a UNIX timestamp).
  , notificationNumber :: Maybe Int                     -- The notification number
  , notifiedUsers :: Maybe [String]
  {-, originalAttributes :: Maybe (Map String String)     -- Original values of object attributes modified at runtime.-}
  , package :: String                                   -- Configuration package name this object belongs to. Local configuration is set to _etc, runtime created objects use _api.
  , paused :: Bool                                      -- Object has been paused at runtime (e.g. IdoMysqlConnection. Defaults to false.
  , period :: Maybe String                              -- Optional. The name of a time period which determines when this notification should be triggered. Not set by default.
  , serviceName :: Maybe String                         -- Optional. The short name of the service this notification belongs to. If omitted this notification object is treated as host notification.
  , stateFilterReal :: Maybe Int
  , states :: Maybe [Int]                               -- Optional. A list of state filters when this notification should be triggered. By default everything is matched.
  , templates :: [String]                               -- Templates imported on object compilation.
  , times :: Maybe (Map String Double)                  -- Optional. A dictionary containing begin and end attributes for the notification.
  , objType :: String                                   -- Object type.
  , typeFilterReal :: Maybe Int
  , types :: Maybe [Int]                                -- Optional. A list of type filters when this notification should be triggered. By default everything is matched.
  , userGroups :: Maybe [String]                        -- Optional. A list of user group names who should be notified.
  , users :: Maybe [String]                             -- Optional. A list of user names who should be notified.
  , vars :: Maybe (Map String String)                   -- Optional. A dictionary containing custom attributes that are specific to this notification object.
  , version :: Double                                   -- Timestamp when the object was created or modified. Synced throughout cluster nodes.
  , zone :: Maybe String                                -- Optional. The zone this object is a member of.
  }
  deriving (Show, Eq)

instance FromJSON Notification where
  parseJSON (Object v) =
    Notification <$> v .: "name"
                 <*> v .: "active"
                 <*> v .: "command"
                 <*> v .:? "command_endpoint"
                 <*> v .:? "ha_mode"
                 <*> v .: "host_name"
                 <*> v .:? "interval"
                 <*> v .:? "last_notification"
                 <*> v .:? "last_problem_notification"
                 <*> v .:? "next_notification"
                 <*> v .:? "notification_number"
                 <*> v .:? "notified_users"
                 <*> v .: "package"
                 <*> v .: "paused"
                 <*> v .:? "period"
                 <*> v .:? "service_name"
                 <*> v .:? "state_filter_real"
                 <*> v .:? "states"
                 <*> v .: "templates"
                 <*> v .:? "times"
                 <*> v .: "type"
                 <*> v .:? "type_filter_real"
                 <*> v .:? "types"
                 <*> v .:? "user_groups"
                 <*> v .:? "users"
                 <*> v .:? "vars"
                 <*> v .: "version"
                 <*> v .:? "zone"
  parseJSON _ = mzero

instance ToJSON Notification where
  toJSON Notification {..} =
    object' [ "name" .= name
            , "active" .= active
            , "command" .= command
            , "command_endpoint" .= commandEndpoint
            , "ha_mode" .= haMode
            , "host_name" .= hostName
            , "interval" .= interval
            , "last_notification" .= lastNotification
            , "last_problem_notification" .= lastProblemNotification
            , "next_notification" .= nextNotification
            , "notification_number" .= notificationNumber
            , "notified_users" .= notifiedUsers
            , "package" .= package
            , "pause" .= paused
            , "period" .= period
            , "service_name" .= serviceName
            , "state_filter_real" .= stateFilterReal
            , "states" .= states
            , "templates" .= templates
            , "times" .= times
            , "type" .= objType
            , "type_filter_real" .= typeFilterReal
            , "types" .= types
            , "user_groups" .= userGroups
            , "users" .= users
            , "vars" .= vars
            , "version" .= version
            , "zone" .= zone
            ]
