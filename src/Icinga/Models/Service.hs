{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.Service where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Map (Map)
import Icinga.Models.Core
import qualified Icinga.Models.CheckResult as CR

data Service
  = Service
  { name :: String                                      -- Required. The service name. Must be unique on a per-host basis (Similar to the service_description attribute in Icinga 1.x).
  , acknowledgement :: Int                              -- The acknowledgement type (0 = NONE, 1 = NORMAL, 2 = STICKY).
  , acknowledgementExpiry :: Int                        -- When the acknowledgement expires (as a UNIX timestamp; 0 = no expiry).
  , actionUrl :: Maybe String                           -- Optional. Url for actions for the service (for example, an external graphing tool).
  , active :: Bool                                      -- Object is active (e.g. a service being checked).
  , checkAttempt :: Int                                 -- The current check attempt number.
  , checkCommand :: String                              -- Required. The name of the check command.
  , checkInterval :: Maybe Int                          -- Optional. The check interval (in seconds). This interval is used for checks when the service is in a HARD state. Defaults to 5 minutes.
  , commandEndpoint :: Maybe Value                      -- Optional. The endpoint where commands are executed on.
  , displayName :: Maybe String                         -- Optional. A short description of the service.
  , enableActiveChecks :: Maybe Bool                    -- Optional. Whether active checks are enabled. Defaults to true.
  , enableEventHandler :: Maybe Bool                    -- Optional. Enables event handlers for this host. Defaults to true.
  , enableFlapping :: Maybe Bool                        -- Optional. Whether flap detection is enabled. Defaults to false.
  , enableNotifications :: Maybe Bool                   -- Optional. Whether notifications are enabled. Defaults to true.
  , enablePassiveChecks :: Maybe Bool                   -- Optional. Whether passive checks are enabled. Defaults to true.
  , enablePerfData :: Maybe Bool                        -- Optional. Whether performance data processing is enabled. Defaults to true.
  , flapping :: Bool                                    -- Whether the host is flapping between states.
  , flappingLastChange :: Double                        -- When the last flapping change occurred (as a UNIX timestamp).
  , flappingNegative :: Maybe Int                       -- 
  , flappingPositive :: Maybe Int                       -- 
  , flappingThreshold :: Maybe Int                      -- Optional. The flapping threshold in percent when a service is considered to be flapping.
  , forceNextCheck :: Maybe Bool                        -- 
  , forceNextNotification :: Maybe Bool                 -- 
  , groups :: Maybe [String]                            -- Optional. The service groups this service belongs to.
  , haMode :: Maybe Int                                 -- 
  , hostName :: String                                  -- Required. The host this service belongs to. There must be a Host object with that name.
  , iconImage :: Maybe String                           -- Optional. Icon image for the service. Used by external interfaces only.
  , iconImageAlt :: Maybe String                        -- Optional. Icon image description for the service. Used by external interface only.
  , lastCheck :: Double                                 -- When the last check occured (as a UNIX timestamp).
  , lastCheckResult :: Maybe CR.CheckResult             -- The current check result.
  , lastHardState :: Int                                -- The last hard state (0 = OK, 1 = WARNING, 2 = CRITICAL, 3 = UNKNOWN).
  , lastHardStateChange :: Double                       -- When the last hard state change occurred (as a UNIX timestamp).
  , lastInDowntime :: Bool                              -- Whether the service was in a downtime when the last check occurred.
  , lastReachable :: Bool                               -- Whether the service was reachable when the last check occurred.
  , lastState :: Int                                    -- The previous state (0 = OK, 1 = WARNING, 2 = CRITICAL, 3 = UNKNOWN).
  , lastStateChange :: Double                           -- When the last state change occurred (as a UNIX timestamp).
  , lastStateCritical :: Double                         -- When the last CRITICAL state occurred (as a UNIX timestamp).
  , lastStateOk :: Double                               -- When the last OK state occurred (as a UNIX timestamp).
  , lastStateType :: Int                                -- The previous state type (0 = SOFT, 1 = HARD).
  , lastStateUnknown :: Double                          -- When the last UNKNOWN state occurred (as a UNIX timestamp).
  , lastStateUnreachable :: Maybe Double                -- 
  , lastStateWarning :: Double                          -- When the last WARNING state occurred (as a UNIX timestamp).
  , maxCheckAttempts :: Maybe Int                       -- Optional. The number of times a service is re-checked before changing into a hard state. Defaults to 3.
  , nextCheck :: Double                                 -- When the next check occurs (as a UNIX timestamp).
  , notes :: Maybe String                               -- Optional. Notes for the service.
  , notesUrl :: Maybe String                            -- Optional. Url for notes for the service (for example, in notification commands).
  {-, originalAttributes :: Maybe (Map String String)   -- -}
  , package :: String                                   -- Configuration package name this object belongs to. Local configuration is set to _etc, runtime created objects use _api.
  , paused :: Bool                                      -- Object has been paused at runtime (e.g. IdoMysqlConnection. Defaults to false.
  , retryInterval :: Maybe Int                          -- Optional. The retry interval (in seconds). This interval is used for checks when the service is in a SOFT state. Defaults to 1 minute.
  , state :: Int                                        -- The current state (0 = OK, 1 = WARNING, 2 = CRITICAL, 3 = UNKNOWN).
  , stateType :: Int                                    -- The current state type (0 = SOFT, 1 = HARD).
  , templates :: [String]                               -- Templates imported on object compilation.
  , objType :: String                                   -- Object type.
  , vars :: Maybe (Map String String)                   -- Optional. A dictionary containing custom attributes that are specific to this service.
  , version :: Double                                   -- Timestamp when the object was created or modified. Synced throughout cluster nodes.
  , volatile :: Maybe Bool                              -- Optional. The volatile setting enables always HARD state types if NOT-OK state changes occur.
  , zone :: Maybe String                                -- Optional. The zone this object is a member of.
  }
  deriving (Show, Eq)

instance FromJSON Service where
  parseJSON (Object v) =
    Service <$> v .: "name"
            <*> v .: "acknowledgement"
            <*> v .: "acknowledgement_expiry"
            <*> v .:? "action_url"
            <*> v .: "active"
            <*> v .: "check_attempt"
            <*> v .: "check_command"
            <*> v .:? "check_interval"
            <*> v .:? "command_endpoint"
            <*> v .:? "display_name"
            <*> v .:? "enable_active_checks"
            <*> v .:? "enable_event_handler"
            <*> v .:? "enable_flapping"
            <*> v .:? "enable_notifications"
            <*> v .:? "enable_passive_checks"
            <*> v .:? "enable_perfdata"
            <*> v .: "flapping"
            <*> v .: "flapping_last_change"
            <*> v .:? "flapping_negative"
            <*> v .:? "flapping_positive"
            <*> v .:? "flapping_threshold"
            <*> v .:? "force_next_check"
            <*> v .:? "force_next_notification"
            <*> v .:? "groups"
            <*> v .:? "ha_mode"
            <*> v .: "host_name"
            <*> v .:? "icon_image"
            <*> v .:? "icon_image_alt"
            <*> v .: "last_check"
            <*> v .: "last_check_result"
            <*> v .: "last_hard_state"
            <*> v .: "last_hard_state_change"
            <*> v .: "last_in_downtime"
            <*> v .: "last_reachable"
            <*> v .: "last_state"
            <*> v .: "last_state_change"
            <*> v .: "last_state_critical"
            <*> v .: "last_state_ok"
            <*> v .: "last_state_type"
            <*> v .: "last_state_unknown"
            <*> v .:? "last_state_unreachable"
            <*> v .: "last_state_warning"
            <*> v .:? "max_check_attempts"
            <*> v .: "next_check"
            <*> v .:? "notes"
            <*> v .:? "notes_url"
            {-<*> v .:? "original_attributes"-}
            <*> v .: "package"
            <*> v .: "paused"
            <*> v .:? "retry_interval"
            <*> v .: "state"
            <*> v .: "state_type"
            <*> v .: "templates"
            <*> v .: "type"
            <*> v .:? "vars"
            <*> v .: "version"
            <*> v .:? "volatile"
            <*> v .:? "zone"
  parseJSON _ = mzero

instance ToJSON Service where
  toJSON Service {..} =
    object' [ "name" .= name
            , "acknowledgement" .= acknowledgement
            , "acknowledgement_expiry" .= acknowledgementExpiry
            , "action_url" .= actionUrl
            , "active" .= active
            , "check_attempt" .= checkAttempt
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
            , "flapping" .= flapping
            , "flapping_last_change" .= flappingLastChange
            , "flapping_negative" .= flappingNegative
            , "flapping_positive" .= flappingPositive
            , "flapping_threshold" .= flappingThreshold
            , "force_next_check" .= forceNextCheck
            , "force_next_notification" .= forceNextNotification
            , "groups" .= groups
            , "ha_mode" .= haMode
            , "host_name" .= hostName
            , "icon_image" .= iconImage
            , "icon_image_alt" .= iconImageAlt
            , "last_check" .= lastCheck
            , "last_check_result" .= lastCheckResult
            , "last_hard_state" .= lastHardState
            , "last_hard_state_change" .= lastHardStateChange
            , "last_in_downtime" .= lastInDowntime
            , "last_reachable" .= lastReachable
            , "last_state" .= lastState
            , "last_state_change" .= lastStateChange
            , "last_state_critical" .= lastStateCritical
            , "last_state_ok" .= lastStateOk
            , "last_state_type" .= lastStateType
            , "last_state_unknown" .= lastStateUnknown
            , "last_state_unreachable" .= lastStateUnreachable
            , "last_state_warning" .= lastStateWarning
            , "max_check_attempts" .= maxCheckAttempts
            , "next_check" .= nextCheck
            , "notes" .= notes
            , "notes_url" .= notesUrl
            {-, "original_attributes" .= originalAttributes-}
            , "package" .= package
            , "paused" .= paused
            , "retry_interval" .= retryInterval
            , "state" .= state
            , "state_type" .= stateType
            , "templates" .= templates
            , "type" .= objType
            , "vars" .= vars
            , "version" .= version
            , "volatile" .= volatile
            , "zone" .= zone
            ]
