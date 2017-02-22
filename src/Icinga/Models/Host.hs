{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.Host where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Map (Map)
import Icinga.Models.Core
import qualified Icinga.Models.CheckResult as CR

data Host
  = Host
  { name :: String
  , acknowledgement :: Int                              -- Number	The acknowledgement type (0 = NONE, 1 = NORMAL, 2 = STICKY).
  , acknowledgementExpiry :: Int                        -- Number	When the acknowledgement expires (as a UNIX timestamp; 0 = no expiry).
  , actionUrl :: Maybe String                           -- Optional. Url for actions for the host (for example, an external graphing tool).
  , active :: Bool                                      -- Object is active (e.g. a service being checked).
  , address :: Maybe String                             -- Optional. The host's address. Available as command runtime macro $address$ if set.
  , address6 :: Maybe String                            -- Optional. The host's address. Available as command runtime macro $address6$ if set.
  , checkAttempt :: Int                                 -- Number	The current check attempt number.
  , checkCommand :: String                              -- Required. The name of the check command.
  , checkInterval :: Maybe Int                          -- Optional. The check interval (in seconds). This interval is used for checks when the host is in a HARD state. Defaults to 5 minutes.
  , checkPeriod :: Maybe String                         -- Optional. The name of a time period which determines when this host should be checked. Not set by default.
  , commandEndpoint :: Maybe Value                      -- Optional. The endpoint where commands are executed on.
  , displayName :: Maybe String                         -- Optional. A short description of the host (e.g. displayed by external interfaces instead of the name if set).
  , enableActiveChecks :: Maybe Bool                    -- Optional. Whether active checks are enabled. Defaults to true.
  , enableEventHandler :: Maybe Bool                    -- Optional. Enables event handlers for this host. Defaults to true.
  , enableFlapping :: Maybe Bool                        -- Optional. Whether flap detection is enabled. Defaults to false.
  , enableNotifications :: Maybe Bool                   -- Optional. Whether notifications are enabled. Defaults to true.
  , enablePassiveChecks :: Maybe Bool                   -- Optional. Whether passive checks are enabled. Defaults to true.
  , enablePerfData :: Maybe Bool                        -- Optional. Whether performance data processing is enabled. Defaults to true.
  , eventCommand :: Maybe String                        -- Optional. The name of an event command that should be executed every time the host's state changes or the host is in a SOFT state.
  , flapping :: Bool                                    -- Boolean	Whether the host is flapping between states.
  , flappingLastChange :: Double                        -- Number	When the last flapping change occurred (as a UNIX timestamp).
  , flappingThreshold :: Maybe Int                      -- Optional. The flapping threshold in percent when a host is considered to be flapping.
  , groups :: Maybe [String]                            -- Optional. A list of host groups this host belongs to.
  , iconImage :: Maybe String                           -- Optional. Icon image for the host. Used by external interfaces only.
  , iconImageAlt :: Maybe String                        -- Optional. Icon image description for the host. Used by external interface only.
  , lastCheck :: Double                                 -- Number	When the last check occured (as a UNIX timestamp).
  , lastCheckResult :: Maybe CR.CheckResult             -- CheckResult	The current check result.
  , lastHardState :: Int                                -- Number	The last hard state (0 = UP, 1 = DOWN).
  , lastHardStateChange :: Double                       -- Number	When the last hard state change occurred (as a UNIX timestamp).
  , lastInDowntime :: Bool                              -- Boolean	Whether the host was in a downtime when the last check occurred.
  , lastReachable :: Bool                               -- Boolean	Whether the host was reachable when the last check occurred.
  , lastState :: Int                                    -- Number	The previous state (0 = UP, 1 = DOWN).
  , lastStateChange :: Double                           -- Number	When the last state change occurred (as a UNIX timestamp).
  , lastStateDown :: Double                             -- Number	When the last DOWN state occurred (as a UNIX timestamp).
  , lastStateType :: Int                                -- Number	The previous state type (0 = SOFT, 1 = HARD).
  , lastStateUp :: Double                               -- Number	When the last UP state occurred (as a UNIX timestamp).
  , maxCheckAttempts :: Maybe Int                       -- Optional. The number of times a host is re-checked before changing into a hard state. Defaults to 3.
  , nextCheck :: Double                                 -- Number	When the next check occurs (as a UNIX timestamp).
  , notes :: Maybe String                               -- Optional. Notes for the host.
  , notesUrl :: Maybe String                            -- Optional. Url for notes for the host (for example, in notification commands).
  {-, originalAttributes :: Maybe (Map String String)     -- Original values of object attributes modified at runtime.-}
  , package :: String                                   -- Configuration package name this object belongs to. Local configuration is set to _etc, runtime created objects use _api.
  , paused :: Bool                                      -- Object has been paused at runtime (e.g. IdoMysqlConnection. Defaults to false.
  , retryInterval :: Maybe Int                          -- Optional. The retry interval (in seconds). This interval is used for checks when the host is in a SOFT state. Defaults to 1 minute.
  , state :: Int                                        -- Number	The current state (0 = UP, 1 = DOWN).
  , stateType :: Int                                    -- Number	The current state type (0 = SOFT, 1 = HARD).
  , templates :: [String]                               -- Templates imported on object compilation.
  , objType :: String                                   -- Object type.
  , vars :: Maybe (Map String String)                   -- Optional. A dictionary containing custom attributes that are specific to this host.
  , version :: Double                                   -- Timestamp when the object was created or modified. Synced throughout cluster nodes.
  , volatile :: Maybe Bool                              -- Optional. The volatile setting enables always HARD state types if NOT-OK state changes occur.
  , zone :: Maybe String                                -- Optional. The zone this object is a member of.
  }
  deriving (Show, Eq)

instance FromJSON Host where
  parseJSON (Object v) =
    Host <$> v .: "name"
         <*> v .: "acknowledgement"
         <*> v .: "acknowledgement_expiry"
         <*> v .:? "action_url"
         <*> v .: "active"
         <*> v .:? "address"
         <*> v .:? "address6"
         <*> v .: "check_attempt"
         <*> v .: "check_command"
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
         <*> v .: "flapping"
         <*> v .: "flapping_last_change"
         <*> v .:? "flapping_threshold"
         <*> v .:? "groups"
         <*> v .:? "icon_image"
         <*> v .:? "icon_image_alt"
         <*> v .: "last_check"
         <*> v .:? "last_check_result"
         <*> v .: "last_hard_state"
         <*> v .: "last_hard_state_change"
         <*> v .: "last_in_downtime"
         <*> v .: "last_reachable"
         <*> v .: "last_state"
         <*> v .: "last_state_change"
         <*> v .: "last_state_down"
         <*> v .: "last_state_type"
         <*> v .: "last_state_up"
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

instance ToJSON Host where
  toJSON Host {..} =
    object' [ "name" .= name
            , "acknowledgement" .= acknowledgement
            , "acknowledgement_expiry" .= acknowledgementExpiry
            , "action_url" .= actionUrl
            , "active" .= active
            , "address" .= address
            , "address6" .= address6
            , "check_attempt" .= checkAttempt
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
            , "flapping" .= flapping
            , "flapping_last_change" .= flappingLastChange
            , "flapping_threshold" .= flappingThreshold
            , "groups" .= groups
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
            , "last_state_down" .= lastStateDown
            , "last_state_type" .= lastStateType
            , "last_state_up" .= lastStateUp
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
