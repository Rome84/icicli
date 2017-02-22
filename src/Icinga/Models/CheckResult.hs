{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.CheckResult where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Map (Map)
import Icinga.Models.Core

data CheckResult
  = CheckResult
  { active :: Bool                          -- Boolean	Whether the result is from an active or passive check.
  , checkSource :: String                   -- String	Name of the node executing the check.
  , command :: Maybe [Value]                -- Value	Array of command with shell-escaped arguments or command line string.
  , executionEnd :: Double                  -- Number	Check execution end time (as a UNIX timestamp).
  , executionStart :: Double                -- Number	Check execution start time (as a UNIX timestamp).
  , exitStatus :: Int                       -- Number	The exit status returned by the check execution.
  , output :: String                        -- String	The check output.
  , performanceData :: Maybe [Value]        -- Array	Array of performance data values.
  , scheduleEnd :: Double                   -- Number	Scheduled check execution end time (as a UNIX timestamp).
  , scheduleStart :: Double                 -- Number	Scheduled check execution start time (as a UNIX timestamp).
  , state :: Int                            -- Number	The current state (0 = OK, 1 = WARNING, 2 = CRITICAL, 3 = UNKNOWN).
  }
  deriving (Show, Eq)

instance FromJSON CheckResult where
  parseJSON (Object v) =
    CheckResult <$> v .: "active"
                <*> v .: "check_source"
                <*> v .:? "command"
                <*> v .: "execution_end"
                <*> v .: "execution_start"
                <*> v .: "exit_status"
                <*> v .: "output"
                <*> v .: "performance_data"
                <*> v .: "schedule_end"
                <*> v .: "schedule_start"
                <*> v .: "state"
  parseJSON _ = mzero

instance ToJSON CheckResult where
  toJSON CheckResult {..} =
    object' [ "active" .= active
            , "check_source" .= checkSource
            , "command" .= command
            , "execution_end" .= executionEnd
            , "execution_start" .= executionStart
            , "exit_status" .= exitStatus
            , "output" .= output
            , "performance_data" .= performanceData
            , "schedule_end" .= scheduleEnd
            , "schedule_start" .= scheduleStart
            , "state" .= state
            ]
