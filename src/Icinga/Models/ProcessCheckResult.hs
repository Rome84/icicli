{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.ProcessCheckResult where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Map (Map)
import Icinga.Models.Core

data ProcessCheckResult
  = ProcessCheckResult
  { exitStatus :: Int                   -- Required. For services: 0=OK, 1=WARNING, 2=CRITICAL, 3=UNKNOWN, for hosts: 0=OK, 1=CRITICAL.
  , pluginOutput :: String              -- Required. The plugins main output. Does not contain the performance data.
  , performanceData :: Maybe [String]   -- Optional. The performance data.
  , checkCommand :: Maybe [String]      -- Optional. The first entry should be the check commands path, then one entry for each command line option followed by an entry for each of its argument.
  , checkSource :: Maybe String         -- Optional. Usually the name of the command_endpoint.
  }
  deriving (Show, Eq)

instance FromJSON ProcessCheckResult where
  parseJSON (Object v) =
    ProcessCheckResult <$> v .: "exit_status"
                       <*> v .: "plugin_output"
                       <*> v .:? "performance_data"
                       <*> v .:? "check_command"
                       <*> v .:? "check_source"
  parseJSON _ = mzero

instance ToJSON ProcessCheckResult where
  toJSON ProcessCheckResult {..} =
    object' [ "exit_status" .= exitStatus
            , "plugin_output" .= pluginOutput
            , "performance_data" .= performanceData
            , "check_command" .= checkCommand
            , "check_source" .= checkSource
            ]
