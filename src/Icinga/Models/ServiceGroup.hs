{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.ServiceGroup where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Map (Map)
import Icinga.Models.Core

data ServiceGroup
  = ServiceGroup
  { name :: String
  , active :: Bool                                      -- Object is active (e.g. a service being checked).
  , displayName :: Maybe String                         -- Optional. A short description of the host (e.g. displayed by external interfaces instead of the name if set).
  , groups :: Maybe [String]                            -- Optional. A list of host groups this host belongs to.
  {-, originalAttributes :: Maybe (Map String String)     -- Original values of object attributes modified at runtime.-}
  , package :: String                                   -- Configuration package name this object belongs to. Local configuration is set to _etc, runtime created objects use _api.
  , paused :: Bool                                      -- Object has been paused at runtime (e.g. IdoMysqlConnection. Defaults to false.
  , templates :: [String]                               -- Templates imported on object compilation.
  , objType :: String                                   -- Object type.
  , vars :: Maybe (Map String String)                   -- Optional. A dictionary containing custom attributes that are specific to this host.
  , version :: Double                                   -- Timestamp when the object was created or modified. Synced throughout cluster nodes.
  }
  deriving (Show, Eq)

instance FromJSON ServiceGroup where
  parseJSON (Object v) =
    ServiceGroup <$> v .: "name"
                 <*> v .: "active"
                 <*> v .:? "display_name"
                 <*> v .:? "groups"
                 {-<*> v .:? "original_attributes"-}
                 <*> v .: "package"
                 <*> v .: "paused"
                 <*> v .: "templates"
                 <*> v .: "type"
                 <*> v .:? "vars"
                 <*> v .: "version"
  parseJSON _ = mzero

instance ToJSON ServiceGroup where
  toJSON ServiceGroup {..} =
    object' [ "name" .= name
            , "active" .= active
            , "display_name" .= displayName
            , "groups" .= groups
            {-, "original_attributes" .= originalAttributes-}
            , "package" .= package
            , "paused" .= paused
            , "templates" .= templates
            , "type" .= objType
            , "vars" .= vars
            , "version" .= version
            ]
