 {-# LANGUAGE DeriveGeneric #-}

module Icinga.Models.Endpoint where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)

data Endpoint
  = Endpoing
  { host :: String
  , port :: Int
  }
  deriving (Generic, Show, Eq)

instance FromJSON Endpoint
instance ToJSON Endpoint where
  toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }
