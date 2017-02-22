{-# LANGUAGE DeriveGeneric #-}

module Icinga.Models.CreateRequest where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)

data CreateRequest a
  = CreateRequest
  { templates :: Maybe [String]
  , attrs :: a
  }
  deriving (Generic, Show, Eq)

instance (FromJSON a) => FromJSON (CreateRequest a)
instance (ToJSON a) => ToJSON (CreateRequest a) where
  toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }
