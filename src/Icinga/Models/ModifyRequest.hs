{-# LANGUAGE DeriveGeneric #-}

module Icinga.Models.ModifyRequest where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)

data ModifyRequest a
  = ModifyRequest
  { attrs :: a
  }
  deriving (Generic, Show, Eq)

instance (FromJSON a) => FromJSON (ModifyRequest a)
instance (ToJSON a) => ToJSON (ModifyRequest a) where
  toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }
