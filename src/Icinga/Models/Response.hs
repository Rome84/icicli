{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga.Models.Response where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)

data ResponseItem a
  = ResponseItem
  { attrs :: a
  , joins :: Object
  , meta :: Object
  , name :: String
  {-, type' :: String-}
  }
  deriving (Generic, Show, Eq)

instance (FromJSON a) => FromJSON (ResponseItem a)
instance (ToJSON a) => ToJSON (ResponseItem a) where
  toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }

data Response a
  = Response
  { results :: [ResponseItem a]
  }
  deriving (Generic, Show, Eq)

instance (FromJSON a) => FromJSON (Response a)
instance (ToJSON a) => ToJSON (Response a) where
  toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }

getResponseBody :: Maybe (Response a) -> Maybe [a]
getResponseBody (Just Response {..}) = Just (map attrs results)
getResponseBody _ = Nothing
