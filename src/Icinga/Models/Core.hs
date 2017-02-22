module Icinga.Models.Core where

import Data.Aeson
import Data.Aeson.Types

object' :: [Pair] -> Value
object' = object . filter notNull
  where
    notNull (_, Null) = False
    notNull _ = True
