{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains all types with cyclic dependencies (between each other or to itself)
module OpenAPI.CyclicTypes where

import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Char8 as Data.ByteString.Internal
import qualified Data.Functor
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Generics
import qualified GHC.Int
import qualified GHC.Show
import qualified GHC.Types
import qualified OpenAPI.Common
import OpenAPI.Types.ApiResponse
import OpenAPI.Types.Category
import OpenAPI.Types.Order
import OpenAPI.Types.Pet
import OpenAPI.Types.Tag
import OpenAPI.Types.User
import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
