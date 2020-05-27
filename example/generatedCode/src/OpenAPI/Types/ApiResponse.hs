{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the types generated from the schema ApiResponse
module OpenAPI.Types.ApiResponse where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
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

-- | Defines the data type for the schema ApiResponse
-- 
-- 
data ApiResponse = ApiResponse {
  -- | code
  apiResponseCode :: (GHC.Maybe.Maybe GHC.Int.Int32)
  -- | message
  , apiResponseMessage :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | type
  , apiResponseType :: (GHC.Maybe.Maybe GHC.Base.String)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON ApiResponse
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "code" (apiResponseCode obj) : (Data.Aeson..=) "message" (apiResponseMessage obj) : (Data.Aeson..=) "type" (apiResponseType obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "code" (apiResponseCode obj) GHC.Base.<> ((Data.Aeson..=) "message" (apiResponseMessage obj) GHC.Base.<> (Data.Aeson..=) "type" (apiResponseType obj)))
instance Data.Aeson.Types.FromJSON.FromJSON ApiResponse
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "ApiResponse" (\obj -> ((GHC.Base.pure ApiResponse GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "code")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "message")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "type"))