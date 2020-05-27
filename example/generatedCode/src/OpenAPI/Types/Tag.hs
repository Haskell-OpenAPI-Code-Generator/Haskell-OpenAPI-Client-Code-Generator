{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the types generated from the schema Tag
module OpenAPI.Types.Tag where

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

-- | Defines the data type for the schema Tag
-- 
-- 
data Tag = Tag {
  -- | id
  tagId :: (GHC.Maybe.Maybe GHC.Int.Int64)
  -- | name
  , tagName :: (GHC.Maybe.Maybe GHC.Base.String)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON Tag
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "id" (tagId obj) : (Data.Aeson..=) "name" (tagName obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "id" (tagId obj) GHC.Base.<> (Data.Aeson..=) "name" (tagName obj))
instance Data.Aeson.Types.FromJSON.FromJSON Tag
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "Tag" (\obj -> (GHC.Base.pure Tag GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "name"))