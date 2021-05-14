{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains the types generated from the schema Category
module OpenAPI.Types.Category where

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
import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe

-- | Defines the data type for the schema Category
data Category = Category
  { -- | id
    categoryId :: (GHC.Maybe.Maybe GHC.Int.Int64),
    -- | name
    categoryName :: (GHC.Maybe.Maybe GHC.Base.String)
  }
  deriving
    ( GHC.Show.Show,
      GHC.Classes.Eq
    )

instance Data.Aeson.ToJSON Category where
  toJSON obj = Data.Aeson.object ((Data.Aeson..=) "id" (categoryId obj) : (Data.Aeson..=) "name" (categoryName obj) : [])
  toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "id" (categoryId obj) GHC.Base.<> (Data.Aeson..=) "name" (categoryName obj))

instance Data.Aeson.Types.FromJSON.FromJSON Category where
  parseJSON = Data.Aeson.Types.FromJSON.withObject "Category" (\obj -> (GHC.Base.pure Category GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "name"))
