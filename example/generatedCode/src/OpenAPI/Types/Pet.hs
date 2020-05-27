{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the types generated from the schema Pet
module OpenAPI.Types.Pet where

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
import OpenAPI.Types.Category
import OpenAPI.Types.Tag

-- | Defines the data type for the schema Pet
-- 
-- 
data Pet = Pet {
  -- | category
  petCategory :: (GHC.Maybe.Maybe Category)
  -- | id
  , petId :: (GHC.Maybe.Maybe GHC.Int.Int64)
  -- | name
  , petName :: GHC.Base.String
  -- | photoUrls
  , petPhotoUrls :: ([] GHC.Base.String)
  -- | status: pet status in the store
  , petStatus :: (GHC.Maybe.Maybe PetStatus)
  -- | tags
  , petTags :: (GHC.Maybe.Maybe ([] Tag))
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON Pet
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "category" (petCategory obj) : (Data.Aeson..=) "id" (petId obj) : (Data.Aeson..=) "name" (petName obj) : (Data.Aeson..=) "photoUrls" (petPhotoUrls obj) : (Data.Aeson..=) "status" (petStatus obj) : (Data.Aeson..=) "tags" (petTags obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "category" (petCategory obj) GHC.Base.<> ((Data.Aeson..=) "id" (petId obj) GHC.Base.<> ((Data.Aeson..=) "name" (petName obj) GHC.Base.<> ((Data.Aeson..=) "photoUrls" (petPhotoUrls obj) GHC.Base.<> ((Data.Aeson..=) "status" (petStatus obj) GHC.Base.<> (Data.Aeson..=) "tags" (petTags obj))))))
instance Data.Aeson.Types.FromJSON.FromJSON Pet
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "Pet" (\obj -> (((((GHC.Base.pure Pet GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "category")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "name")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "photoUrls")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "status")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "tags"))
-- | Defines the enum schema PetStatus
-- 
-- pet status in the store
data PetStatus
    = PetStatusEnumOther Data.Aeson.Types.Internal.Value
    | PetStatusEnumTyped GHC.Base.String
    | PetStatusEnumString_available
    | PetStatusEnumString_pending
    | PetStatusEnumString_sold
    deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.ToJSON PetStatus
    where toJSON (PetStatusEnumOther patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (PetStatusEnumTyped patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (PetStatusEnumString_available) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "available"
          toJSON (PetStatusEnumString_pending) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "pending"
          toJSON (PetStatusEnumString_sold) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "sold"
instance Data.Aeson.FromJSON PetStatus
    where parseJSON val = GHC.Base.pure (if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "available")
                                          then PetStatusEnumString_available
                                          else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "pending")
                                                then PetStatusEnumString_pending
                                                else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "sold")
                                                      then PetStatusEnumString_sold
                                                      else PetStatusEnumOther val)