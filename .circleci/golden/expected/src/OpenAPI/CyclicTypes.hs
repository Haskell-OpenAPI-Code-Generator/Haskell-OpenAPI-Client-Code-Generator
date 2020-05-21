{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains all types with cyclic dependencies (between each other or to itself)
module OpenAPI.CyclicTypes where

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
import OpenAPI.Types.Dog
import OpenAPI.Types.PetByType
import OpenAPI.Types.Test
import OpenAPI.Types.Test2

-- | Defines the data type for the schema Cat
-- 
-- 
data Cat = Cat {
  -- | age
  catAge :: (GHC.Maybe.Maybe GHC.Integer.Type.Integer)
  -- | ananyoftype
  , catAnanyoftype :: (GHC.Maybe.Maybe CatAnanyoftype)
  -- | another_relative
  , catAnother_relative :: (GHC.Maybe.Maybe CatAnother_relativeVariants)
  -- | hunts
  , catHunts :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | relative
  , catRelative :: (GHC.Maybe.Maybe CatRelativeVariants)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON Cat
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "age" (catAge obj) : (Data.Aeson..=) "ananyoftype" (catAnanyoftype obj) : (Data.Aeson..=) "another_relative" (catAnother_relative obj) : (Data.Aeson..=) "hunts" (catHunts obj) : (Data.Aeson..=) "relative" (catRelative obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "age" (catAge obj) GHC.Base.<> ((Data.Aeson..=) "ananyoftype" (catAnanyoftype obj) GHC.Base.<> ((Data.Aeson..=) "another_relative" (catAnother_relative obj) GHC.Base.<> ((Data.Aeson..=) "hunts" (catHunts obj) GHC.Base.<> (Data.Aeson..=) "relative" (catRelative obj)))))
instance Data.Aeson.Types.FromJSON.FromJSON Cat
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "Cat" (\obj -> ((((GHC.Base.pure Cat GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "age")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "ananyoftype")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "another_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "hunts")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "relative"))
-- | Defines the data type for the schema CatAnanyoftype
-- 
-- 
data CatAnanyoftype = CatAnanyoftype {
  
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON CatAnanyoftype
    where toJSON obj = Data.Aeson.object []
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "string" ("string" :: GHC.Base.String))
instance Data.Aeson.Types.FromJSON.FromJSON CatAnanyoftype
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "CatAnanyoftype" (\obj -> GHC.Base.pure CatAnanyoftype)
-- | Define the one-of schema CatAnother_relative
-- 
-- 
data CatAnother_relativeVariants
    = CatAnother_relativeCat Cat
    | CatAnother_relativePetByType PetByType
    | CatAnother_relativeString GHC.Base.String
    deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Generics.Generic)
instance Data.Aeson.ToJSON CatAnother_relativeVariants
    where toJSON = Data.Aeson.Types.ToJSON.genericToJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
instance Data.Aeson.FromJSON CatAnother_relativeVariants
    where parseJSON = Data.Aeson.Types.FromJSON.genericParseJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
-- | Define the one-of schema CatRelative
-- 
-- 
data CatRelativeVariants
    = CatRelativeCat Cat
    | CatRelativePetByType PetByType
    | CatRelativeString GHC.Base.String
    deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Generics.Generic)
instance Data.Aeson.ToJSON CatRelativeVariants
    where toJSON = Data.Aeson.Types.ToJSON.genericToJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
instance Data.Aeson.FromJSON CatRelativeVariants
    where parseJSON = Data.Aeson.Types.FromJSON.genericParseJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
-- | Defines the data type for the schema Mischling
-- 
-- 
data Mischling = Mischling {
  -- | age
  mischlingAge :: GHC.Integer.Type.Integer
  -- | ageThird
  , mischlingAgeThird :: (GHC.Maybe.Maybe GHC.Integer.Type.Integer)
  -- | another_relative
  , mischlingAnother_relative :: (GHC.Maybe.Maybe MischlingAnother_relativeVariants)
  -- | bark
  , mischlingBark :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | binary
  , mischlingBinary :: (GHC.Maybe.Maybe OpenAPI.Common.JsonByteString)
  -- | breed
  , mischlingBreed :: (GHC.Maybe.Maybe MischlingBreed)
  -- | byte
  , mischlingByte :: (GHC.Maybe.Maybe OpenAPI.Common.JsonByteString)
  -- | double
  , mischlingDouble :: (GHC.Maybe.Maybe GHC.Types.Double)
  -- | father
  , mischlingFather :: (GHC.Maybe.Maybe MischlingFather)
  -- | first_relative
  , mischlingFirst_relative :: (GHC.Maybe.Maybe MischlingFirst_relative)
  -- | float
  , mischlingFloat :: (GHC.Maybe.Maybe GHC.Types.Float)
  -- | huntssecond
  , mischlingHuntssecond :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | int32
  , mischlingInt32 :: (GHC.Maybe.Maybe GHC.Int.Int32)
  -- | int64
  , mischlingInt64 :: (GHC.Maybe.Maybe GHC.Int.Int64)
  -- | integer
  , mischlingInteger :: (GHC.Maybe.Maybe GHC.Integer.Type.Integer)
  -- | nickname
  , mischlingNickname :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | number
  , mischlingNumber :: (GHC.Maybe.Maybe GHC.Types.Double)
  -- | pet_type
  , mischlingPet_type :: (GHC.Maybe.Maybe MischlingPet_type)
  -- | relative
  , mischlingRelative :: (GHC.Maybe.Maybe MischlingRelativeVariants)
  -- | secondFather
  , mischlingSecondFather :: (GHC.Maybe.Maybe MischlingSecondFather)
  -- | str
  -- 
  -- Constraints:
  -- 
  -- * Maximum length of 244
  -- * Minimum length of 100
  , mischlingStr :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | strDate
  , mischlingStrDate :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | strDateTime
  , mischlingStrDateTime :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | tags
  , mischlingTags :: (GHC.Maybe.Maybe ([] GHC.Base.String))
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON Mischling
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "age" (mischlingAge obj) : (Data.Aeson..=) "ageThird" (mischlingAgeThird obj) : (Data.Aeson..=) "another_relative" (mischlingAnother_relative obj) : (Data.Aeson..=) "bark" (mischlingBark obj) : (Data.Aeson..=) "binary" (mischlingBinary obj) : (Data.Aeson..=) "breed" (mischlingBreed obj) : (Data.Aeson..=) "byte" (mischlingByte obj) : (Data.Aeson..=) "double" (mischlingDouble obj) : (Data.Aeson..=) "father" (mischlingFather obj) : (Data.Aeson..=) "first_relative" (mischlingFirst_relative obj) : (Data.Aeson..=) "float" (mischlingFloat obj) : (Data.Aeson..=) "huntssecond" (mischlingHuntssecond obj) : (Data.Aeson..=) "int32" (mischlingInt32 obj) : (Data.Aeson..=) "int64" (mischlingInt64 obj) : (Data.Aeson..=) "integer" (mischlingInteger obj) : (Data.Aeson..=) "nickname" (mischlingNickname obj) : (Data.Aeson..=) "number" (mischlingNumber obj) : (Data.Aeson..=) "pet_type" (mischlingPet_type obj) : (Data.Aeson..=) "relative" (mischlingRelative obj) : (Data.Aeson..=) "secondFather" (mischlingSecondFather obj) : (Data.Aeson..=) "str" (mischlingStr obj) : (Data.Aeson..=) "strDate" (mischlingStrDate obj) : (Data.Aeson..=) "strDateTime" (mischlingStrDateTime obj) : (Data.Aeson..=) "tags" (mischlingTags obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "age" (mischlingAge obj) GHC.Base.<> ((Data.Aeson..=) "ageThird" (mischlingAgeThird obj) GHC.Base.<> ((Data.Aeson..=) "another_relative" (mischlingAnother_relative obj) GHC.Base.<> ((Data.Aeson..=) "bark" (mischlingBark obj) GHC.Base.<> ((Data.Aeson..=) "binary" (mischlingBinary obj) GHC.Base.<> ((Data.Aeson..=) "breed" (mischlingBreed obj) GHC.Base.<> ((Data.Aeson..=) "byte" (mischlingByte obj) GHC.Base.<> ((Data.Aeson..=) "double" (mischlingDouble obj) GHC.Base.<> ((Data.Aeson..=) "father" (mischlingFather obj) GHC.Base.<> ((Data.Aeson..=) "first_relative" (mischlingFirst_relative obj) GHC.Base.<> ((Data.Aeson..=) "float" (mischlingFloat obj) GHC.Base.<> ((Data.Aeson..=) "huntssecond" (mischlingHuntssecond obj) GHC.Base.<> ((Data.Aeson..=) "int32" (mischlingInt32 obj) GHC.Base.<> ((Data.Aeson..=) "int64" (mischlingInt64 obj) GHC.Base.<> ((Data.Aeson..=) "integer" (mischlingInteger obj) GHC.Base.<> ((Data.Aeson..=) "nickname" (mischlingNickname obj) GHC.Base.<> ((Data.Aeson..=) "number" (mischlingNumber obj) GHC.Base.<> ((Data.Aeson..=) "pet_type" (mischlingPet_type obj) GHC.Base.<> ((Data.Aeson..=) "relative" (mischlingRelative obj) GHC.Base.<> ((Data.Aeson..=) "secondFather" (mischlingSecondFather obj) GHC.Base.<> ((Data.Aeson..=) "str" (mischlingStr obj) GHC.Base.<> ((Data.Aeson..=) "strDate" (mischlingStrDate obj) GHC.Base.<> ((Data.Aeson..=) "strDateTime" (mischlingStrDateTime obj) GHC.Base.<> (Data.Aeson..=) "tags" (mischlingTags obj))))))))))))))))))))))))
instance Data.Aeson.Types.FromJSON.FromJSON Mischling
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "Mischling" (\obj -> (((((((((((((((((((((((GHC.Base.pure Mischling GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "age")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "ageThird")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "another_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "bark")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "binary")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "breed")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "byte")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "double")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "father")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "first_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "float")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "huntssecond")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "int32")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "int64")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "integer")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "nickname")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "number")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "pet_type")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "secondFather")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "str")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "strDate")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "strDateTime")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "tags"))
-- | Defines the data type for the schema MischlingAnother_relativeOneOf4
-- 
-- 
data MischlingAnother_relativeOneOf4 = MischlingAnother_relativeOneOf4 {
  -- | hunts
  mischlingAnother_relativeOneOf4Hunts :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | pet_type
  , mischlingAnother_relativeOneOf4Pet_type :: (GHC.Maybe.Maybe GHC.Base.String)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON MischlingAnother_relativeOneOf4
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "hunts" (mischlingAnother_relativeOneOf4Hunts obj) : (Data.Aeson..=) "pet_type" (mischlingAnother_relativeOneOf4Pet_type obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "hunts" (mischlingAnother_relativeOneOf4Hunts obj) GHC.Base.<> (Data.Aeson..=) "pet_type" (mischlingAnother_relativeOneOf4Pet_type obj))
instance Data.Aeson.Types.FromJSON.FromJSON MischlingAnother_relativeOneOf4
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "MischlingAnother_relativeOneOf4" (\obj -> (GHC.Base.pure MischlingAnother_relativeOneOf4 GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "hunts")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "pet_type"))
-- | Define the one-of schema MischlingAnother_relative
-- 
-- 
data MischlingAnother_relativeVariants
    = MischlingAnother_relativeCat Cat
    | MischlingAnother_relativePetByType PetByType
    | MischlingAnother_relativeString GHC.Base.String
    | MischlingAnother_relativeMischlingAnother_relativeOneOf4 MischlingAnother_relativeOneOf4
    | MischlingAnother_relativeListString ([] GHC.Base.String)
    deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Generics.Generic)
instance Data.Aeson.ToJSON MischlingAnother_relativeVariants
    where toJSON = Data.Aeson.Types.ToJSON.genericToJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
instance Data.Aeson.FromJSON MischlingAnother_relativeVariants
    where parseJSON = Data.Aeson.Types.FromJSON.genericParseJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
-- | Defines the enum schema MischlingBreed
-- 
-- 
data MischlingBreed
    = MischlingBreedEnumOther Data.Aeson.Types.Internal.Value
    | MischlingBreedEnumTyped GHC.Base.String
    | MischlingBreedEnumString_Dingo
    | MischlingBreedEnumString_Husky
    | MischlingBreedEnumString_Retriever
    | MischlingBreedEnumString_Shepherd
    deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.ToJSON MischlingBreed
    where toJSON (MischlingBreedEnumOther patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (MischlingBreedEnumTyped patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (MischlingBreedEnumString_Dingo) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Dingo"
          toJSON (MischlingBreedEnumString_Husky) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Husky"
          toJSON (MischlingBreedEnumString_Retriever) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Retriever"
          toJSON (MischlingBreedEnumString_Shepherd) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Shepherd"
instance Data.Aeson.FromJSON MischlingBreed
    where parseJSON val = GHC.Base.pure (if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Dingo")
                                          then MischlingBreedEnumString_Dingo
                                          else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Husky")
                                                then MischlingBreedEnumString_Husky
                                                else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Retriever")
                                                      then MischlingBreedEnumString_Retriever
                                                      else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Shepherd")
                                                            then MischlingBreedEnumString_Shepherd
                                                            else MischlingBreedEnumOther val)
-- | Defines the data type for the schema MischlingFather
-- 
-- 
data MischlingFather = MischlingFather {
  
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON MischlingFather
    where toJSON obj = Data.Aeson.object []
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "string" ("string" :: GHC.Base.String))
instance Data.Aeson.Types.FromJSON.FromJSON MischlingFather
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "MischlingFather" (\obj -> GHC.Base.pure MischlingFather)
-- | Defines the data type for the schema MischlingFirst_relative
-- 
-- 
data MischlingFirst_relative = MischlingFirst_relative {
  -- | age
  mischlingFirst_relativeAge :: (GHC.Maybe.Maybe GHC.Integer.Type.Integer)
  -- | ananyoftype
  , mischlingFirst_relativeAnanyoftype :: (GHC.Maybe.Maybe MischlingFirst_relativeAnanyoftype)
  -- | another_relative
  , mischlingFirst_relativeAnother_relative :: (GHC.Maybe.Maybe MischlingFirst_relativeAnother_relativeVariants)
  -- | hunts
  , mischlingFirst_relativeHunts :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | pet_type
  , mischlingFirst_relativePet_type :: MischlingFirst_relativePet_type
  -- | relative
  , mischlingFirst_relativeRelative :: (GHC.Maybe.Maybe MischlingFirst_relativeRelativeVariants)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON MischlingFirst_relative
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "age" (mischlingFirst_relativeAge obj) : (Data.Aeson..=) "ananyoftype" (mischlingFirst_relativeAnanyoftype obj) : (Data.Aeson..=) "another_relative" (mischlingFirst_relativeAnother_relative obj) : (Data.Aeson..=) "hunts" (mischlingFirst_relativeHunts obj) : (Data.Aeson..=) "pet_type" (mischlingFirst_relativePet_type obj) : (Data.Aeson..=) "relative" (mischlingFirst_relativeRelative obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "age" (mischlingFirst_relativeAge obj) GHC.Base.<> ((Data.Aeson..=) "ananyoftype" (mischlingFirst_relativeAnanyoftype obj) GHC.Base.<> ((Data.Aeson..=) "another_relative" (mischlingFirst_relativeAnother_relative obj) GHC.Base.<> ((Data.Aeson..=) "hunts" (mischlingFirst_relativeHunts obj) GHC.Base.<> ((Data.Aeson..=) "pet_type" (mischlingFirst_relativePet_type obj) GHC.Base.<> (Data.Aeson..=) "relative" (mischlingFirst_relativeRelative obj))))))
instance Data.Aeson.Types.FromJSON.FromJSON MischlingFirst_relative
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "MischlingFirst_relative" (\obj -> (((((GHC.Base.pure MischlingFirst_relative GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "age")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "ananyoftype")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "another_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "hunts")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "pet_type")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "relative"))
-- | Defines the data type for the schema MischlingFirst_relativeAnanyoftype
-- 
-- 
data MischlingFirst_relativeAnanyoftype = MischlingFirst_relativeAnanyoftype {
  
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON MischlingFirst_relativeAnanyoftype
    where toJSON obj = Data.Aeson.object []
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "string" ("string" :: GHC.Base.String))
instance Data.Aeson.Types.FromJSON.FromJSON MischlingFirst_relativeAnanyoftype
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "MischlingFirst_relativeAnanyoftype" (\obj -> GHC.Base.pure MischlingFirst_relativeAnanyoftype)
-- | Define the one-of schema MischlingFirst_relativeAnother_relative
-- 
-- 
data MischlingFirst_relativeAnother_relativeVariants
    = MischlingFirst_relativeAnother_relativeCat Cat
    | MischlingFirst_relativeAnother_relativePetByType PetByType
    | MischlingFirst_relativeAnother_relativeString GHC.Base.String
    deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Generics.Generic)
instance Data.Aeson.ToJSON MischlingFirst_relativeAnother_relativeVariants
    where toJSON = Data.Aeson.Types.ToJSON.genericToJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
instance Data.Aeson.FromJSON MischlingFirst_relativeAnother_relativeVariants
    where parseJSON = Data.Aeson.Types.FromJSON.genericParseJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
-- | Defines the enum schema MischlingFirst_relativePet_type
-- 
-- 
data MischlingFirst_relativePet_type
    = MischlingFirst_relativePet_typeEnumOther Data.Aeson.Types.Internal.Value
    | MischlingFirst_relativePet_typeEnumTyped GHC.Base.String
    | MischlingFirst_relativePet_typeEnumString_Cat
    | MischlingFirst_relativePet_typeEnumString_Dog
    deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.ToJSON MischlingFirst_relativePet_type
    where toJSON (MischlingFirst_relativePet_typeEnumOther patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (MischlingFirst_relativePet_typeEnumTyped patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (MischlingFirst_relativePet_typeEnumString_Cat) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Cat"
          toJSON (MischlingFirst_relativePet_typeEnumString_Dog) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Dog"
instance Data.Aeson.FromJSON MischlingFirst_relativePet_type
    where parseJSON val = GHC.Base.pure (if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Cat")
                                          then MischlingFirst_relativePet_typeEnumString_Cat
                                          else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Dog")
                                                then MischlingFirst_relativePet_typeEnumString_Dog
                                                else MischlingFirst_relativePet_typeEnumOther val)
-- | Define the one-of schema MischlingFirst_relativeRelative
-- 
-- 
data MischlingFirst_relativeRelativeVariants
    = MischlingFirst_relativeRelativeCat Cat
    | MischlingFirst_relativeRelativePetByType PetByType
    | MischlingFirst_relativeRelativeString GHC.Base.String
    deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Generics.Generic)
instance Data.Aeson.ToJSON MischlingFirst_relativeRelativeVariants
    where toJSON = Data.Aeson.Types.ToJSON.genericToJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
instance Data.Aeson.FromJSON MischlingFirst_relativeRelativeVariants
    where parseJSON = Data.Aeson.Types.FromJSON.genericParseJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
-- | Defines the data type for the schema MischlingPet_type
-- 
-- 
data MischlingPet_type = MischlingPet_type {
  
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON MischlingPet_type
    where toJSON obj = Data.Aeson.object []
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "string" ("string" :: GHC.Base.String))
instance Data.Aeson.Types.FromJSON.FromJSON MischlingPet_type
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "MischlingPet_type" (\obj -> GHC.Base.pure MischlingPet_type)
-- | Define the one-of schema MischlingRelative
-- 
-- 
data MischlingRelativeVariants
    = MischlingRelativeCat Cat
    | MischlingRelativePetByType PetByType
    | MischlingRelativeString GHC.Base.String
    deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Generics.Generic)
instance Data.Aeson.ToJSON MischlingRelativeVariants
    where toJSON = Data.Aeson.Types.ToJSON.genericToJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
instance Data.Aeson.FromJSON MischlingRelativeVariants
    where parseJSON = Data.Aeson.Types.FromJSON.genericParseJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
-- | Defines the data type for the schema MischlingSecondFather
-- 
-- 
data MischlingSecondFather = MischlingSecondFather {
  
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON MischlingSecondFather
    where toJSON obj = Data.Aeson.object []
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "string" ("string" :: GHC.Base.String))
instance Data.Aeson.Types.FromJSON.FromJSON MischlingSecondFather
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "MischlingSecondFather" (\obj -> GHC.Base.pure MischlingSecondFather)
-- | Defines the data type for the schema PetByAge
-- 
-- 
data PetByAge = PetByAge {
  -- | age
  petByAgeAge :: GHC.Integer.Type.Integer
  -- | another_relative
  , petByAgeAnother_relative :: (GHC.Maybe.Maybe PetByAgeAnother_relativeVariants)
  -- | first_relative
  , petByAgeFirst_relative :: (GHC.Maybe.Maybe PetByAgeFirst_relative)
  -- | nickname
  , petByAgeNickname :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | relative
  , petByAgeRelative :: (GHC.Maybe.Maybe PetByAgeRelativeVariants)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON PetByAge
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "age" (petByAgeAge obj) : (Data.Aeson..=) "another_relative" (petByAgeAnother_relative obj) : (Data.Aeson..=) "first_relative" (petByAgeFirst_relative obj) : (Data.Aeson..=) "nickname" (petByAgeNickname obj) : (Data.Aeson..=) "relative" (petByAgeRelative obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "age" (petByAgeAge obj) GHC.Base.<> ((Data.Aeson..=) "another_relative" (petByAgeAnother_relative obj) GHC.Base.<> ((Data.Aeson..=) "first_relative" (petByAgeFirst_relative obj) GHC.Base.<> ((Data.Aeson..=) "nickname" (petByAgeNickname obj) GHC.Base.<> (Data.Aeson..=) "relative" (petByAgeRelative obj)))))
instance Data.Aeson.Types.FromJSON.FromJSON PetByAge
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "PetByAge" (\obj -> ((((GHC.Base.pure PetByAge GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "age")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "another_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "first_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "nickname")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "relative"))
-- | Defines the data type for the schema PetByAgeAnother_relativeOneOf4
-- 
-- 
data PetByAgeAnother_relativeOneOf4 = PetByAgeAnother_relativeOneOf4 {
  -- | hunts
  petByAgeAnother_relativeOneOf4Hunts :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | pet_type
  , petByAgeAnother_relativeOneOf4Pet_type :: (GHC.Maybe.Maybe GHC.Base.String)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON PetByAgeAnother_relativeOneOf4
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "hunts" (petByAgeAnother_relativeOneOf4Hunts obj) : (Data.Aeson..=) "pet_type" (petByAgeAnother_relativeOneOf4Pet_type obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "hunts" (petByAgeAnother_relativeOneOf4Hunts obj) GHC.Base.<> (Data.Aeson..=) "pet_type" (petByAgeAnother_relativeOneOf4Pet_type obj))
instance Data.Aeson.Types.FromJSON.FromJSON PetByAgeAnother_relativeOneOf4
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "PetByAgeAnother_relativeOneOf4" (\obj -> (GHC.Base.pure PetByAgeAnother_relativeOneOf4 GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "hunts")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "pet_type"))
-- | Define the one-of schema PetByAgeAnother_relative
-- 
-- 
data PetByAgeAnother_relativeVariants
    = PetByAgeAnother_relativeCat Cat
    | PetByAgeAnother_relativePetByType PetByType
    | PetByAgeAnother_relativeString GHC.Base.String
    | PetByAgeAnother_relativePetByAgeAnother_relativeOneOf4 PetByAgeAnother_relativeOneOf4
    | PetByAgeAnother_relativeListString ([] GHC.Base.String)
    deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Generics.Generic)
instance Data.Aeson.ToJSON PetByAgeAnother_relativeVariants
    where toJSON = Data.Aeson.Types.ToJSON.genericToJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
instance Data.Aeson.FromJSON PetByAgeAnother_relativeVariants
    where parseJSON = Data.Aeson.Types.FromJSON.genericParseJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
-- | Defines the data type for the schema PetByAgeFirst_relative
-- 
-- 
data PetByAgeFirst_relative = PetByAgeFirst_relative {
  -- | age
  petByAgeFirst_relativeAge :: (GHC.Maybe.Maybe GHC.Integer.Type.Integer)
  -- | ananyoftype
  , petByAgeFirst_relativeAnanyoftype :: (GHC.Maybe.Maybe PetByAgeFirst_relativeAnanyoftype)
  -- | another_relative
  , petByAgeFirst_relativeAnother_relative :: (GHC.Maybe.Maybe PetByAgeFirst_relativeAnother_relativeVariants)
  -- | hunts
  , petByAgeFirst_relativeHunts :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | pet_type
  , petByAgeFirst_relativePet_type :: PetByAgeFirst_relativePet_type
  -- | relative
  , petByAgeFirst_relativeRelative :: (GHC.Maybe.Maybe PetByAgeFirst_relativeRelativeVariants)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON PetByAgeFirst_relative
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "age" (petByAgeFirst_relativeAge obj) : (Data.Aeson..=) "ananyoftype" (petByAgeFirst_relativeAnanyoftype obj) : (Data.Aeson..=) "another_relative" (petByAgeFirst_relativeAnother_relative obj) : (Data.Aeson..=) "hunts" (petByAgeFirst_relativeHunts obj) : (Data.Aeson..=) "pet_type" (petByAgeFirst_relativePet_type obj) : (Data.Aeson..=) "relative" (petByAgeFirst_relativeRelative obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "age" (petByAgeFirst_relativeAge obj) GHC.Base.<> ((Data.Aeson..=) "ananyoftype" (petByAgeFirst_relativeAnanyoftype obj) GHC.Base.<> ((Data.Aeson..=) "another_relative" (petByAgeFirst_relativeAnother_relative obj) GHC.Base.<> ((Data.Aeson..=) "hunts" (petByAgeFirst_relativeHunts obj) GHC.Base.<> ((Data.Aeson..=) "pet_type" (petByAgeFirst_relativePet_type obj) GHC.Base.<> (Data.Aeson..=) "relative" (petByAgeFirst_relativeRelative obj))))))
instance Data.Aeson.Types.FromJSON.FromJSON PetByAgeFirst_relative
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "PetByAgeFirst_relative" (\obj -> (((((GHC.Base.pure PetByAgeFirst_relative GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "age")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "ananyoftype")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "another_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "hunts")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "pet_type")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "relative"))
-- | Defines the data type for the schema PetByAgeFirst_relativeAnanyoftype
-- 
-- 
data PetByAgeFirst_relativeAnanyoftype = PetByAgeFirst_relativeAnanyoftype {
  
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON PetByAgeFirst_relativeAnanyoftype
    where toJSON obj = Data.Aeson.object []
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "string" ("string" :: GHC.Base.String))
instance Data.Aeson.Types.FromJSON.FromJSON PetByAgeFirst_relativeAnanyoftype
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "PetByAgeFirst_relativeAnanyoftype" (\obj -> GHC.Base.pure PetByAgeFirst_relativeAnanyoftype)
-- | Define the one-of schema PetByAgeFirst_relativeAnother_relative
-- 
-- 
data PetByAgeFirst_relativeAnother_relativeVariants
    = PetByAgeFirst_relativeAnother_relativeCat Cat
    | PetByAgeFirst_relativeAnother_relativePetByType PetByType
    | PetByAgeFirst_relativeAnother_relativeString GHC.Base.String
    deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Generics.Generic)
instance Data.Aeson.ToJSON PetByAgeFirst_relativeAnother_relativeVariants
    where toJSON = Data.Aeson.Types.ToJSON.genericToJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
instance Data.Aeson.FromJSON PetByAgeFirst_relativeAnother_relativeVariants
    where parseJSON = Data.Aeson.Types.FromJSON.genericParseJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
-- | Defines the enum schema PetByAgeFirst_relativePet_type
-- 
-- 
data PetByAgeFirst_relativePet_type
    = PetByAgeFirst_relativePet_typeEnumOther Data.Aeson.Types.Internal.Value
    | PetByAgeFirst_relativePet_typeEnumTyped GHC.Base.String
    | PetByAgeFirst_relativePet_typeEnumString_Cat
    | PetByAgeFirst_relativePet_typeEnumString_Dog
    deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.ToJSON PetByAgeFirst_relativePet_type
    where toJSON (PetByAgeFirst_relativePet_typeEnumOther patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (PetByAgeFirst_relativePet_typeEnumTyped patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (PetByAgeFirst_relativePet_typeEnumString_Cat) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Cat"
          toJSON (PetByAgeFirst_relativePet_typeEnumString_Dog) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Dog"
instance Data.Aeson.FromJSON PetByAgeFirst_relativePet_type
    where parseJSON val = GHC.Base.pure (if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Cat")
                                          then PetByAgeFirst_relativePet_typeEnumString_Cat
                                          else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Dog")
                                                then PetByAgeFirst_relativePet_typeEnumString_Dog
                                                else PetByAgeFirst_relativePet_typeEnumOther val)
-- | Define the one-of schema PetByAgeFirst_relativeRelative
-- 
-- 
data PetByAgeFirst_relativeRelativeVariants
    = PetByAgeFirst_relativeRelativeCat Cat
    | PetByAgeFirst_relativeRelativePetByType PetByType
    | PetByAgeFirst_relativeRelativeString GHC.Base.String
    deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Generics.Generic)
instance Data.Aeson.ToJSON PetByAgeFirst_relativeRelativeVariants
    where toJSON = Data.Aeson.Types.ToJSON.genericToJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
instance Data.Aeson.FromJSON PetByAgeFirst_relativeRelativeVariants
    where parseJSON = Data.Aeson.Types.FromJSON.genericParseJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
-- | Define the one-of schema PetByAgeRelative
-- 
-- 
data PetByAgeRelativeVariants
    = PetByAgeRelativeCat Cat
    | PetByAgeRelativePetByType PetByType
    | PetByAgeRelativeString GHC.Base.String
    deriving (GHC.Show.Show, GHC.Classes.Eq, GHC.Generics.Generic)
instance Data.Aeson.ToJSON PetByAgeRelativeVariants
    where toJSON = Data.Aeson.Types.ToJSON.genericToJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}
instance Data.Aeson.FromJSON PetByAgeRelativeVariants
    where parseJSON = Data.Aeson.Types.FromJSON.genericParseJSON Data.Aeson.Types.Internal.defaultOptions{Data.Aeson.Types.Internal.sumEncoding = Data.Aeson.Types.Internal.UntaggedValue}