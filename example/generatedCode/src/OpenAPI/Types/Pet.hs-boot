module OpenAPI.Types.Pet where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Pet
instance Show Pet
instance Eq Pet
instance Data.Aeson.FromJSON Pet
instance Data.Aeson.ToJSON Pet
data PetStatus
instance Show PetStatus
instance Eq PetStatus
instance Data.Aeson.FromJSON PetStatus
instance Data.Aeson.ToJSON PetStatus
