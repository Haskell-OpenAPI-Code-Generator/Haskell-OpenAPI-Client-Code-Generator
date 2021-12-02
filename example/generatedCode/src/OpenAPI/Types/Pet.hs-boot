module OpenAPI.Types.Pet where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
data Pet
instance Show Pet
instance Eq Pet
instance FromJSON Pet
instance ToJSON Pet
data PetStatus
instance Show PetStatus
instance Eq PetStatus
instance FromJSON PetStatus
instance ToJSON PetStatus
