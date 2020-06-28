module OpenAPI.Types.Dog where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
data Dog 
instance Show Dog 
instance Eq Dog 
instance FromJSON Dog 
instance ToJSON Dog 
data DogBreed
instance Show DogBreed
instance Eq DogBreed
instance FromJSON DogBreed
instance ToJSON DogBreed
