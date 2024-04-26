module OpenAPI.Types.Dog where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Dog
instance Show Dog
instance Eq Dog
instance Data.Aeson.FromJSON Dog
instance Data.Aeson.ToJSON Dog
data DogBreed
instance Show DogBreed
instance Eq DogBreed
instance Data.Aeson.FromJSON DogBreed
instance Data.Aeson.ToJSON DogBreed
