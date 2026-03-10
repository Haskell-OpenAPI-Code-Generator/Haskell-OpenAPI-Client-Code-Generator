module OpenAPI.Types.Guppie where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Guppie
instance Show Guppie
instance Eq Guppie
instance Data.Aeson.FromJSON Guppie
instance Data.Aeson.ToJSON Guppie
