module OpenAPI.Types.Shark where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Shark
instance Show Shark
instance Eq Shark
instance Data.Aeson.FromJSON Shark
instance Data.Aeson.ToJSON Shark
