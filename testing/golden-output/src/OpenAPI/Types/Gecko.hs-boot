module OpenAPI.Types.Gecko where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Gecko
instance Show Gecko
instance Eq Gecko
instance Data.Aeson.FromJSON Gecko
instance Data.Aeson.ToJSON Gecko
