module OpenAPI.Types.Value where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Value
instance Show Value
instance Eq Value
instance Data.Aeson.FromJSON Value
instance Data.Aeson.ToJSON Value
