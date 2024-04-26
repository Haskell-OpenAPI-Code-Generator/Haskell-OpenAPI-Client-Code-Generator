module OpenAPI.Types.Category where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Category
instance Show Category
instance Eq Category
instance Data.Aeson.FromJSON Category
instance Data.Aeson.ToJSON Category
