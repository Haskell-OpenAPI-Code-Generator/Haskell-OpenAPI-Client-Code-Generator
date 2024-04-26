module OpenAPI.Types.Tag where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Tag
instance Show Tag
instance Eq Tag
instance Data.Aeson.FromJSON Tag
instance Data.Aeson.ToJSON Tag
