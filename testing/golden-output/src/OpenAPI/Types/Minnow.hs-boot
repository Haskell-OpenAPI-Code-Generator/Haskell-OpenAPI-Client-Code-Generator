module OpenAPI.Types.Minnow where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Minnow
instance Show Minnow
instance Eq Minnow
instance Data.Aeson.FromJSON Minnow
instance Data.Aeson.ToJSON Minnow
