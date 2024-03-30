module OpenAPI.Types.User where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified OpenAPI.Common
data User
instance Show User
instance Eq User
instance FromJSON User
instance ToJSON User
