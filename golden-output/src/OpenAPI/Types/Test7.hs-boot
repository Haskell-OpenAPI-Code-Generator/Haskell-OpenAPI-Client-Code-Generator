module OpenAPI.Types.Test7 where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified OpenAPI.Common
data Test7Item
instance Show Test7Item
instance Eq Test7Item
instance FromJSON Test7Item
instance ToJSON Test7Item
type Test7 = [Test7Item]
