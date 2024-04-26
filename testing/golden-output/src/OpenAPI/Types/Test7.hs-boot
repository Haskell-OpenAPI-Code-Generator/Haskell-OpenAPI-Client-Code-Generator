module OpenAPI.Types.Test7 where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Test7Item
instance Show Test7Item
instance Eq Test7Item
instance Data.Aeson.FromJSON Test7Item
instance Data.Aeson.ToJSON Test7Item
type Test7 = [Test7Item]
