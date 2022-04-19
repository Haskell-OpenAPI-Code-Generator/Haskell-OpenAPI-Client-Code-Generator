module OpenAPI.Types.Order where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified OpenAPI.Common
data Order
instance Show Order
instance Eq Order
instance FromJSON Order
instance ToJSON Order
data OrderStatus
instance Show OrderStatus
instance Eq OrderStatus
instance FromJSON OrderStatus
instance ToJSON OrderStatus
