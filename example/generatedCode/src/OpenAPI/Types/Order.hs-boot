module OpenAPI.Types.Order where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Order
instance Show Order
instance Eq Order
instance Data.Aeson.FromJSON Order
instance Data.Aeson.ToJSON Order
data OrderStatus
instance Show OrderStatus
instance Eq OrderStatus
instance Data.Aeson.FromJSON OrderStatus
instance Data.Aeson.ToJSON OrderStatus
