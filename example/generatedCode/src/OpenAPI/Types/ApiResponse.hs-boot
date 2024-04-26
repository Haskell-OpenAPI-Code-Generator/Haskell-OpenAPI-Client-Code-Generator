module OpenAPI.Types.ApiResponse where
import qualified Data.Aeson
import qualified OpenAPI.Common
data ApiResponse
instance Show ApiResponse
instance Eq ApiResponse
instance Data.Aeson.FromJSON ApiResponse
instance Data.Aeson.ToJSON ApiResponse
