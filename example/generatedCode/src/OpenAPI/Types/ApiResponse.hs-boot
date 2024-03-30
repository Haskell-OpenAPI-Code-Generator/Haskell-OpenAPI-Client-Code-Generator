module OpenAPI.Types.ApiResponse where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified OpenAPI.Common
data ApiResponse
instance Show ApiResponse
instance Eq ApiResponse
instance FromJSON ApiResponse
instance ToJSON ApiResponse
