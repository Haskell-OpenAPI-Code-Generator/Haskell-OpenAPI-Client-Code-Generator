module OpenAPI.Types.ApiResponse where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
data ApiResponse
instance Show ApiResponse
instance Eq ApiResponse
instance FromJSON ApiResponse
instance ToJSON ApiResponse
