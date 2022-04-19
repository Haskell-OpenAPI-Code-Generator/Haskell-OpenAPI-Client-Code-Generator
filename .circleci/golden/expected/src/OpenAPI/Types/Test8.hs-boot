module OpenAPI.Types.Test8 where
import Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified OpenAPI.Common
data Test8NonNullable
instance Show Test8NonNullable
instance Eq Test8NonNullable
instance FromJSON Test8NonNullable
instance ToJSON Test8NonNullable
type Test8 = OpenAPI.Common.Nullable Test8NonNullable
