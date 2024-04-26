module OpenAPI.Types.Test8 where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Test8NonNullable
instance Show Test8NonNullable
instance Eq Test8NonNullable
instance Data.Aeson.FromJSON Test8NonNullable
instance Data.Aeson.ToJSON Test8NonNullable
type Test8 = OpenAPI.Common.Nullable Test8NonNullable
