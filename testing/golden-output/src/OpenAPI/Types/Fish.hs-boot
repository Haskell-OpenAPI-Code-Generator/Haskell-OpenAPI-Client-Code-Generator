module OpenAPI.Types.Fish where
import qualified Data.Aeson
import qualified OpenAPI.Common
data FishVariants
instance Show FishVariants
instance Eq FishVariants
instance Data.Aeson.FromJSON FishVariants
instance Data.Aeson.ToJSON FishVariants
type Fish = FishVariants
