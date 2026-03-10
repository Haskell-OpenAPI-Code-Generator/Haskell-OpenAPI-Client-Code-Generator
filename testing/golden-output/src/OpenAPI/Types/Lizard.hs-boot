module OpenAPI.Types.Lizard where
import qualified Data.Aeson
import qualified OpenAPI.Common
data LizardVariants
instance Show LizardVariants
instance Eq LizardVariants
instance Data.Aeson.FromJSON LizardVariants
instance Data.Aeson.ToJSON LizardVariants
type Lizard = LizardVariants
