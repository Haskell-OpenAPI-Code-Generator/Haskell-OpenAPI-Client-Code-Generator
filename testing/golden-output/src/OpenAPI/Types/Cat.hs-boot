module OpenAPI.Types.Cat where
import qualified Data.Aeson
import qualified OpenAPI.Common
data Cat
instance Show Cat
instance Eq Cat
instance Data.Aeson.FromJSON Cat
instance Data.Aeson.ToJSON Cat
data CatAnother_relativeVariants
instance Show CatAnother_relativeVariants
instance Eq CatAnother_relativeVariants
instance Data.Aeson.FromJSON CatAnother_relativeVariants
instance Data.Aeson.ToJSON CatAnother_relativeVariants
data CatRelativeVariants
instance Show CatRelativeVariants
instance Eq CatRelativeVariants
instance Data.Aeson.FromJSON CatRelativeVariants
instance Data.Aeson.ToJSON CatRelativeVariants
