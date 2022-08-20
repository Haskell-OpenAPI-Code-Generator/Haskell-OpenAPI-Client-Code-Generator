module OpenAPI.Generate.OptParse.Types
  ( FixedValueStrategy (..),
  )
where

import Autodocodec

data FixedValueStrategy = FixedValueStrategyExclude | FixedValueStrategyInclude
  deriving (Eq, Bounded, Enum)

instance Show FixedValueStrategy where
  show FixedValueStrategyExclude = "exclude"
  show FixedValueStrategyInclude = "include"

instance Read FixedValueStrategy where
  readsPrec _ ('e' : 'x' : 'c' : 'l' : 'u' : 'd' : 'e' : rest) = [(FixedValueStrategyExclude, rest)]
  readsPrec _ ('i' : 'n' : 'c' : 'l' : 'u' : 'd' : 'e' : rest) = [(FixedValueStrategyInclude, rest)]
  readsPrec _ _ = []

instance HasCodec FixedValueStrategy where
  codec = shownBoundedEnumCodec
