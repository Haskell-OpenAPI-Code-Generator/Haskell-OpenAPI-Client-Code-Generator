{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.OptParse.Types
  ( FixedValueStrategy (..),
    ModuleName,
    ModulePathInfo,
    mkModuleName,
    getModuleName,
    mkModulePathInfo,
    getModuleInfoPath,
    getModuleInfoDir,
  )
where

import Autodocodec
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import System.FilePath ((</>))

newtype ModuleName = ModuleName String
  deriving (Show, Eq)

newtype ModulePathInfo = ModulePathInfo (Maybe FilePath, FilePath)

mkModuleName :: String -> ModuleName
mkModuleName = ModuleName

getModuleName :: ModuleName -> String
getModuleName (ModuleName moduleNameStr) = moduleNameStr

mkModulePathInfo :: ModuleName -> ModulePathInfo
mkModulePathInfo (ModuleName moduleNameStr) =
  let (dirMay, fileNoExtensionMay) =
        foldl
          ( \acc next ->
              let dMay = case acc of
                    (Nothing, Just prev) -> Just prev
                    (Just dir, Just prev) -> Just $ dir </> prev
                    (dMay', Nothing) -> dMay'
               in (dMay, Just $ T.unpack next)
          )
          (Nothing, Nothing)
          . T.splitOn "."
          . T.pack
          $ moduleNameStr
   in ModulePathInfo (dirMay, Maybe.fromMaybe moduleNameStr fileNoExtensionMay)

getModuleInfoPath :: ModulePathInfo -> Maybe String -> String -> FilePath
getModuleInfoPath (ModulePathInfo (dirMay, fileNoExtension)) suffixMay extension =
  let file = case suffixMay of
        Nothing -> fileNoExtension <> extension
        Just suffix -> fileNoExtension </> suffix <> extension
   in maybe file (</> file) dirMay

getModuleInfoDir :: ModulePathInfo -> FilePath
getModuleInfoDir (ModulePathInfo (dirMay, fileNoExtension)) =
  maybe fileNoExtension (</> fileNoExtension) dirMay

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
