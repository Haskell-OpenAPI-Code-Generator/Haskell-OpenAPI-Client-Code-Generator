{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Control.Monad (when)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.TH
import Data.Char
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Pet = Pet
  { petId :: Int,
    petName :: String,
    petStatus :: String,
    petPhotoUrls :: [String]
  }
  deriving (Show, Eq)

$(deriveJSON (defaultOptions {fieldLabelModifier = (\(x : xs) -> toLower x : xs) . drop 3}) ''Pet)

type API =
  "store" :> "inventory" :> Get '[JSON] Value
    :<|> "pet" :> "findByStatus" :> QueryParam "status" String :> Get '[JSON] [Pet]
    :<|> "pet" :> ReqBody '[JSON] Pet :> Post '[JSON] NoContent
    :<|> "echo" :> Header "User-Agent" String :> Get '[JSON] String
    :<|> "echo" :> Capture "path" String :> Get '[JSON] String
    :<|> "nullable-optional" :> Capture "mode" String :> ReqBody '[JSON] Value :> Post '[JSON] Value

startApp :: IO ()
startApp = run 8887 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = pure getInventory :<|> findByStatus :<|> addPet :<|> userAgentEcho :<|> pathEcho :<|> checkNullableAndOptional

getInventory :: Value
getInventory = object ["pet1" .= Number 23, "pet2" .= Number 2]

findByStatus :: Maybe String -> Handler [Pet]
findByStatus (Just "pending") =
  pure
    [ Pet
        { petId = 23,
          petName = "Ted",
          petStatus = "pending",
          petPhotoUrls = []
        }
    ]
findByStatus _ = throwError err400

addPet :: Pet -> Handler NoContent
addPet Pet {petId = 21, petName = "Harro", petStatus = "available", petPhotoUrls = []} = pure NoContent
addPet _ = throwError err405

userAgentEcho :: Maybe String -> Handler String
userAgentEcho (Just userAgent) = pure userAgent
userAgentEcho Nothing = throwError err400

pathEcho :: String -> Handler String
pathEcho = pure

checkNullableAndOptional :: String -> Value -> Handler Value
checkNullableAndOptional "filled" (Object map) = do
  when (KeyMap.lookup (Key.fromText "requiredNonNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "requiredNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "optionalNonNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "optionalNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedRequiredNonNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedRequiredNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedOptionalNonNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedOptionalNullable") map /= Just "x") $ throwError err400
  pure $
    Object $
      KeyMap.fromList
        [ (Key.fromText "requiredNonNullable", "x"),
          (Key.fromText "requiredNullable", "x"),
          (Key.fromText "optionalNonNullable", "x"),
          (Key.fromText "optionalNullable", "x"),
          (Key.fromText "referencedRequiredNonNullable", "x"),
          (Key.fromText "referencedRequiredNullable", "x"),
          (Key.fromText "referencedOptionalNonNullable", "x"),
          (Key.fromText "referencedOptionalNullable", "x")
        ]
checkNullableAndOptional "emptyNull" (Object map) = do
  when (KeyMap.lookup (Key.fromText "requiredNonNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "requiredNullable") map /= Just Null) $ throwError err400
  when (KeyMap.lookup (Key.fromText "optionalNonNullable") map /= Nothing) $ throwError err400
  when (KeyMap.lookup (Key.fromText "optionalNullable") map /= Just Null) $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedRequiredNonNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedRequiredNullable") map /= Just Null) $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedOptionalNonNullable") map /= Nothing) $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedOptionalNullable") map /= Just Null) $ throwError err400
  pure $
    Object $
      KeyMap.fromList
        [ (Key.fromText "requiredNonNullable", "x"),
          (Key.fromText "requiredNullable", Null),
          (Key.fromText "optionalNullable", Null),
          (Key.fromText "referencedRequiredNonNullable", "x"),
          (Key.fromText "referencedRequiredNullable", Null),
          (Key.fromText "referencedOptionalNullable", Null)
        ]
checkNullableAndOptional "emptyAbsent" (Object map) = do
  when (KeyMap.lookup (Key.fromText "requiredNonNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "requiredNullable") map /= Just Null) $ throwError err400
  when (KeyMap.lookup (Key.fromText "optionalNonNullable") map /= Nothing) $ throwError err400
  when (KeyMap.lookup (Key.fromText "optionalNullable") map /= Nothing) $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedRequiredNonNullable") map /= Just "x") $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedRequiredNullable") map /= Just Null) $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedOptionalNonNullable") map /= Nothing) $ throwError err400
  when (KeyMap.lookup (Key.fromText "referencedOptionalNullable") map /= Nothing) $ throwError err400
  pure $
    Object $
      KeyMap.fromList
        [ (Key.fromText "requiredNonNullable", "x"),
          (Key.fromText "requiredNullable", Null),
          (Key.fromText "referencedRequiredNonNullable", "x"),
          (Key.fromText "referencedRequiredNullable", Null)
        ]
checkNullableAndOptional "errorRequiredNonNullableWithNull" (Object map) =
  pure $
    Object $
      KeyMap.fromList
        [ (Key.fromText "requiredNonNullable", Null),
          (Key.fromText "requiredNullable", "x"),
          (Key.fromText "optionalNonNullable", "x"),
          (Key.fromText "optionalNullable", "x"),
          (Key.fromText "referencedRequiredNonNullable", "x"),
          (Key.fromText "referencedRequiredNullable", "x"),
          (Key.fromText "referencedOptionalNonNullable", "x"),
          (Key.fromText "referencedOptionalNullable", "x")
        ]
checkNullableAndOptional "errorRequiredNonNullableWithAbsence" (Object map) =
  pure $
    Object $
      KeyMap.fromList
        [ (Key.fromText "requiredNullable", "x"),
          (Key.fromText "optionalNonNullable", "x"),
          (Key.fromText "optionalNullable", "x"),
          (Key.fromText "referencedRequiredNonNullable", "x"),
          (Key.fromText "referencedRequiredNullable", "x"),
          (Key.fromText "referencedOptionalNonNullable", "x"),
          (Key.fromText "referencedOptionalNullable", "x")
        ]
checkNullableAndOptional "errorRequiredNullable" (Object map) =
  pure $
    Object $
      KeyMap.fromList
        [ (Key.fromText "requiredNonNullable", "x"),
          (Key.fromText "optionalNonNullable", "x"),
          (Key.fromText "optionalNullable", "x"),
          (Key.fromText "referencedRequiredNonNullable", "x"),
          (Key.fromText "referencedRequiredNullable", "x"),
          (Key.fromText "referencedOptionalNonNullable", "x"),
          (Key.fromText "referencedOptionalNullable", "x")
        ]
checkNullableAndOptional "errorOptionalNonNullable" (Object map) =
  pure $
    Object $
      KeyMap.fromList
        [ (Key.fromText "requiredNonNullable", "x"),
          (Key.fromText "requiredNullable", "x"),
          (Key.fromText "optionalNonNullable", Null),
          (Key.fromText "optionalNullable", "x"),
          (Key.fromText "referencedRequiredNonNullable", "x"),
          (Key.fromText "referencedRequiredNullable", "x"),
          (Key.fromText "referencedOptionalNonNullable", "x"),
          (Key.fromText "referencedOptionalNullable", "x")
        ]
checkNullableAndOptional _ _ = throwError err500
