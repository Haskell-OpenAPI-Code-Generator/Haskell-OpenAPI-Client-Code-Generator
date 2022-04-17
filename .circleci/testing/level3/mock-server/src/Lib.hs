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
import Data.Aeson.TH
import Data.Char
import qualified Data.HashMap.Strict as HM
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
    :<|> "nullable-optional" :> Capture "mode" String :> ReqBody '[JSON] Value :> Post '[JSON] Value

startApp :: IO ()
startApp = run 8887 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = pure getInventory :<|> findByStatus :<|> addPet :<|> userAgentEcho :<|> checkNullableAndOptional

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

checkNullableAndOptional :: String -> Value -> Handler Value
checkNullableAndOptional "filled" (Object map) = do
  when (HM.lookup "requiredNonNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "requiredNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "optionalNonNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "optionalNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "referencedRequiredNonNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "referencedRequiredNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "referencedOptionalNonNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "referencedOptionalNullable" map /= Just "x") $ throwError err400
  pure $
    Object $
      HM.fromList
        [ ("requiredNonNullable", "x"),
          ("requiredNullable", "x"),
          ("optionalNonNullable", "x"),
          ("optionalNullable", "x"),
          ("referencedRequiredNonNullable", "x"),
          ("referencedRequiredNullable", "x"),
          ("referencedOptionalNonNullable", "x"),
          ("referencedOptionalNullable", "x")
        ]
checkNullableAndOptional "emptyNull" (Object map) = do
  when (HM.lookup "requiredNonNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "requiredNullable" map /= Just Null) $ throwError err400
  when (HM.lookup "optionalNonNullable" map /= Nothing) $ throwError err400
  when (HM.lookup "optionalNullable" map /= Just Null) $ throwError err400
  when (HM.lookup "referencedRequiredNonNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "referencedRequiredNullable" map /= Just Null) $ throwError err400
  when (HM.lookup "referencedOptionalNonNullable" map /= Nothing) $ throwError err400
  when (HM.lookup "referencedOptionalNullable" map /= Just Null) $ throwError err400
  pure $
    Object $
      HM.fromList
        [ ("requiredNonNullable", "x"),
          ("requiredNullable", Null),
          ("optionalNullable", Null),
          ("referencedRequiredNonNullable", "x"),
          ("referencedRequiredNullable", Null),
          ("referencedOptionalNullable", Null)
        ]
checkNullableAndOptional "emptyAbsent" (Object map) = do
  when (HM.lookup "requiredNonNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "requiredNullable" map /= Just Null) $ throwError err400
  when (HM.lookup "optionalNonNullable" map /= Nothing) $ throwError err400
  when (HM.lookup "optionalNullable" map /= Nothing) $ throwError err400
  when (HM.lookup "referencedRequiredNonNullable" map /= Just "x") $ throwError err400
  when (HM.lookup "referencedRequiredNullable" map /= Just Null) $ throwError err400
  when (HM.lookup "referencedOptionalNonNullable" map /= Nothing) $ throwError err400
  when (HM.lookup "referencedOptionalNullable" map /= Nothing) $ throwError err400
  pure $
    Object $
      HM.fromList
        [ ("requiredNonNullable", "x"),
          ("requiredNullable", Null),
          ("referencedRequiredNonNullable", "x"),
          ("referencedRequiredNullable", Null)
        ]
checkNullableAndOptional "errorRequiredNonNullableWithNull" (Object map) =
  pure $
    Object $
      HM.fromList
        [ ("requiredNonNullable", Null),
          ("requiredNullable", "x"),
          ("optionalNonNullable", "x"),
          ("optionalNullable", "x"),
          ("referencedRequiredNonNullable", "x"),
          ("referencedRequiredNullable", "x"),
          ("referencedOptionalNonNullable", "x"),
          ("referencedOptionalNullable", "x")
        ]
checkNullableAndOptional "errorRequiredNonNullableWithAbsence" (Object map) =
  pure $
    Object $
      HM.fromList
        [ ("requiredNullable", "x"),
          ("optionalNonNullable", "x"),
          ("optionalNullable", "x"),
          ("referencedRequiredNonNullable", "x"),
          ("referencedRequiredNullable", "x"),
          ("referencedOptionalNonNullable", "x"),
          ("referencedOptionalNullable", "x")
        ]
checkNullableAndOptional "errorRequiredNullable" (Object map) =
  pure $
    Object $
      HM.fromList
        [ ("requiredNonNullable", "x"),
          ("optionalNonNullable", "x"),
          ("optionalNullable", "x"),
          ("referencedRequiredNonNullable", "x"),
          ("referencedRequiredNullable", "x"),
          ("referencedOptionalNonNullable", "x"),
          ("referencedOptionalNullable", "x")
        ]
checkNullableAndOptional "errorOptionalNonNullable" (Object map) =
  pure $
    Object $
      HM.fromList
        [ ("requiredNonNullable", "x"),
          ("requiredNullable", "x"),
          ("optionalNonNullable", Null),
          ("optionalNullable", "x"),
          ("referencedRequiredNonNullable", "x"),
          ("referencedRequiredNullable", "x"),
          ("referencedOptionalNonNullable", "x"),
          ("referencedOptionalNullable", "x")
        ]
checkNullableAndOptional _ _ = throwError err500
