{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Data.Aeson
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

startApp :: IO ()
startApp = run 8887 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = pure getInventory :<|> findByStatus :<|> addPet :<|> userAgentEcho

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
