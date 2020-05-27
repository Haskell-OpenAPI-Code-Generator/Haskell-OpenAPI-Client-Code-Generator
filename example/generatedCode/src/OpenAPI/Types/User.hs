{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the types generated from the schema User
module OpenAPI.Types.User where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Char8 as Data.ByteString.Internal
import qualified Data.Functor
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Generics
import qualified GHC.Int
import qualified GHC.Show
import qualified GHC.Types
import qualified OpenAPI.Common

-- | Defines the data type for the schema User
-- 
-- 
data User = User {
  -- | email
  userEmail :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | firstName
  , userFirstName :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | id
  , userId :: (GHC.Maybe.Maybe GHC.Int.Int64)
  -- | lastName
  , userLastName :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | password
  , userPassword :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | phone
  , userPhone :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | userStatus: User Status
  , userUserStatus :: (GHC.Maybe.Maybe GHC.Int.Int32)
  -- | username
  , userUsername :: (GHC.Maybe.Maybe GHC.Base.String)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON User
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "email" (userEmail obj) : (Data.Aeson..=) "firstName" (userFirstName obj) : (Data.Aeson..=) "id" (userId obj) : (Data.Aeson..=) "lastName" (userLastName obj) : (Data.Aeson..=) "password" (userPassword obj) : (Data.Aeson..=) "phone" (userPhone obj) : (Data.Aeson..=) "userStatus" (userUserStatus obj) : (Data.Aeson..=) "username" (userUsername obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "email" (userEmail obj) GHC.Base.<> ((Data.Aeson..=) "firstName" (userFirstName obj) GHC.Base.<> ((Data.Aeson..=) "id" (userId obj) GHC.Base.<> ((Data.Aeson..=) "lastName" (userLastName obj) GHC.Base.<> ((Data.Aeson..=) "password" (userPassword obj) GHC.Base.<> ((Data.Aeson..=) "phone" (userPhone obj) GHC.Base.<> ((Data.Aeson..=) "userStatus" (userUserStatus obj) GHC.Base.<> (Data.Aeson..=) "username" (userUsername obj))))))))
instance Data.Aeson.Types.FromJSON.FromJSON User
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "User" (\obj -> (((((((GHC.Base.pure User GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "email")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "firstName")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "lastName")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "password")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "phone")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "userStatus")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "username"))