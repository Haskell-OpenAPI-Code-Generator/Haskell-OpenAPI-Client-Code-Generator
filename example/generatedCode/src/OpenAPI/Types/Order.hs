{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains the types generated from the schema Order
module OpenAPI.Types.Order where

import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
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
import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe

-- | Defines the data type for the schema Order
data Order = Order
  { -- | complete
    orderComplete :: (GHC.Maybe.Maybe GHC.Types.Bool),
    -- | id
    orderId :: (GHC.Maybe.Maybe GHC.Int.Int64),
    -- | petId
    orderPetId :: (GHC.Maybe.Maybe GHC.Int.Int64),
    -- | quantity
    orderQuantity :: (GHC.Maybe.Maybe GHC.Int.Int32),
    -- | shipDate
    orderShipDate :: (GHC.Maybe.Maybe GHC.Base.String),
    -- | status: Order Status
    orderStatus :: (GHC.Maybe.Maybe OrderStatus)
  }
  deriving
    ( GHC.Show.Show,
      GHC.Classes.Eq
    )

instance Data.Aeson.ToJSON Order where
  toJSON obj = Data.Aeson.object ((Data.Aeson..=) "complete" (orderComplete obj) : (Data.Aeson..=) "id" (orderId obj) : (Data.Aeson..=) "petId" (orderPetId obj) : (Data.Aeson..=) "quantity" (orderQuantity obj) : (Data.Aeson..=) "shipDate" (orderShipDate obj) : (Data.Aeson..=) "status" (orderStatus obj) : [])
  toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "complete" (orderComplete obj) GHC.Base.<> ((Data.Aeson..=) "id" (orderId obj) GHC.Base.<> ((Data.Aeson..=) "petId" (orderPetId obj) GHC.Base.<> ((Data.Aeson..=) "quantity" (orderQuantity obj) GHC.Base.<> ((Data.Aeson..=) "shipDate" (orderShipDate obj) GHC.Base.<> (Data.Aeson..=) "status" (orderStatus obj))))))

instance Data.Aeson.Types.FromJSON.FromJSON Order where
  parseJSON = Data.Aeson.Types.FromJSON.withObject "Order" (\obj -> (((((GHC.Base.pure Order GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "complete")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "petId")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "quantity")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "shipDate")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "status"))

-- | Defines the enum schema OrderStatus
--
-- Order Status
data OrderStatus
  = OrderStatusEnumOther Data.Aeson.Types.Internal.Value
  | OrderStatusEnumTyped GHC.Base.String
  | OrderStatusEnumString_approved
  | OrderStatusEnumString_delivered
  | OrderStatusEnumString_placed
  deriving (GHC.Show.Show, GHC.Classes.Eq)

instance Data.Aeson.ToJSON OrderStatus where
  toJSON (OrderStatusEnumOther patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
  toJSON (OrderStatusEnumTyped patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
  toJSON (OrderStatusEnumString_approved) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "approved"
  toJSON (OrderStatusEnumString_delivered) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "delivered"
  toJSON (OrderStatusEnumString_placed) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "placed"

instance Data.Aeson.FromJSON OrderStatus where
  parseJSON val =
    GHC.Base.pure
      ( if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "approved")
          then OrderStatusEnumString_approved
          else
            if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "delivered")
              then OrderStatusEnumString_delivered
              else
                if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "placed")
                  then OrderStatusEnumString_placed
                  else OrderStatusEnumOther val
      )
