{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString.Char8
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common

lineItemPeriod = InvoiceLineItemPeriod 10 80

checkoutLineItem =
  (mkPostCheckoutSessionsRequestBodyLineItems' 4)
    { postCheckoutSessionsRequestBodyLineItems'Amount = Just 1000,
      postCheckoutSessionsRequestBodyLineItems'Currency = Just "CHF",
      postCheckoutSessionsRequestBodyLineItems'Description = Just "algebraic data types",
      postCheckoutSessionsRequestBodyLineItems'Name = Just "static types"
    }

checkoutLineItem2 =
  (mkPostCheckoutSessionsRequestBodyLineItems' 2)
    { postCheckoutSessionsRequestBodyLineItems'Amount = Just 5000,
      postCheckoutSessionsRequestBodyLineItems'Currency = Just "CHF",
      postCheckoutSessionsRequestBodyLineItems'Description = Just "lambda calculus",
      postCheckoutSessionsRequestBodyLineItems'Name = Just "Haskell"
    }

checkoutSession =
  ( mkPostCheckoutSessionsRequestBody
      "https://localhost:8080/payments/index.html?success=false&sessionId={CHECKOUT_SESSION_ID}"
      [PostCheckoutSessionsRequestBodyPaymentMethodTypes'EnumCard]
      "https://localhost:8080/payments/index.html?success=true&sessionId={CHECKOUT_SESSION_ID}"
  )
    { postCheckoutSessionsRequestBodyLineItems = Just [checkoutLineItem, checkoutLineItem2]
    }

runCheckoutSession :: MonadHTTP m => m (Response PostCheckoutSessionsResponse)
runCheckoutSession =
  runWithConfiguration defaultConfiguration $
    postCheckoutSessions checkoutSession

runCheckoutSessionRaw :: MonadHTTP m => m (Response ByteString)
runCheckoutSessionRaw =
  runWithConfiguration defaultConfiguration $
    postCheckoutSessionsRaw checkoutSession

runGetPaymentIntent :: MonadHTTP m => m (Response GetPaymentIntentsResponse)
runGetPaymentIntent =
  getPaymentIntentsWithConfiguration
    defaultConfiguration {configBaseURL = "http://test.url"}
    parameters
  where
    parameters =
      GetPaymentIntentsParameters
        { getPaymentIntentsParametersQueryCreated = Just $ GetPaymentIntentsParametersQueryCreated'Int 3,
          getPaymentIntentsParametersQueryCustomer = Nothing,
          getPaymentIntentsParametersQueryEndingBefore = Just "some_string",
          getPaymentIntentsParametersQueryExpand = Just ["another_string"],
          getPaymentIntentsParametersQueryLimit = Just 100,
          getPaymentIntentsParametersQueryStartingAfter = Just "the last string"
        }

runPostPaymentIntent :: MonadHTTP m => m (Response PostPaymentIntentsResponse)
runPostPaymentIntent = postPaymentIntentsWithConfiguration defaultConfiguration myPaymentIntent
  where
    myPaymentIntent =
      ( mkPostPaymentIntentsRequestBody
          171
          "CHF"
      )
        { postPaymentIntentsRequestBodyApplicationFeeAmount = Just 7,
          postPaymentIntentsRequestBodyPaymentMethodTypes = Just ["myType", "mySecondType"]
        }
