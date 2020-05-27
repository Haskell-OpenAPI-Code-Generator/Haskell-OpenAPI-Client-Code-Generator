{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString.Char8
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common
import qualified OpenAPI.Operations.PostCheckoutSessions as OpCheckout
import qualified OpenAPI.Types as Types

lineItemMetaData = Types.lineItemMetadata

lineItemPeriod = Types.InvoiceLineItemPeriod 10 80

checkoutLineItem =
  OpCheckout.PostCheckoutSessionsRequestBodyLineItems'
    { OpCheckout.postCheckoutSessionsRequestBodyLineItems'Amount = Just 1000,
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'Currency = Just "CHF",
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'Description = Just "algebraic data types",
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'Images = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'Name = Just "static types",
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'Quantity = 4,
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'TaxRates = Nothing
    }

checkoutLineItem2 =
  OpCheckout.PostCheckoutSessionsRequestBodyLineItems'
    { OpCheckout.postCheckoutSessionsRequestBodyLineItems'Amount = Just 5000,
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'Currency = Just "CHF",
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'Description = Just "lambda calculus",
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'Images = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'Name = Just "Haskell",
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'Quantity = 2,
      OpCheckout.postCheckoutSessionsRequestBodyLineItems'TaxRates = Nothing
    }

checkoutSession =
  OpCheckout.PostCheckoutSessionsRequestBody
    { OpCheckout.postCheckoutSessionsRequestBodyBillingAddressCollection = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyCancelUrl = "https://localhost:8080/payments/index.html?success=false&sessionId={CHECKOUT_SESSION_ID}",
      OpCheckout.postCheckoutSessionsRequestBodyClientReferenceId = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyCustomer = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyCustomerEmail = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyExpand = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyLineItems = Just [checkoutLineItem, checkoutLineItem2],
      OpCheckout.postCheckoutSessionsRequestBodyLocale = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyMetadata = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyMode = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyPaymentIntentData = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodyPaymentMethodTypes = [OpCheckout.PostCheckoutSessionsRequestBodyPaymentMethodTypes'EnumStringCard],
      OpCheckout.postCheckoutSessionsRequestBodySetupIntentData = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodySubmitType = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodySubscriptionData = Nothing,
      OpCheckout.postCheckoutSessionsRequestBodySuccessUrl = "https://localhost:8080/payments/index.html?success=true&sessionId={CHECKOUT_SESSION_ID}"
    }

runCheckoutSession :: MonadHTTP m => m (Either HttpException (Response PostCheckoutSessionsResponse))
runCheckoutSession =
  runWithConfiguration defaultConfiguration $
    OpCheckout.postCheckoutSessionsM checkoutSession

runCheckoutSessionRaw :: MonadHTTP m => m (Either HttpException (Response ByteString))
runCheckoutSessionRaw =
  runWithConfiguration defaultConfiguration $
    OpCheckout.postCheckoutSessionsRawM checkoutSession

runGetPaymentIntent :: MonadHTTP m => m (Either HttpException (Response GetPaymentIntentsResponse))
runGetPaymentIntent =
  getPaymentIntents
    defaultConfiguration {configBaseURL = "http://test.url"}
    (Just "the first string")
    Nothing
    (Just "some_string")
    (Just "another_string")
    (Just 100)
    (Just "the last string")
    Nothing

runPostPaymentIntent :: MonadHTTP m => m (Either HttpException (Response PostPaymentIntentsResponse))
runPostPaymentIntent = postPaymentIntents defaultConfiguration myPaymentIntent
  where
    myPaymentIntent =
      PostPaymentIntentsRequestBody
        { postPaymentIntentsRequestBodyAmount = 171,
          postPaymentIntentsRequestBodyApplicationFeeAmount = Just 7,
          postPaymentIntentsRequestBodyCaptureMethod = Nothing,
          postPaymentIntentsRequestBodyConfirm = Nothing,
          postPaymentIntentsRequestBodyConfirmationMethod = Nothing,
          postPaymentIntentsRequestBodyCurrency = "CHF",
          postPaymentIntentsRequestBodyCustomer = Nothing,
          postPaymentIntentsRequestBodyDescription = Nothing,
          postPaymentIntentsRequestBodyErrorOnRequiresAction = Nothing,
          postPaymentIntentsRequestBodyExpand = Nothing,
          postPaymentIntentsRequestBodyMandate = Nothing,
          postPaymentIntentsRequestBodyMandateData = Nothing,
          postPaymentIntentsRequestBodyMetadata = Nothing,
          postPaymentIntentsRequestBodyOffSession = Nothing,
          postPaymentIntentsRequestBodyOnBehalfOf = Nothing,
          postPaymentIntentsRequestBodyPaymentMethod = Nothing,
          postPaymentIntentsRequestBodyPaymentMethodOptions = Nothing,
          postPaymentIntentsRequestBodyPaymentMethodTypes = Just ["myType", "mySecondType"],
          postPaymentIntentsRequestBodyReceiptEmail = Nothing,
          postPaymentIntentsRequestBodyReturnUrl = Nothing,
          postPaymentIntentsRequestBodySavePaymentMethod = Nothing,
          postPaymentIntentsRequestBodySetupFutureUsage = Nothing,
          postPaymentIntentsRequestBodyShipping = Nothing,
          postPaymentIntentsRequestBodyStatementDescriptor = Nothing,
          postPaymentIntentsRequestBodyStatementDescriptorSuffix = Nothing,
          postPaymentIntentsRequestBodyTransferData = Nothing,
          postPaymentIntentsRequestBodyTransferGroup = Nothing,
          postPaymentIntentsRequestBodyUseStripeSdk = Nothing
        }
