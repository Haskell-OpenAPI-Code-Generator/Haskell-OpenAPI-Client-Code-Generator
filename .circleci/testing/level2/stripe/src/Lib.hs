{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.ByteString.Char8
import Network.HTTP.Client
import OpenAPI
import OpenAPI.Common

lineItemPeriod = InvoiceLineItemPeriod 10 80

checkoutLineItem =
  PostCheckoutSessionsRequestBodyLineItems'
    { postCheckoutSessionsRequestBodyLineItems'Amount = Just 1000,
      postCheckoutSessionsRequestBodyLineItems'Currency = Just "CHF",
      postCheckoutSessionsRequestBodyLineItems'Description = Just "algebraic data types",
      postCheckoutSessionsRequestBodyLineItems'Images = Nothing,
      postCheckoutSessionsRequestBodyLineItems'Name = Just "static types",
      postCheckoutSessionsRequestBodyLineItems'Quantity = 4,
      postCheckoutSessionsRequestBodyLineItems'TaxRates = Nothing
    }

checkoutLineItem2 =
  PostCheckoutSessionsRequestBodyLineItems'
    { postCheckoutSessionsRequestBodyLineItems'Amount = Just 5000,
      postCheckoutSessionsRequestBodyLineItems'Currency = Just "CHF",
      postCheckoutSessionsRequestBodyLineItems'Description = Just "lambda calculus",
      postCheckoutSessionsRequestBodyLineItems'Images = Nothing,
      postCheckoutSessionsRequestBodyLineItems'Name = Just "Haskell",
      postCheckoutSessionsRequestBodyLineItems'Quantity = 2,
      postCheckoutSessionsRequestBodyLineItems'TaxRates = Nothing
    }

checkoutSession =
  PostCheckoutSessionsRequestBody
    { postCheckoutSessionsRequestBodyBillingAddressCollection = Nothing,
      postCheckoutSessionsRequestBodyCancelUrl = "https://localhost:8080/payments/index.html?success=false&sessionId={CHECKOUT_SESSION_ID}",
      postCheckoutSessionsRequestBodyClientReferenceId = Nothing,
      postCheckoutSessionsRequestBodyCustomer = Nothing,
      postCheckoutSessionsRequestBodyCustomerEmail = Nothing,
      postCheckoutSessionsRequestBodyExpand = Nothing,
      postCheckoutSessionsRequestBodyLineItems = Just [checkoutLineItem, checkoutLineItem2],
      postCheckoutSessionsRequestBodyLocale = Nothing,
      postCheckoutSessionsRequestBodyMetadata = Nothing,
      postCheckoutSessionsRequestBodyMode = Nothing,
      postCheckoutSessionsRequestBodyPaymentIntentData = Nothing,
      postCheckoutSessionsRequestBodyPaymentMethodTypes = [PostCheckoutSessionsRequestBodyPaymentMethodTypes'EnumStringCard],
      postCheckoutSessionsRequestBodySetupIntentData = Nothing,
      postCheckoutSessionsRequestBodySubmitType = Nothing,
      postCheckoutSessionsRequestBodySubscriptionData = Nothing,
      postCheckoutSessionsRequestBodySuccessUrl = "https://localhost:8080/payments/index.html?success=true&sessionId={CHECKOUT_SESSION_ID}"
    }

runCheckoutSession :: MonadHTTP m => m (Either HttpException (Response PostCheckoutSessionsResponse))
runCheckoutSession =
  runWithConfiguration defaultConfiguration $
    postCheckoutSessionsM checkoutSession

runCheckoutSessionRaw :: MonadHTTP m => m (Either HttpException (Response ByteString))
runCheckoutSessionRaw =
  runWithConfiguration defaultConfiguration $
    postCheckoutSessionsRawM checkoutSession

runGetPaymentIntent :: MonadHTTP m => m (Either HttpException (Response GetPaymentIntentsResponse))
runGetPaymentIntent =
  getPaymentIntents
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
