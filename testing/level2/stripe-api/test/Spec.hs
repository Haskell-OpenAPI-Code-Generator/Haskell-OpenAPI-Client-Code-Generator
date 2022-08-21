{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Reader
import qualified Data.Aeson as Aeson
import Data.ByteString.Char8
import Data.Either
import Lib
import Network.HTTP.Client.Internal
import Network.HTTP.Simple
import Network.HTTP.Types
import OpenAPI
import Test.Hspec
import Test.MonadHTTP

paymentIntent =
  mkPaymentIntent
    23
    PaymentIntentCaptureMethod'EnumAutomatic
    (PaymentIntentConfirmationMethod'Other $ Aeson.String "bar")
    45
    "CHF"
    "123"
    False
    []
    PaymentIntentStatus'EnumSucceeded

succeededResponseBody :: ByteString
succeededResponseBody =
  mconcat
    [ "{",
      "\"amount\": 23,",
      "\"automatic_tax\": {\"enabled\":false},",
      "\"capture_method\": \"automatic\",",
      "\"confirmation_method\": \"bar\",",
      "\"created\": 45,",
      "\"currency\": \"CHF\",",
      "\"id\": \"123\",",
      "\"livemode\": false,",
      "\"object\": \"payment_intent\",",
      "\"payment_method_types\": [],",
      "\"status\": \"succeeded\"",
      "}"
    ]

succeededResponse :: Response ByteString
succeededResponse =
  defaultResponse
    { responseBody = succeededResponseBody
    }

successResponseGet :: Response ByteString
successResponseGet =
  defaultResponse
    { responseBody =
        mconcat
          [ "{",
            "\"has_more\": false,",
            "\"object\": \"list\",",
            "\"url\": \"http://example.org\",",
            "\"data\": [" <> succeededResponseBody <> "]",
            "}"
          ]
    }

successResponseGetExpected =
  GetPaymentIntentsResponseBody200
    { getPaymentIntentsResponseBody200Data = [paymentIntent],
      getPaymentIntentsResponseBody200HasMore = False,
      getPaymentIntentsResponseBody200Url = "http://example.org"
    }

main :: IO ()
main =
  hspec $ do
    describe "runPostPaymentIntent" $
      it "should encode Body" $
        do
          let requestExpectation = expectBody "amount=171&application_fee_amount=7&currency=CHF&payment_method_types%5B%5D=myType&payment_method_types%5B%5D=mySecondType" $ expectMethod "POST" noExpectation
          response <- runMock runPostPaymentIntent (requestExpectation, succeededResponse)
          getResponseBody response `shouldBe` PostPaymentIntentsResponse200 paymentIntent
    describe "runGetPaymentIntent" $
      it "should encode Body" $
        do
          let requestExpectation =
                expectMethod "GET" $
                  expectURL
                    "http://test.url/v1/payment_intents?created=3&ending_before=some_string&expand%5B%5D=another_string&limit=100&starting_after=the%20last%20string"
                    noExpectation
          response <- runMock runGetPaymentIntent (requestExpectation, successResponseGet)
          getResponseBody response `shouldBe` GetPaymentIntentsResponse200 successResponseGetExpected
    describe "runCheckoutSession" $ do
      it "should encode Body" $ do
        let requestExpectation = expectBody "cancel_url=https%3A%2F%2Flocalhost%3A8080%2Fpayments%2Findex.html%3Fsuccess%3Dfalse%26sessionId%3D%7BCHECKOUT_SESSION_ID%7D&line_items%5B%5D%5Bamount%5D=1000&line_items%5B%5D%5Bcurrency%5D=CHF&line_items%5B%5D%5Bdescription%5D=algebraic%20data%20types&line_items%5B%5D%5Bname%5D=static%20types&line_items%5B%5D%5Bquantity%5D=4&line_items%5B%5D%5Bamount%5D=5000&line_items%5B%5D%5Bcurrency%5D=CHF&line_items%5B%5D%5Bdescription%5D=lambda%20calculus&line_items%5B%5D%5Bname%5D=Haskell&line_items%5B%5D%5Bquantity%5D=2&payment_method_types%5B%5D=card&success_url=https%3A%2F%2Flocalhost%3A8080%2Fpayments%2Findex.html%3Fsuccess%3Dtrue%26sessionId%3D%7BCHECKOUT_SESSION_ID%7D" $ expectMethod "POST" noExpectation
        response <- runMock runCheckoutSession (requestExpectation, succeededResponse)
        getResponseBody response `shouldBe` PostCheckoutSessionsResponseError "Error in $: key \"cancel_url\" not found"
      it "should run Raw Response" $ do
        let requestExpectation = noExpectation
        response <- runMock runCheckoutSessionRaw (requestExpectation, succeededResponse)
        getResponseBody response `shouldBe` succeededResponseBody
