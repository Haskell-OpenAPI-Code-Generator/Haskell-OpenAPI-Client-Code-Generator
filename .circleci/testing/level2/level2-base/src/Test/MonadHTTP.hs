{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.MonadHTTP where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Network.HTTP.Client.Internal
import Network.HTTP.Simple
import Network.HTTP.Types
import OpenAPI.Common

newtype MethodMismatch = MethodMismatch Method
  deriving (Show, Eq)

instance Exception MethodMismatch

newtype URLMismatch = URLMismatch String
  deriving (Show, Eq)

instance Exception URLMismatch

newtype AuthorizationMismatch = AuthorizationMismatch (Maybe ByteString)
  deriving (Show, Eq)

instance Exception AuthorizationMismatch

newtype BodyMismatch = BodyMismatch (Maybe ByteString)
  deriving (Show, Eq)

instance Exception BodyMismatch

data RequestExpectation
  = RequestExpectation
      { methodExpectation :: Maybe Method,
        urlExpectation :: Maybe String,
        authorizationExpectation :: Maybe String,
        bodyExpectation :: Maybe ByteString
      }

noExpectation :: RequestExpectation
noExpectation = RequestExpectation Nothing Nothing Nothing Nothing

expectMethod :: Method -> RequestExpectation -> RequestExpectation
expectMethod method expectation = expectation {methodExpectation = Just method}

expectURL :: String -> RequestExpectation -> RequestExpectation
expectURL url expectation = expectation {urlExpectation = Just url}

expectAuthorization :: String -> RequestExpectation -> RequestExpectation
expectAuthorization authorization expectation = expectation {authorizationExpectation = Just authorization}

expectBody :: ByteString -> RequestExpectation -> RequestExpectation
expectBody body expectation = expectation {bodyExpectation = Just body}

newtype MockMonadHTTP m a = MockMonadHTTP {unMock :: ReaderT (RequestExpectation, Response ByteString) m a}
  deriving (Functor, Applicative, Monad, MonadReader (RequestExpectation, Response ByteString))

runMock :: MockMonadHTTP m a -> (RequestExpectation, Response ByteString) -> m a
runMock (MockMonadHTTP m) = runReaderT m

instance (Monad m, MonadFail m) => MonadHTTP (MockMonadHTTP m) where
  httpBS request = do
    (requestExpectation, response) <- ask
    case methodExpectation requestExpectation of
      Nothing -> pure ()
      (Just m) ->
        let method' = method request
         in when (method' /= m) $
              throw (MethodMismatch method')
    case urlExpectation requestExpectation of
      Nothing -> pure ()
      (Just u) ->
        let uri = show (getUri request)
         in when (uri /= u) $
              throw (URLMismatch uri)
    case authorizationExpectation requestExpectation of
      Nothing -> pure ()
      (Just a) ->
        let maybeAuthorization = listToMaybe $ getRequestHeader hAuthorization request
         in when (maybeAuthorization /= Just (pack a)) $
              throw (AuthorizationMismatch maybeAuthorization)
    case bodyExpectation requestExpectation of
      Nothing -> pure ()
      (Just body) ->
        let maybeBody = case requestBody request of
              RequestBodyLBS b -> Just $ LBS.toStrict b
              RequestBodyBS b -> Just b
              _ -> Nothing
         in when (maybeBody /= Just body) $
              throw (BodyMismatch maybeBody)
    pure $ pure response

defaultResponse :: Response ByteString
defaultResponse =
  Response
    { responseStatus = mkStatus 200 "success",
      responseVersion = http11,
      responseHeaders = [],
      responseBody = "",
      responseCookieJar = createCookieJar [],
      responseClose' = ResponseClose (pure () :: IO ())
    }
