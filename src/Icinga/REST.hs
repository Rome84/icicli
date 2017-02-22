{-# LANGUAGE OverloadedStrings #-}

module Icinga.REST
  ( Credentials (..)
  , get
  , put
  , post
  , delete
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Catch
import Data.CaseInsensitive (CI, mk)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Network.HTTP.Types (RequestHeaders)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

type URL = String

data Credentials
  = Credentials
  { username :: String
  , password :: String
  }
  deriving (Show, Eq)

data HTTPMethod
  = GET
  | PUT
  | POST
  | DELETE
  deriving (Show, Eq)

applyMethod :: HTTPMethod -> Request -> Request
applyMethod GET req = req { method = "GET" }
applyMethod PUT req = req { method = "PUT" }
applyMethod POST req = req { method = "POST" }
applyMethod DELETE req = req { method = "DELETE" }

applyAuth :: Maybe Credentials -> Request -> Request
applyAuth Nothing req = req
applyAuth (Just (Credentials username password)) req = do
  let username' = B.pack username
      password' = B.pack password
  applyBasicAuth username' password' req

applyHeaders :: RequestHeaders -> Request -> Request
applyHeaders [] req = req
applyHeaders headers req = req { requestHeaders = headers }

applyBody :: Maybe RequestBody -> Request -> Request
applyBody Nothing req = req
applyBody (Just body) req = req { requestBody = body }

applyThrowOnError :: Bool -> Request -> Request
applyThrowOnError True req = req
applyThrowOnError False req = req

buildRequest :: MonadThrow m => HTTPMethod -> URL -> Maybe Credentials -> RequestHeaders -> Maybe RequestBody -> m Request
buildRequest method url creds headers body = apply <$> parseUrlThrow url
  where
    apply = applyMethod method . applyAuth creds . applyHeaders headers . applyBody body . applyThrowOnError False

buildManager :: IO Manager
buildManager = newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing

request :: HTTPMethod -> URL -> Maybe Credentials -> RequestHeaders -> Maybe RequestBody -> IO (Response LB.ByteString)
request method url creds headers body = do
  req <- buildRequest method url creds headers body
  man <- buildManager
  httpLbs req man

toHeaders :: [(String, String)] -> RequestHeaders
toHeaders xs = loop xs []
  where
    loop :: [(String, String)] -> RequestHeaders -> RequestHeaders
    loop [] aux = aux
    loop ((a, b):xs) aux = (mk $ B.pack a, B.pack b) : aux

get :: URL -> Maybe Credentials -> [(String, String)] -> IO (Response LB.ByteString)
get url creds headers = request GET url creds (toHeaders headers) Nothing

put :: URL -> Maybe Credentials -> [(String, String)] -> String -> IO (Response LB.ByteString)
put url creds headers body = request PUT url creds (toHeaders headers) (Just $ RequestBodyBS $ B.pack body)

post :: URL -> Maybe Credentials -> [(String, String)] -> String -> IO (Response LB.ByteString)
post url creds headers body = request POST url creds (toHeaders headers) (Just $ RequestBodyBS $ B.pack body)

delete :: URL -> Maybe Credentials -> [(String, String)] -> String -> IO (Response LB.ByteString)
delete url creds headers body = request DELETE url creds (toHeaders headers) (Just $ RequestBodyBS $ B.pack body)
