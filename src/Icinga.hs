{-# LANGUAGE CPP #-}

module Icinga
  ( Credentials (..)
  , Setup (..)
  -- status
  , checkStatus
  -- host group management
  , getHostGroup
  , createHostGroup
  , modifyHostGroup
  , deleteHostGroup
  -- host management
  , getHost
  , createHost
  , modifyHost
  , deleteHost
  -- service group management
  , getServiceGroup
  , createServiceGroup
  , modifyServiceGroup
  , deleteServiceGroup
  -- service management
  , getService
  , createService
  , modifyService
  , deleteService
  , processCheckResult
  -- notification management
  , getNotification
  , createNotification
  , modifyNotification
  , deleteNotification
  ) where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 as LB
import Data.ByteString as B
import qualified Data.List as DL
import Debug.Trace (traceIO)
import qualified Icinga.Models.HostGroup as MHG
import qualified Icinga.Models.HostGroupConfig as MHGC
import qualified Icinga.Models.Host as MH
import qualified Icinga.Models.HostConfig as MHC
import qualified Icinga.Models.ServiceGroup as MSG
import qualified Icinga.Models.ServiceGroupConfig as MSGC
import qualified Icinga.Models.Service as MS
import qualified Icinga.Models.ServiceConfig as MSC
import qualified Icinga.Models.ProcessCheckResult as MPCR
import qualified Icinga.Models.Notification as MN
import qualified Icinga.Models.NotificationConfig as MNC
import qualified Icinga.Models.Response as MR
import qualified Icinga.Models.CreateRequest as MCR
import qualified Icinga.Models.ModifyRequest as MMR
import Icinga.REST
import Network.HTTP.Conduit (responseStatus, responseBody)
import Network.HTTP.Types (Status (..), statusIsSuccessful)

data Setup
  = Setup
  { baseUrl :: String
  , credentials :: Maybe Credentials
  , routingKey :: Maybe String
  }
  deriving (Show, Eq)

debug :: String -> IO ()
#ifdef DEBUG
debug msg = traceIO $ "\nDEBUG:\n" ++ DL.unlines (DL.map ("  " ++) (DL.lines msg))
#else
debug _ = return ()
#endif

prettyConfig :: Config
prettyConfig = Config (Spaces 2) mempty Generic

deleteBody :: Setup -> String
deleteBody Setup { routingKey = Nothing } = "{ \"attrs\": null }"
deleteBody Setup { routingKey = Just routingKey } = "{ \"attrs\": { \"vars.routing_key\": \"" ++ routingKey ++ "\" } }"

-- status

checkStatus :: Setup -> IO (String, Bool)
checkStatus setup = do
  res <- get (baseUrl setup ++ "/status") (credentials setup) []
  debug $ "checkStatus:\n" ++ show res
  let resBody = responseBody res
  return (LB.unpack resBody, statusIsSuccessful $ responseStatus res)

-- host group management

getHostGroupUrl :: Setup -> String -> String
getHostGroupUrl setup groupname = baseUrl setup ++ "/objects/hostgroups/" ++ groupname

getHostGroup :: Setup -> String -> IO (Maybe String, Bool)
getHostGroup setup groupname = do
  let url = getHostGroupUrl setup groupname
      headers = [("Accept", "application/json")]
  res <- get url (credentials setup) headers
  debug $ "getHostGroup ->\n" ++ show res
  let resBody = responseBody res
  let result = MR.getResponseBody (decode resBody :: Maybe (MR.Response MHG.HostGroup))
  case result of
    Nothing -> return (Nothing, statusIsSuccessful $ responseStatus res)
    Just group -> return (Just $ LB.unpack $ encodePretty' prettyConfig group, statusIsSuccessful $ responseStatus res)

createHostGroup :: Setup -> String -> Maybe [String] -> MHGC.HostGroupConfig -> IO (Status, Bool)
createHostGroup setup groupname templates groupConfig = do
  let config = MHGC.updateRoutingKey groupConfig (routingKey setup)
      req = MCR.CreateRequest { MCR.templates = templates, MCR.attrs = config }
      url = getHostGroupUrl setup groupname
      reqBody = LB.unpack $ encode req
  res <- put url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "createHostGroup ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

modifyHostGroup :: Setup -> String -> MHGC.HostGroupConfig -> IO (Status, Bool)
modifyHostGroup setup groupname groupConfig = do
  let config = MHGC.updateRoutingKey groupConfig (routingKey setup)
  let req = MMR.ModifyRequest { MMR.attrs = config }
      url = getHostGroupUrl setup groupname
      reqBody = LB.unpack $ encode req
  res <- post url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "modifyHostGroup ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

deleteHostGroup :: Setup -> String -> IO (Status, Bool)
deleteHostGroup setup groupname = do
  let url = getHostGroupUrl setup groupname ++ "?cascade=1"
      body = deleteBody setup
      headers = [("Accept", "application/json")]
  res <- delete url (credentials setup) headers body
  debug $ "deleteHostGroup ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

-- host management

getHostUrl :: Setup -> String -> String
getHostUrl setup hostname = baseUrl setup ++ "/objects/hosts/" ++ hostname

getHost :: Setup -> String -> IO (Maybe String, Bool)
getHost setup hostname = do
  let url = getHostUrl setup hostname
      headers = [("Accept", "application/json")]
  res <- get url (credentials setup) headers
  debug $ "getHost ->\n" ++ show res
  let resBody = responseBody res
  let result = MR.getResponseBody (decode resBody :: Maybe (MR.Response MH.Host))
  case result of
    Nothing -> return (Nothing, statusIsSuccessful $ responseStatus res)
    Just host -> return (Just $ LB.unpack $ encodePretty' prettyConfig host, statusIsSuccessful $ responseStatus res)

createHost :: Setup -> String -> Maybe [String] -> MHC.HostConfig -> IO (Status, Bool)
createHost setup hostname templates hostConfig = do
  let config = MHC.updateRoutingKey hostConfig (routingKey setup)
      req = MCR.CreateRequest { MCR.templates = templates, MCR.attrs = config }
      url = getHostUrl setup hostname
      reqBody = LB.unpack $ encode req
  res <- put url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "createHost ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

modifyHost :: Setup -> String -> MHC.HostConfig -> IO (Status, Bool)
modifyHost setup hostname hostConfig = do
  let config = MHC.updateRoutingKey hostConfig (routingKey setup)
  let req = MMR.ModifyRequest { MMR.attrs = config }
      url = getHostUrl setup hostname
      reqBody = LB.unpack $ encode req
  res <- post url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "modifyHost ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

deleteHost :: Setup -> String -> IO (Status, Bool)
deleteHost setup hostname = do
  let url = getHostUrl setup hostname ++ "?cascade=1"
      body = deleteBody setup
      headers = [("Accept", "application/json")]
  res <- delete url (credentials setup) headers body
  debug $ "deleteHost ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

-- service group management

getServiceGroupUrl :: Setup -> String -> String
getServiceGroupUrl setup name = baseUrl setup ++ "/objects/servicegroups/" ++ name

getServiceGroup :: Setup -> String -> IO (Maybe String, Bool)
getServiceGroup setup name = do
  let url = getServiceGroupUrl setup name
      headers = [("Accept", "application/json")]
  res <- get url (credentials setup) headers
  debug $ "getServiceGroup ->\n" ++ show res
  let resBody = responseBody res
  let result = MR.getResponseBody (decode resBody :: Maybe (MR.Response MSG.ServiceGroup))
  case result of
    Nothing -> return (Nothing, statusIsSuccessful $ responseStatus res)
    Just group -> return (Just $ LB.unpack $ encodePretty' prettyConfig group, statusIsSuccessful $ responseStatus res)

createServiceGroup :: Setup -> String -> Maybe [String] -> MSGC.ServiceGroupConfig -> IO (Status, Bool)
createServiceGroup setup name templates serviceConfig = do
  let config = MSGC.updateRoutingKey serviceConfig (routingKey setup)
      req = MCR.CreateRequest { MCR.templates = templates, MCR.attrs = config }
      url = getServiceGroupUrl setup name
      reqBody = LB.unpack $ encode req
  res <- put url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "createServiceGroup ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

modifyServiceGroup :: Setup -> String -> MSGC.ServiceGroupConfig -> IO (Status, Bool)
modifyServiceGroup setup name serviceConfig = do
  let config = MSGC.updateRoutingKey serviceConfig (routingKey setup)
  let req = MMR.ModifyRequest { MMR.attrs = config }
      url = getServiceGroupUrl setup name
      reqBody = LB.unpack $ encode req
  res <- post url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "modifyServiceGroup ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

deleteServiceGroup :: Setup -> String -> IO (Status, Bool)
deleteServiceGroup setup name = do
  let url = getServiceGroupUrl setup name ++ "?cascade=1"
      body = deleteBody setup
      headers = [("Accept", "application/json")]
  res <- delete url (credentials setup) headers body
  debug $ "deleteServiceGroup ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

-- service management

getServiceUrl :: Setup -> String -> String -> String
getServiceUrl setup hostname name = baseUrl setup ++ "/objects/services/" ++ hostname ++ "!" ++ name

getService :: Setup -> String -> String -> IO (Maybe String, Bool)
getService setup hostname name = do
  let url = getServiceUrl setup hostname name
      headers = [("Accept", "application/json")]
  res <- get url (credentials setup) headers
  debug $ "getService ->\n" ++ show res
  let resBody = responseBody res
  let result = MR.getResponseBody (decode resBody :: Maybe (MR.Response MS.Service))
  case result of
    Nothing -> return (Nothing, statusIsSuccessful $ responseStatus res)
    Just host -> return (Just $ LB.unpack $ encodePretty' prettyConfig host, statusIsSuccessful $ responseStatus res)

createService :: Setup -> String -> String -> Maybe [String] -> MSC.ServiceConfig -> IO (Status, Bool)
createService setup hostname name templates serviceConfig = do
  let config = MSC.updateRoutingKey serviceConfig (routingKey setup)
      req = MCR.CreateRequest { MCR.templates = templates, MCR.attrs = config }
      url = getServiceUrl setup hostname name
      reqBody = LB.unpack $ encode req
  res <- put url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "createService ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

modifyService :: Setup -> String -> String -> MSC.ServiceConfig -> IO (Status, Bool)
modifyService setup hostname name serviceConfig = do
  let config = MSC.updateRoutingKey serviceConfig (routingKey setup)
  let req = MMR.ModifyRequest { MMR.attrs = config }
      url = getServiceUrl setup hostname name
      reqBody = LB.unpack $ encode req
  res <- post url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "modifyService ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

deleteService :: Setup -> String -> String -> IO (Status, Bool)
deleteService setup hostname name = do
  let url = getServiceUrl setup hostname name ++ "?cascade=1"
      body = deleteBody setup
      headers = [("Accept", "application/json")]
  res <- delete url (credentials setup) headers body
  debug $ "deleteService ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

getProcessCheckResultUrl :: Setup -> String -> String -> String
getProcessCheckResultUrl setup hostname servicename = baseUrl setup ++ "/actions/process-check-result?service=" ++ hostname ++ "!" ++ servicename

processCheckResult :: Setup -> String -> String -> MPCR.ProcessCheckResult -> IO (Status, Bool)
processCheckResult setup hostname name result = do
  let url = getProcessCheckResultUrl setup hostname name
      reqBody = LB.unpack $ encode result
  res <- post url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "processCheckResult ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

-- notification management

getNotificationUrl :: Setup -> String -> Maybe String -> String -> String
getNotificationUrl setup hostname Nothing name = baseUrl setup ++ "/objects/notifications/" ++ hostname ++ "!" ++ "!" ++ name
getNotificationUrl setup hostname (Just servicename) name = baseUrl setup ++ "/objects/notifications/" ++ hostname ++ "!" ++ servicename ++ "!" ++ name

getNotification :: Setup -> String -> Maybe String -> String -> IO (Maybe String, Bool)
getNotification setup hostname servicename name = do
  let url = getNotificationUrl setup hostname servicename name
      headers = [("Accept", "application/json")]
  res <- get url (credentials setup) headers
  debug $ "getNotification ->\n" ++ show res
  let resBody = responseBody res
  let result = MR.getResponseBody (decode resBody :: Maybe (MR.Response MN.Notification))
  case result of
    Nothing -> return (Nothing, statusIsSuccessful $ responseStatus res)
    Just host -> return (Just $ LB.unpack $ encodePretty' prettyConfig host, statusIsSuccessful $ responseStatus res)

createNotification :: Setup -> String -> Maybe String -> String -> Maybe [String] -> MNC.NotificationConfig -> IO (Status, Bool)
createNotification setup hostname servicename name templates notificationConfig = do
  let config = MNC.updateRoutingKey notificationConfig (routingKey setup)
      req = MCR.CreateRequest { MCR.templates = templates, MCR.attrs = config }
      url = getNotificationUrl setup hostname servicename name
      reqBody = LB.unpack $ encode req
  res <- put url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "createNotification ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

modifyNotification :: Setup -> String -> Maybe String -> String -> MNC.NotificationConfig -> IO (Status, Bool)
modifyNotification setup hostname servicename name notificationConfig = do
  let config = MNC.updateRoutingKey notificationConfig (routingKey setup)
  let req = MMR.ModifyRequest { MMR.attrs = config }
      url = getNotificationUrl setup hostname servicename name
      reqBody = LB.unpack $ encode req
  res <- post url (credentials setup) [("Accept", "application/json")] reqBody
  debug $ "modifyNotification ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)

deleteNotification :: Setup -> String -> Maybe String -> String -> IO (Status, Bool)
deleteNotification setup hostname servicename name = do
  let url = getNotificationUrl setup hostname servicename name ++ "?cascade=1"
      body = deleteBody setup
      headers = [("Accept", "application/json")]
  res <- delete url (credentials setup) headers body
  debug $ "deleteNotification ->\n" ++ show res
  return (responseStatus res, statusIsSuccessful $ responseStatus res)
