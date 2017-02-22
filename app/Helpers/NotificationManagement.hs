module Helpers.NotificationManagement
  ( getNotification'
  , getNotification
  , newNotification'
  , newNotification
  , setNotificationVars'
  , setNotificationVars
  , delNotification'
  , delNotification
  ) where

import App
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Map (fromList)
import Data.Maybe (isNothing)
import qualified Icinga as I
import qualified Icinga.Models.NotificationConfig as MNC
import Text.Read (readMaybe)

getNotification' :: String -> String -> App Bool
getNotification' hostname = getNotification hostname ""

getNotification :: String -> String -> String -> App Bool
getNotification hostname servicename name = do
  setup <- toSetup <$> getConfig
  let svcName = if null servicename then Nothing; else Just servicename
  (result, rc) <- liftIO $ I.getNotification setup hostname svcName name
  case result of
    Nothing -> liftIO $ putStrLn "" >> putStrLn "Nothing" >> putStrLn ""
    Just value -> liftIO $ putStrLn "" >> putStrLn value >> putStrLn ""
  return rc

newNotification' :: String -> String -> String -> String -> String -> String -> String -> String -> App Bool
newNotification' hostname servicename name templates command users states vars =
  case readMaybe templates :: Maybe [String] of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse templates: " ++ templates) >> return False
    Just templates' ->
      case readMaybe users :: Maybe [String] of
        Nothing -> liftIO $ putStrLn ("ERROR: cannot parse users: " ++ users) >> return False
        Just users' ->
          case readMaybe states :: Maybe [Int] of
            Nothing -> liftIO $ putStrLn ("ERROR: cannot parse states: " ++ states) >> return False
            Just states' ->
              case readMaybe vars :: Maybe [(String, String)] of
                Nothing -> liftIO $ putStrLn ("ERROR: cannot parse vars: " ++ vars) >> return False
                Just vars' -> do
                  let svcName = if null servicename then Nothing; else Just servicename
                  newNotification hostname svcName name templates' command users' states' vars'

newNotification :: String -> Maybe String -> String -> [String] -> String -> [String] -> [Int] -> [(String, String)] -> App Bool
newNotification hostname servicename name templates command users states vars = do
  setup <- toSetup <$> getConfig
  let config = (MNC.mkNotificationConfig hostname servicename command) { MNC.states = if null states then Nothing; else Just states }
      mtemplates = case templates of [] -> Nothing; _ -> Just templates
  liftIO $ I.createNotification setup hostname servicename name mtemplates config >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

setNotificationVars' :: String -> String -> String -> String -> App Bool
setNotificationVars' hostname servicename name vars = do
  let svcName = if null servicename then Nothing; else Just servicename
  case readMaybe vars :: Maybe [(String, String)] of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse vars: " ++ vars) >> return False
    Just vars' -> setNotificationVars hostname svcName name vars'

setNotificationVars :: String -> Maybe String -> String -> [(String, String)] -> App Bool
setNotificationVars hostname servicename name vars = do
  setup <- toSetup <$> getConfig
  let config = MNC.defNotificationConfig { MNC.vars = Just $ fromList vars }
  liftIO $ I.modifyNotification setup hostname servicename name config >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

delNotification' :: String -> String -> String -> App Bool
delNotification' hostname servicename name = do
  let svcName = if null servicename then Nothing; else Just servicename
  delNotification hostname svcName name

delNotification :: String -> Maybe String -> String -> App Bool
delNotification hostname servicename name = do
  setup <- toSetup <$> getConfig
  liftIO $ I.deleteNotification setup hostname servicename name >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc
