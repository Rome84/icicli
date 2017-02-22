module Main where

import App
import CmdArgsParser
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import qualified Icinga as I
import Network (withSocketsDo)
import Helpers
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hFlush, stdout)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  debug $ "Arguments: " ++ show args
  case args of
    -- help
    [] -> usage' >> exitSuccess
    ["?"] -> usage' >> exitSuccess
    ["help"] -> usage' >> exitSuccess
    -- interactive
    [url] -> processInteractive url "" "" ""
    [url, routingKey] -> processInteractive url "" "" routingKey
    [url, username, password] -> processInteractive url username password ""
    [url, username, password, routingKey] -> processInteractive url username password routingKey
    _ ->
      case args of
        -- non-interactive
        (url:username:password:routingKey:xs) -> processNonInteractive url username password routingKey xs
        (url:username:password:xs) -> processNonInteractive url username password "" xs
        (url:routingKey:xs) -> processNonInteractive url "" "" routingKey xs
        (url:xs) -> processNonInteractive url "" "" "" xs
        _ -> putStrLn ("Unknown command: " ++ unwords args) >> exitFailure

processInteractive :: String -> String -> String -> String -> IO ()
processInteractive url username password routingKey = do
  debug "INTERACTIVE"
  let config = mkConfig url username password routingKey
  runApp config $ do
    liftIO usage
    rc <- process Nothing
    liftIO $ if rc then exitSuccess; else exitFailure

processNonInteractive :: String -> String -> String -> String -> [String] -> IO ()
processNonInteractive url username password routingKey args = do
  debug $ "NON-INTERACTIVE: " ++ show args
  let config = mkConfig url username password routingKey
  runApp config $ do
    rc <- process $ Just args
    liftIO $ if rc then exitSuccess; else exitFailure

promptArgsOrPassthrough :: Maybe [String] -> IO ([String], Bool)
promptArgsOrPassthrough (Just args) = return (args, False)
promptArgsOrPassthrough Nothing = do
  line <- putStr "icicli> " >> hFlush stdout >> getLine
  case parseArgs line of
    Left _ -> return ([], True)
    Right x -> return (x, True)

process :: Maybe [String] -> App Bool
process args = do
  -- prompt for line if in interactive mode
  (args', isInteractive) <- liftIO $ promptArgsOrPassthrough args
  case args' of
    [] -> continueOrStop (Right ())
    -- general
    ["?"] -> liftIO (if isInteractive then usage else usage') >> continueOrStop (Right ())
    ["help"] -> liftIO (if isInteractive then usage else usage') >> continueOrStop (Right ())
    ["quit"] -> return True
    ["exit"] -> return True
    ["status"] -> status >>= continueOrStop'
    -- host group
    ["get", "host-group", name] -> getHostGroup name >>= continueOrStop'
    ["new", "host-group", name, templates, displayName, groups] -> newHostGroup'' name templates displayName groups >>= continueOrStop'
    ["new", "host-group", name, templates, displayName, groups, vars] -> newHostGroup' name templates displayName groups vars >>= continueOrStop'
    ["set", "host-group", name, displayName] -> setHostGroup'' name displayName >>= continueOrStop'
    ["set", "host-group", name, displayName, vars] -> setHostGroup' name displayName vars >>= continueOrStop'
    ["del", "host-group", name] -> delHostGroup name >>= continueOrStop'
    -- host
    ["get", "host", name] -> getHost name >>= continueOrStop'
    ["new", "host", name, templates, displayName, groups, address, checkCommand] -> newHost'' name templates displayName groups address checkCommand >>= continueOrStop'
    ["new", "host", name, templates, displayName, groups, address, checkCommand, vars] -> newHost' name templates displayName groups address checkCommand vars >>= continueOrStop'
    ["set", "host", name, activeChecks, passiveChecks, perfData, notifications, eventHandler, flapDetection] -> setHostChecks' name activeChecks passiveChecks perfData notifications eventHandler flapDetection >>= continueOrStop'
    ["set", "host", name, checkInterval, retryInterval, maxCheckAttempts, checkCommand] -> setHostCommand'' name checkInterval retryInterval maxCheckAttempts checkCommand >>= continueOrStop'
    ["set", "host", name, checkInterval, retryInterval, maxCheckAttempts, checkCommand, vars] -> setHostCommand' name checkInterval retryInterval maxCheckAttempts checkCommand vars >>= continueOrStop'
    ["set", "host", name, vars] -> setHostVars' name vars >>= continueOrStop'
    ["del", "host", name] -> delHost name >>= continueOrStop'
    -- service group
    ["get", "service-group", name] -> getServiceGroup name >>= continueOrStop'
    ["new", "service-group", name, templates, displayName, groups] -> newServiceGroup'' name templates displayName groups >>= continueOrStop'
    ["new", "service-group", name, templates, displayName, groups, vars] -> newServiceGroup' name templates displayName groups vars >>= continueOrStop'
    ["set", "service-group", name, displayName] -> setServiceGroup'' name displayName >>= continueOrStop'
    ["set", "service-group", name, displayName, vars] -> setServiceGroup' name displayName vars >>= continueOrStop'
    ["del", "service-group", name] -> delServiceGroup name >>= continueOrStop'
    -- service
    ["get", "service", hostname, name] -> getService hostname name >>= continueOrStop'
    ["new", "service", hostname, name, templates, displayName, groups, checkCommand] -> newService'' hostname name templates displayName groups checkCommand >>= continueOrStop'
    ["new", "service", hostname, name, templates, displayName, groups, checkCommand, vars] -> newService' hostname name templates displayName groups checkCommand vars >>= continueOrStop'
    ["set", "service", hostname, name, activeChecks, passiveChecks, perfData, notifications, eventHandler, flapDetection] -> setServiceChecks' hostname name activeChecks passiveChecks perfData notifications eventHandler flapDetection >>= continueOrStop'
    ["set", "service", hostname, name, checkInterval, retryInterval, maxCheckAttempts, checkCommand] -> setServiceCommand'' hostname name checkInterval retryInterval maxCheckAttempts checkCommand >>= continueOrStop'
    ["set", "service", hostname, name, checkInterval, retryInterval, maxCheckAttempts, checkCommand, vars] -> setServiceCommand' hostname name checkInterval retryInterval maxCheckAttempts checkCommand vars >>= continueOrStop'
    ["set", "service", hostname, name, vars] -> setServiceVars' hostname name vars >>= continueOrStop'
    ["run", "service", hostname, name, exitStatus, pluginOutput] -> processCheckResult''' hostname name exitStatus pluginOutput >>= continueOrStop'
    ["run", "service", hostname, name, exitStatus, pluginOutput, perfData] -> processCheckResult'' hostname name exitStatus pluginOutput perfData >>= continueOrStop'
    ["del", "service", hostname, name] -> delService hostname name >>= continueOrStop'
    -- notification
    ["get", "notification", hostname, name] -> getNotification' hostname name >>= continueOrStop'
    ["get", "notification", hostname, servicename, name] -> getNotification hostname servicename name >>= continueOrStop'
    ["new", "notification", hostname, name, templates, command, users, states, vars] -> newNotification' hostname "" name templates command users states vars >>= continueOrStop'
    ["new", "notification", hostname, servicename, name, templates, command, users, states, vars] -> newNotification' hostname servicename name templates command users states vars >>= continueOrStop'
    ["set", "notification", hostname, name, vars] -> setNotificationVars' hostname "" name vars >>= continueOrStop'
    ["set", "notification", hostname, servicename, name, vars] -> setNotificationVars' hostname servicename name vars >>= continueOrStop'
    ["del", "notification", hostname, name] -> delNotification' hostname "" name >>= continueOrStop'
    ["del", "notification", hostname, servicename, name] -> delNotification' hostname servicename name >>= continueOrStop'
    _ -> continueOrStop (Left ("Unknown command: " ++ unwords args'))
  where
    continueOrStop' :: Bool -> App Bool
    continueOrStop' True = case args of Nothing -> process Nothing; Just _ -> return True
    continueOrStop' False = case args of Nothing -> process Nothing; Just _ -> return False
    continueOrStop :: Either String () -> App Bool
    continueOrStop (Left errMsg) = case args of Nothing -> liftIO (putStrLn errMsg >> usage) >> process Nothing; Just _ -> return False
    continueOrStop (Right _) = case args of Nothing -> process Nothing; Just _ -> return True
