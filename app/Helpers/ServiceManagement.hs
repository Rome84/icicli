module Helpers.ServiceManagement
  ( getService
  , newService''
  , newService'
  , newService
  , setServiceChecks'
  , setServiceChecks
  , setServiceCommand''
  , setServiceCommand'
  , setServiceCommand
  , setServiceVars'
  , setServiceVars
  , delService
  , processCheckResult'''
  , processCheckResult''
  , processCheckResult'
  , processCheckResult
  ) where

import App
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Map (fromList)
import Data.Maybe (isNothing)
import qualified Icinga as I
import qualified Icinga.Models.ServiceConfig as MSC
import qualified Icinga.Models.ProcessCheckResult as MPCR
import Text.Read (readMaybe)

getService :: String -> String -> App Bool
getService hostname serviceName = do
  setup <- toSetup <$> getConfig
  (result, rc) <- liftIO $ I.getService setup hostname serviceName
  case result of
    Nothing -> liftIO $ putStrLn "" >> putStrLn "Nothing" >> putStrLn ""
    Just value -> liftIO $ putStrLn "" >> putStrLn value >> putStrLn ""
  return rc

newService'' :: String -> String -> String -> String -> String -> String -> App Bool
newService'' hostname name templates displayName groups checkCommand = newService' hostname name templates displayName groups checkCommand "[]"

newService' :: String -> String -> String -> String -> String -> String -> String -> App Bool
newService' hostname name templates displayName groups checkCommand vars =
  case readMaybe groups :: Maybe [String] of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse groups: " ++ groups) >> return False
    Just groups' ->
      case readMaybe templates :: Maybe [String] of
        Nothing -> liftIO $ putStrLn ("ERROR: cannot parse templates: " ++ templates) >> return False
        Just templates' ->
          case readMaybe vars :: Maybe [(String, String)] of
            Nothing -> liftIO $ putStrLn ("ERROR: cannot parse vars: " ++ vars) >> return False
            Just vars' -> newService hostname name templates' displayName groups' checkCommand vars'

newService :: String -> String -> [String] -> String -> [String] -> String -> [(String, String)] -> App Bool
newService hostname name templates displayName groups checkCommand vars = do
  setup <- toSetup <$> getConfig
  let config = MSC.mkServiceConfig displayName hostname groups checkCommand vars
      mtemplates = case templates of [] -> Nothing; _ -> Just templates
  liftIO $ I.createService setup hostname name mtemplates config >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

setServiceChecks' :: String -> String -> String -> String -> String -> String -> String -> String -> App Bool
setServiceChecks' hostname name activeChecks passiveChecks perfData notifications eventHandler flapDetection = do
  let xs =
        [ readMaybe activeChecks :: Maybe Bool
        , readMaybe passiveChecks :: Maybe Bool
        , readMaybe perfData :: Maybe Bool
        , readMaybe notifications :: Maybe Bool
        , readMaybe eventHandler :: Maybe Bool
        , readMaybe flapDetection :: Maybe Bool
        ]
  if any isNothing xs
     then liftIO $ putStrLn "ERROR: cannot parse one of the boolean values (valid values: True|False)" >> return False
     else do
       let [Just activeChecks', Just passiveChecks', Just perfData', Just notifications', Just eventHandler', Just flapDetection'] = xs
       setServiceChecks hostname name activeChecks' passiveChecks' perfData' notifications' eventHandler' flapDetection'

setServiceChecks :: String -> String -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> App Bool
setServiceChecks hostname name activeChecks passiveChecks perfData notifications eventHandler flapDetection = do
  setup <- toSetup <$> getConfig
  let config =
        MSC.defServiceConfig
        { MSC.enableActiveChecks = Just activeChecks
        , MSC.enablePassiveChecks = Just passiveChecks
        , MSC.enablePerfData = Just perfData
        , MSC.enableNotifications = Just notifications
        , MSC.enableEventHandler = Just eventHandler
        , MSC.enableFlapping = Just flapDetection
        }
  liftIO $ I.modifyService setup hostname name config >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

setServiceCommand'' :: String -> String -> String -> String -> String -> String -> App Bool
setServiceCommand'' hostname name checkInterval retryInterval maxCheckAttempts checkCommand = setServiceCommand' hostname name checkInterval retryInterval maxCheckAttempts checkCommand "[]"

setServiceCommand' :: String -> String -> String -> String -> String -> String -> String -> App Bool
setServiceCommand' hostname name checkInterval retryInterval maxCheckAttempts checkCommand vars = do
  let xs =
        [ readMaybe checkInterval :: Maybe Int
        , readMaybe retryInterval :: Maybe Int
        , readMaybe maxCheckAttempts :: Maybe Int
        ]
      ys = [readMaybe vars :: Maybe [(String, String)]]
  if any isNothing xs || any isNothing ys
     then liftIO $ putStrLn "ERROR: cannot parse one of the values" >> return False
     else do
       let [Just checkInterval', Just retryInterval', Just maxCheckAttempts'] = xs
           [Just vars'] = ys
       setServiceCommand hostname name checkInterval' retryInterval' maxCheckAttempts' checkCommand vars'

setServiceCommand :: String -> String -> Int -> Int -> Int -> String -> [(String, String)] -> App Bool
setServiceCommand hostname name checkInterval retryInterval maxCheckAttempts checkCommand vars = do
  setup <- toSetup <$> getConfig
  let config =
        MSC.defServiceConfig
        { MSC.checkInterval = Just checkInterval
        , MSC.retryInterval = Just retryInterval
        , MSC.maxCheckAttempts = Just maxCheckAttempts
        , MSC.checkCommand = Just checkCommand
        , MSC.vars = Just $ fromList vars
        }
  liftIO $ I.modifyService setup hostname name config >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

setServiceVars' :: String -> String -> String -> App Bool
setServiceVars' hostname name vars =
  case readMaybe vars :: Maybe [(String, String)] of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse vars: " ++ vars) >> return False
    Just vars' -> setServiceVars hostname name vars'

setServiceVars :: String -> String -> [(String, String)] -> App Bool
setServiceVars hostname name vars = do
  setup <- toSetup <$> getConfig
  let config = MSC.defServiceConfig { MSC.vars = Just $ fromList vars }
  liftIO $ I.modifyService setup hostname name config >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

delService :: String -> String -> App Bool
delService hostname name = do
  setup <- toSetup <$> getConfig
  liftIO $ I.deleteService setup hostname name >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

processCheckResult''' :: String -> String -> String -> String -> App Bool
processCheckResult''' hostname name exitStatus pluginOutput = processCheckResult' hostname name exitStatus pluginOutput "[]" "[]" ""

processCheckResult'' :: String -> String -> String -> String -> String -> App Bool
processCheckResult'' hostname name exitStatus pluginOutput perfData = processCheckResult' hostname name exitStatus pluginOutput perfData "[]" ""

processCheckResult' :: String -> String -> String -> String -> String -> String -> String -> App Bool
processCheckResult' hostname name exitStatus pluginOutput perfData checkCommand checkSource =
  case readMaybe exitStatus :: Maybe Int of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse exit status: " ++ exitStatus) >> return False
    Just exitStatus' ->
      case readMaybe perfData :: Maybe [String] of
        Nothing -> liftIO $ putStrLn ("ERROR: cannot parse performance data: " ++ perfData) >> return False
        Just perfData' ->
          case readMaybe checkCommand :: Maybe [String] of
            Nothing -> liftIO $ putStrLn ("ERROR: cannot parse check command: " ++ checkCommand) >> return False
            Just checkCommand' -> processCheckResult hostname name exitStatus' pluginOutput perfData' checkCommand' checkSource

processCheckResult :: String -> String -> Int -> String -> [String] -> [String] -> String -> App Bool
processCheckResult hostname name exitStatus pluginOutput perfData checkCommand checkSource = do
  setup <- toSetup <$> getConfig
  let perfData' = case perfData of [] -> Nothing; _ -> Just perfData
      checkCommand' = case checkCommand of [] -> Nothing; _ -> Just checkCommand
      checkSource' = if null checkSource then Nothing; else Just checkSource
      result = MPCR.ProcessCheckResult exitStatus pluginOutput perfData' checkCommand' checkSource'
  liftIO $ I.processCheckResult setup hostname name result >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc
