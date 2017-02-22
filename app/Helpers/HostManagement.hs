module Helpers.HostManagement
  ( getHost
  , newHost''
  , newHost'
  , newHost
  , setHostChecks'
  , setHostChecks
  , setHostCommand''
  , setHostCommand'
  , setHostCommand
  , setHostVars'
  , setHostVars
  , delHost
  ) where

import App
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Map (fromList)
import Data.Maybe (isNothing)
import qualified Icinga as I
import qualified Icinga.Models.HostConfig as MHC
import Text.Read (readMaybe)

getHost :: String -> App Bool
getHost hostname = do
  setup <- toSetup <$> getConfig
  (result, rc) <- liftIO $ I.getHost setup hostname
  case result of
    Nothing -> liftIO $ putStrLn "" >> putStrLn "Nothing" >> putStrLn ""
    Just value -> liftIO $ putStrLn "" >> putStrLn value >> putStrLn ""
  return rc

newHost'' :: String -> String -> String -> String -> String -> String -> App Bool
newHost'' name templates displayName groups address checkCommand = newHost' name templates displayName groups address checkCommand "[]"

newHost' :: String -> String -> String -> String -> String -> String -> String -> App Bool
newHost' name templates displayName groups address checkCommand vars =
  case readMaybe groups :: Maybe [String] of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse groups: " ++ groups) >> return False
    Just groups' ->
      case readMaybe templates :: Maybe [String] of
        Nothing -> liftIO $ putStrLn ("ERROR: cannot parse templates: " ++ templates) >> return False
        Just templates' ->
          case readMaybe vars :: Maybe [(String, String)] of
            Nothing -> liftIO $ putStrLn ("ERROR: cannot parse vars: " ++ vars) >> return False
            Just vars' -> newHost name templates' displayName groups' address checkCommand vars'

newHost :: String -> [String] -> String -> [String] -> String -> String -> [(String, String)] -> App Bool
newHost name templates displayName groups address checkCommand vars = do
  setup <- toSetup <$> getConfig
  let hostConfig = MHC.mkHostConfig displayName groups address checkCommand vars
      mtemplates = case templates of [] -> Nothing; _ -> Just templates
  liftIO $ I.createHost setup name mtemplates hostConfig >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

setHostChecks' :: String -> String -> String -> String -> String -> String -> String -> App Bool
setHostChecks' name activeChecks passiveChecks perfData notifications eventHandler flapDetection = do
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
       setHostChecks name activeChecks' passiveChecks' perfData' notifications' eventHandler' flapDetection'

setHostChecks :: String -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> App Bool
setHostChecks name activeChecks passiveChecks perfData notifications eventHandler flapDetection = do
  setup <- toSetup <$> getConfig
  let hostConfig =
        MHC.defHostConfig
        { MHC.enableActiveChecks = Just activeChecks
        , MHC.enablePassiveChecks = Just passiveChecks
        , MHC.enablePerfData = Just perfData
        , MHC.enableNotifications = Just notifications
        , MHC.enableEventHandler = Just eventHandler
        , MHC.enableFlapping = Just flapDetection
        }
  liftIO $ I.modifyHost setup name hostConfig >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

setHostCommand'' :: String -> String -> String -> String -> String -> App Bool
setHostCommand'' name checkInterval retryInterval maxCheckAttempts checkCommand = setHostCommand' name checkInterval retryInterval maxCheckAttempts checkCommand "[]"

setHostCommand' :: String -> String -> String -> String -> String -> String -> App Bool
setHostCommand' name checkInterval retryInterval maxCheckAttempts checkCommand vars = do
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
       setHostCommand name checkInterval' retryInterval' maxCheckAttempts' checkCommand vars'

setHostCommand :: String -> Int -> Int -> Int -> String -> [(String, String)] -> App Bool
setHostCommand name checkInterval retryInterval maxCheckAttempts checkCommand vars = do
  setup <- toSetup <$> getConfig
  let hostConfig =
        MHC.defHostConfig
        { MHC.checkInterval = Just checkInterval
        , MHC.retryInterval = Just retryInterval
        , MHC.maxCheckAttempts = Just maxCheckAttempts
        , MHC.checkCommand = Just checkCommand
        , MHC.vars = Just $ fromList vars
        }
  liftIO $ I.modifyHost setup name hostConfig >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

setHostVars' :: String -> String -> App Bool
setHostVars' name vars =
  case readMaybe vars :: Maybe [(String, String)] of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse vars: " ++ vars) >> return False
    Just vars' -> setHostVars name vars'

setHostVars :: String -> [(String, String)] -> App Bool
setHostVars name vars = do
  setup <- toSetup <$> getConfig
  let hostConfig = MHC.defHostConfig { MHC.vars = Just $ fromList vars }
  liftIO $ I.modifyHost setup name hostConfig >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

delHost :: String -> App Bool
delHost hostname = do
  setup <- toSetup <$> getConfig
  liftIO $ I.deleteHost setup hostname >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc
