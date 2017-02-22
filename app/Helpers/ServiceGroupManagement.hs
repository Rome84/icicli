module Helpers.ServiceGroupManagement
  ( getServiceGroup
  , newServiceGroup''
  , newServiceGroup'
  , newServiceGroup
  , setServiceGroup''
  , setServiceGroup'
  , setServiceGroup
  , delServiceGroup
  ) where

import App
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Map (fromList)
import qualified Icinga as I
import qualified Icinga.Models.ServiceGroupConfig as MSGC
import Text.Read (readMaybe)

getServiceGroup :: String -> App Bool
getServiceGroup name = do
  setup <- toSetup <$> getConfig
  (result, rc) <- liftIO $ I.getServiceGroup setup name
  case result of
    Nothing -> liftIO $ putStrLn "" >> putStrLn "Nothing" >> putStrLn ""
    Just value -> liftIO $ putStrLn "" >> putStrLn value >> putStrLn ""
  return rc

newServiceGroup'' :: String -> String -> String -> String -> App Bool
newServiceGroup'' name templates displayName groups = newServiceGroup' name templates displayName groups "[]"

newServiceGroup' :: String -> String -> String -> String -> String -> App Bool
newServiceGroup' name templates displayName groups vars =
  case readMaybe templates :: Maybe [String] of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse templates: " ++ templates) >> return False
    Just templates' ->
      case readMaybe groups :: Maybe [String] of
        Nothing -> liftIO $ putStrLn ("ERROR: cannot parse groups: " ++ groups) >> return False
        Just groups' ->
          case readMaybe vars :: Maybe [(String, String)] of
            Nothing -> liftIO $ putStrLn ("ERROR: cannot parse vars: " ++ vars) >> return False
            Just vars' -> newServiceGroup name templates' displayName groups' vars'

newServiceGroup :: String -> [String] -> String -> [String] -> [(String, String)] -> App Bool
newServiceGroup name templates displayName groups vars = do
  setup <- toSetup <$> getConfig
  let config = MSGC.mkServiceGroupConfig displayName groups vars
      mtemplates = case templates of [] -> Nothing; _ -> Just templates
  liftIO $ I.createServiceGroup setup name mtemplates config >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

setServiceGroup'' :: String -> String -> App Bool
setServiceGroup'' name displayName = setServiceGroup' name displayName "[]"

setServiceGroup' :: String -> String -> String -> App Bool
setServiceGroup' name displayName vars =
  case readMaybe vars :: Maybe [(String, String)] of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse vars: " ++ vars) >> return False
    Just vars' -> setServiceGroup name displayName vars'

setServiceGroup :: String -> String -> [(String, String)] -> App Bool
setServiceGroup name displayName vars = do
  setup <- toSetup <$> getConfig
  let config =
        MSGC.defServiceGroupConfig
        { MSGC.displayName = Just displayName
        , MSGC.vars = Just $ fromList vars
        }
  liftIO $ I.modifyServiceGroup setup name config >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc

delServiceGroup :: String -> App Bool
delServiceGroup name = do
  setup <- toSetup <$> getConfig
  liftIO $ I.deleteServiceGroup setup name >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc
