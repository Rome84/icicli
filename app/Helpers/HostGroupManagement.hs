module Helpers.HostGroupManagement
  ( getHostGroup
  , newHostGroup''
  , newHostGroup'
  , newHostGroup
  , setHostGroup''
  , setHostGroup'
  , setHostGroup
  , delHostGroup
  ) where

import App
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Map (fromList)
import qualified Icinga as I
import qualified Icinga.Models.HostGroupConfig as MHGC
import Text.Read (readMaybe)

getHostGroup :: String -> App Bool
getHostGroup groupname = do
  setup <- toSetup <$> getConfig
  (result, rc) <- liftIO $ I.getHostGroup setup groupname
  case result of
    Nothing -> liftIO (putStrLn "" >> putStrLn "Nothing" >> putStrLn "")
    Just value -> liftIO (putStrLn "" >> putStrLn value >> putStrLn "")
  return rc

newHostGroup'' :: String -> String -> String -> String -> App Bool
newHostGroup'' name templates displayName groups = newHostGroup' name templates displayName groups "[]"

newHostGroup' :: String -> String -> String -> String -> String -> App Bool
newHostGroup' name templates displayName groups vars =
  case readMaybe templates :: Maybe [String] of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse templates: " ++ templates) >> return False
    Just templates' ->
      case readMaybe groups :: Maybe [String] of
        Nothing -> liftIO $ putStrLn ("ERROR: cannot parse groups: " ++ groups) >> return False
        Just groups' ->
          case readMaybe vars :: Maybe [(String, String)] of
            Nothing -> liftIO $ putStrLn ("ERROR: cannot parse vars: " ++ vars) >> return False
            Just vars' -> newHostGroup name templates' displayName groups' vars'

newHostGroup :: String -> [String] -> String -> [String] -> [(String, String)] -> App Bool
newHostGroup name templates displayName groups vars = do
  setup <- toSetup <$> getConfig
  let groupConfig = MHGC.mkHostGroupConfig displayName groups vars
      mtemplates = case templates of [] -> Nothing; _ -> Just templates
  liftIO (I.createHostGroup setup name mtemplates groupConfig >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc)

setHostGroup'' :: String -> String -> App Bool
setHostGroup'' name displayName = setHostGroup' name displayName "[]"

setHostGroup' :: String -> String -> String -> App Bool
setHostGroup' name displayName vars =
  case readMaybe vars :: Maybe [(String, String)] of
    Nothing -> liftIO $ putStrLn ("ERROR: cannot parse vars: " ++ vars) >> return False
    Just vars' -> setHostGroup name displayName vars'

setHostGroup :: String -> String -> [(String, String)] -> App Bool
setHostGroup name displayName vars = do
  setup <- toSetup <$> getConfig
  let config =
        MHGC.defHostGroupConfig
        { MHGC.displayName = Just displayName
        , MHGC.vars = Just $ fromList vars
        }
  liftIO (I.modifyHostGroup setup name config >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc)

delHostGroup :: String -> App Bool
delHostGroup groupname = do
  setup <- toSetup <$> getConfig
  liftIO (I.deleteHostGroup setup groupname >>= \ (x, rc) -> putStrLn "" >> print x >> putStrLn "" >> return rc)
