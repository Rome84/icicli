module App where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Control.Monad.State
import qualified Icinga as I

data Credentials
  = Credentials
  { credsUsername :: String
  , credsPassword :: String
  } deriving (Show, Eq)

data AppConfig
  = AppConfig
  { cfgUrl :: String
  , cfgCredentials :: Maybe Credentials
  , cfgRoutingKey :: Maybe String
  } deriving (Show, Eq)

data AppState
  = AppState
  { stLastError :: Maybe String
  } deriving (Show, Eq)

type App a = StateT AppState (ReaderT AppConfig IO) a

runApp :: AppConfig -> App a -> IO a
runApp cfg process = do
  let initState = AppState { stLastError = Nothing }
  fst <$> runReaderT (runStateT process initState) cfg

getConfig :: App AppConfig
getConfig = ask

mkConfig :: String -> String -> String -> String -> AppConfig
mkConfig url "" "" "" = AppConfig { cfgUrl = url, cfgCredentials = Nothing, cfgRoutingKey = Nothing }
mkConfig url "" "" routingKey = AppConfig { cfgUrl = url, cfgCredentials = Nothing, cfgRoutingKey = Just routingKey }
mkConfig url username password "" = AppConfig { cfgUrl = url, cfgCredentials = Just (Credentials username password), cfgRoutingKey = Nothing }
mkConfig url username password routingKey = AppConfig { cfgUrl = url, cfgCredentials = Just (Credentials username password), cfgRoutingKey = Just routingKey }

getState :: App AppState
getState = get

setState :: AppState -> App ()
setState = put

toSetup :: AppConfig -> I.Setup
toSetup config = do
  let url = cfgUrl config
      creds = cfgCredentials config
      creds' = case creds of Nothing -> Nothing; Just x -> Just $ I.Credentials (credsUsername x) (credsPassword x)
      routingKey = cfgRoutingKey config
  I.Setup url creds' routingKey
