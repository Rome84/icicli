module Helpers.StatusManagement where

import App
import Control.Monad.IO.Class (liftIO)
import qualified Icinga as I

status :: App Bool
status = do
  setup <- toSetup <$> getConfig
  liftIO $ print setup
  liftIO (I.checkStatus setup >>= \ (x, rc) -> putStrLn "" >> putStrLn x >> putStrLn "" >> return rc)
