{-# LANGUAGE CPP #-}

module Helpers.Tracing where

import qualified Data.List as DL
import Debug.Trace (traceIO)

debug :: String -> IO ()
#ifdef DEBUG
debug msg = traceIO $ "\nDEBUG:\n" ++ DL.unlines (DL.map ("  " ++) (DL.lines msg))
#else
debug _ = return ()
#endif
