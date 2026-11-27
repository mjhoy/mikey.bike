module Main where

import LinkCheck (checkLinks)
import Site (site)
import Control.Monad (unless)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["check-links"] -> checkLinks "_site" >>= (`unless` exitFailure)
    _ -> site
