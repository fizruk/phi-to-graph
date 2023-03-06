module Main where

import qualified PhiToGraph.Interpret as Phi

main :: IO ()
main = do
  input <- getContents
  Phi.interpretIO input
