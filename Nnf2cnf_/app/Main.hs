module Main where

import Text.PropoLogic.Parser
import Text.PropoLogic.Data
import System.Environment(getArgs)
       
main :: IO ()
main = do
     args <- getArgs
     case args of
      [f] -> parseFile f
      _   -> putStrLn "Need file"
