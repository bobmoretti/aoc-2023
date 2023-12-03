module Main where

-- import Day02 (day02)

import Data.String (String)
import Day01 (day01)
import Day02 (day02)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  let val = read $ head args
  run val

run args =
  case args of
    1 -> day01
    2 -> day02
    _ -> error "None or invalid day number provided."