module Day02 where

import Data.List.Extra (splitOn)
import Data.Text (strip)
import Debug.Trace (trace)
import Paths_aoc2023 (getDataFileName)
import Text.Printf (printf)

data Trial = Trial {red :: Int, green :: Int, blue :: Int}

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  printf "part 1: %d\n" (part1 inputLines)
  printf "part 2: %d\n" (part2 inputLines)

part1 :: [String] -> Int
part1 lines = sum [trialScore (n, t) | (n, t) <- [parseGame line | line <- lines]]

part2 :: [String] -> Int
part2 lines = sum [product $ trialVals t | (n, t) <- [parseGame line | line <- lines]]

trialVals :: Trial -> [Int]
trialVals t = map ($ t) [red, green, blue]

singleGroupTrial :: String -> Trial
singleGroupTrial txt =
  let tokens = words txt
      n = read $ head tokens
   in case last tokens of
        "red" -> Trial {red = n, green = 0, blue = 0}
        "green" -> Trial {red = 0, green = n, blue = 0}
        "blue" -> Trial {red = 0, green = 0, blue = n}

parseTrial :: String -> Trial
parseTrial txt =
  let groups = splitOn "," txt
      groupsAsTrials = [singleGroupTrial group | group <- groups]
   in Trial
        { red = sum [red g | g <- groupsAsTrials],
          green = sum [green g | g <- groupsAsTrials],
          blue = sum [blue g | g <- groupsAsTrials]
        }

parseGameId :: String -> Int
parseGameId txt =
  let tokens = splitOn " " txt
   in read (tokens !! 1)

maxTrial :: Trial -> Trial -> Trial
maxTrial a b =
  Trial
    { red = max (red a) (red b),
      green = max (green a) (green b),
      blue = max (blue a) (blue b)
    }

isValid :: Trial -> Bool
isValid t = red t <= 12 && green t <= 13 && blue t <= 14

trialScore :: (Int, Trial) -> Int
trialScore (id, t) = if isValid t then id else 0

parseGame :: String -> (Int, Trial)
parseGame txt =
  let tokens = splitOn ":" txt
      gameId = parseGameId $ head tokens
      trialStr = tokens !! 1
      trial = foldl1 maxTrial [parseTrial t | t <- splitOn ";" trialStr]
   in (gameId, trial)
