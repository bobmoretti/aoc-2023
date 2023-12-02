module Day01 where

import Data.Char
import Data.List
import Data.Maybe
import Data.Maybe (catMaybes)
import Paths_aoc2023 (getDataFileName)
import Text.Printf (printf)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  printf "part 1: %d\n" (part1 inputLines)
  printf "part 2: %d\n" (part2 inputLines)

part1 :: [String] -> Int
part1 lines =
  let digits = filter isDigit
      firstAndLast line = read [head (digits line), last (digits line)]
   in sum $ map firstAndLast lines :: Int

nameToDigit :: String -> Maybe Int
nameToDigit name
  | "one" `isPrefixOf` name = Just 1
  | "two" `isPrefixOf` name = Just 2
  | "three" `isPrefixOf` name = Just 3
  | "four" `isPrefixOf` name = Just 4
  | "five" `isPrefixOf` name = Just 5
  | "six" `isPrefixOf` name = Just 6
  | "seven" `isPrefixOf` name = Just 7
  | "eight" `isPrefixOf` name = Just 8
  | "nine" `isPrefixOf` name = Just 9
  | otherwise = Nothing

enumerate = zip [0 ..]

part2 :: [String] -> Int
part2 lines =
  let parseSubstr s =
        if isDigit $ head s
          then Just $ digitToInt $ head s
          else nameToDigit s
      digits line = catMaybes [parseSubstr (drop n line) | (n, _) <- enumerate line]
      firstAndLast line = 10 * head (digits line) + last (digits line)
   in sum $ map firstAndLast lines :: Int
