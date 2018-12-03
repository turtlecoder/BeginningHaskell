{-# LANGUAGE DataKinds #-}

module FirstPrinciples.Chapter08.WordNumber(wordNumber, digits, digitToWord) where

import Data.List (intersperse)
import Refined

-- Converts a single digit, but not a total function
-- Undefined behavior for Integers > 9, Integers < 0
digitToWord :: Int  -> String
digitToWord n = let
  digitList = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  in
  digitList !! n

-- Converts,
-- Examples 0 - [0]
--          12 - [1,2]
--          123 - [1,2,3]
-- Properties
-- - Always returns a non-empty list
-- - Always has the the same number of elemnts as the digits of a number
digits :: Refined (From 0) Int -> [Int]
digits n = let
  goDigits :: Int -> [Int] -> [Int]
  goDigits 0 []    = [0]
  goDigits 0 accum = accum
  goDigits n' accum = let
    divR = n' `div` 10
    modR = n' `mod` 10
    in
    goDigits divR (modR:accum)
  in
  goDigits (unrefine n) []


wordNumber :: Int -> Either RefineException String
wordNumber n = do
  number <- (refine n)::(Either RefineException (Refined (From 0) Int))
  digitList <- Right (digits number)
  let wordList = concat $ intersperse "-" $ fmap digitToWord digitList
  return wordList
