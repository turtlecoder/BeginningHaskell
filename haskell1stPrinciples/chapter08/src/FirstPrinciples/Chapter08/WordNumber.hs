{-# LANGUAGE DataKinds #-}

module FirstPrinciples.Chapter08.WordNumber(wordNumber) where

import Data.List (intersperse)
import Refined

-- Converts a single digit 
digitToWord :: Int  -> String
digitToWord n = let
  digitList = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  in
  digitList !! n

-- Converts 
digits :: Refined Positive Int -> [Int]
digits n = let
  goDigits :: Int -> [Int] -> [Int]
  goDigits 0 []    = [0]
  goDigits 0 accum = accum
  goDigits n accum = let
    divR = n `div` 10
    modR = n `mod` 10
    in
    goDigits divR (modR:accum)
  in
  goDigits (unrefine n) []


wordNumber :: Int -> Either String String
wordNumber n = do
  number <- (refine n )::(Either String (Refined Positive Int))
  digitList <- Right (digits number)
  let wordList = concat $ intersperse "-" $ fmap digitToWord digitList
  return wordList
