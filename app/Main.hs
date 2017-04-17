module Main where

import Lib
import Data.List.Split


main :: IO ()
main = do
    contents <- readFile "test/TGC55CLF-utf8.ged"
    let lines = splitOn "\n" contents
    let tags = map parseTag lines

    let (people, families) = parseLine (head tags) (tail tags) ([], [])

    mapM_ print people
    mapM_ print families
    print (length people)
    print (length families)