module Main where

import Data.List.Split
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson.Encode.Pretty
import Lib
import JSON


main :: IO ()
main = do
    contents <- readFile "test/TGC55CLF-utf8.ged"
    let lines = splitOn "\n" contents
    let tags = map parseTag lines

    let (people, families) = parseLine (head tags) (tail tags) ([], [])

    LC.putStrLn . encodePretty $ families
    LC.putStrLn . encodePretty $ people

    print (length people)
    print (length families)
