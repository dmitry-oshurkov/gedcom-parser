{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Lib
import JSON


main :: IO ()
main = do
    contents <- readFile "test/TGC55CLF-utf8.ged"
    let tags = splitContent contents

    let (people, families) = parseLine (head tags) (tail tags) ([], [])

    LC.putStrLn . encodePretty $ families
    LC.putStrLn . encodePretty $ people

    print (length people)
    print (length families)

    LC.putStrLn ""
    LC.putStrLn $ LC.concat [ "http://jsoneditoronline.org/?json=", encode people ]