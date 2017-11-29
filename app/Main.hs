{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Lib
import RuleParser
import Data.Text.IO as TIO
import Data.ByteString.Lazy as B
import Control.Lens
import Data.Aeson.Lens

main :: IO ()
main = do
  ds <- B.readFile "./ext/data_source.json"
  rules <- parseRule <$> TIO.readFile "./ext/rules.txt"
  print "Working Memory"
  print $ ds^..key "employees" . values.key "salary"
  print "Rules"
  print rules
  return ()
