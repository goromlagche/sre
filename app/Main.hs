{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Lib
import DataSource
import RuleParser
import Data.Text.IO as TIO
import Data.ByteString.Lazy as B

main :: IO ()
main = do
  wm <- parseEmployee <$> B.readFile "./ext/data_source.json"
  rules <- parseRule <$> TIO.readFile "./ext/rules.txt"
  print "Working Memory"
  print wm
  print "Rules"
  print rules
  return ()
