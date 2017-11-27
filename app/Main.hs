{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Lib
import DataSource

main :: IO ()
main = do
  wm <- parseEmployee <$> getJSON
  print wm
  return ()
