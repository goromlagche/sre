{-# LANGUAGE OverloadedStrings  #-}

module Lib where

import Data.Text as Text
import Data.Map.Strict as Map
import Data.Sequence as Seq

type Fact = Map Text Text
type WorkingMemory = Seq Fact
