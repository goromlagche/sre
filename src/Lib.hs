{-# LANGUAGE OverloadedStrings  #-}

module Lib where

import Data.Text as Text
import Data.Map.Strict as Map

type Fact = Map Text Text
-- type WorkingMemory = Seq Fact

-- type LHS = Fact -> Bool
-- type RHS = Fact -> WorkingMemory

-- checkRule :: WorkingMemory -> LHS -> WorkingMemory
-- checkRule wm r  = Seq.filter r wm

-- applyRule :: WorkingMemory -> RHS
-- applyRule wm f  = wm |> f

-- evalRule :: WorkingMemory -> LHS -> RHS
-- evalRule wm l r = applyRule (checkRule wm l) r
