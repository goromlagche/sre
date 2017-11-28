{-# LANGUAGE OverloadedStrings  #-}

module RuleParser where

import Lib
import Data.Text            as T
import Data.Attoparsec.Text as P
import Data.Text.IO         as TIO
import Data.ByteString.Lazy as B
import Control.Applicative
import Data.Map.Strict      as Map

data Rule = Rule
            { condition :: Condition
            , action    :: Fact
            } deriving (Show)

data Condition = Condition
                 { entity    :: T.Text
                 , field     :: T.Text
                 , comparator :: T.Text
                 } deriving (Show)

rules :: P.Parser Rule
rules = do
  -- condition
  entity <- P.takeTill (== ' ')
  _ <- P.char ' '
  field <- P.takeTill (== ' ')
  _ <- P.char ' '
  comparator <- P.takeTill (== ',')
  let condition = Condition entity field comparator
  -- seperator
  _ <- P.string ", "
  -- action
  key <- P.takeTill (== ' ')
  _ <- P.string " is "
  value <- P.takeTill (== '\n')
  let action = Map.insert key value Map.empty
  _ <- P.string "\n"
  return $ Rule condition action

parseRule = P.parseOnly (many rules)
