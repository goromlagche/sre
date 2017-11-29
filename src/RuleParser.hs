{-# LANGUAGE OverloadedStrings  #-}

module RuleParser where

import Lib
import qualified Data.Text            as T
import qualified Data.Attoparsec.Text as P
import qualified Control.Applicative  as A
import qualified Data.Map.Strict      as Map

data Rule = Rule
            { condition :: Condition
            , action    :: Fact
            } deriving (Show)

data Condition = Condition
                 { entity    :: T.Text
                 , field     :: T.Text
                 , comparator :: [([Ordering], T.Text)]
                 } deriving (Show)

rules :: P.Parser Rule
rules = do
  -- condition
  ent <- P.takeTill (== ' ')
  _ <- P.char ' '
  fld <- P.takeTill (== ' ')
  _ <- P.char ' '
  comperator <- P.takeTill (== ',')
  let cond = Condition ent fld (evalComp comperator)
  -- seperator
  _ <- P.string ", "
  -- action
  key <- P.takeTill (== ' ')
  _ <- P.string " is "
  value <- P.takeTill (== '\n')
  let act = Map.insert key value Map.empty
  _ <- P.string "\n"
  return $ Rule cond act

parseRule :: T.Text -> Either String [Rule]
parseRule = P.parseOnly (A.many rules)

-- ">= 250000 and < 500000" ==> [([GT,EQ],"250000"),([LT],"500000")]
evalComp :: T.Text -> [([Ordering], T.Text)]
evalComp compStr = map(makeTuple . T.splitOn " " . T.strip)
                    (T.splitOn "and" compStr)
  where
    makeTuple(x:y:_) = ((mapToOrd x), y)
    mapToOrd "<"  = [LT]
    mapToOrd "<=" = [LT, EQ]
    mapToOrd ">" = [GT]
    mapToOrd ">=" = [GT, EQ]
    mapToOrd "=" = [EQ]
