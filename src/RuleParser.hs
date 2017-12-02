{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module RuleParser where

import Lib
import qualified Data.Text            as T
import qualified Data.Attoparsec.Text as P
import Control.Applicative  as A
import qualified Data.Map.Strict      as Map
import Control.Lens

data Rule = Rule
            { _condition :: Condition
            , _action    :: Fact
            } deriving (Show)

data Condition = Condition
                 { _entity    :: T.Text
                 , _field     :: T.Text
                 , _comparator :: (T.Text, T.Text)
                 } deriving (Show)


makeLenses ''Rule
makeLenses ''Condition

rules :: P.Parser Rule
rules = do
  -- condition
  ent <- P.takeTill (== ' ')
  _ <- P.char ' '
  fld <- P.takeTill (== ' ')
  _ <- P.char ' '
  ord <- P.string "<=" <|> P.string ">="
         <|> P.string "<" <|> P.string ">" <|> P.string "="
  _ <- P.char ' '
  str <- P.takeTill (== ',')
  let cond = Condition ent fld (ord, str)
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

evalOrd :: T.Text -> [Ordering]
evalOrd "<" = [LT]
evalOrd ">" = [GT]
evalOrd "=" = [EQ]
evalOrd "<=" = [LT, EQ]
evalOrd ">=" = [GT, EQ]
evalOrd _ = undefined
