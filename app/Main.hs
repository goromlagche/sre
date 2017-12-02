{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Lib
import RuleParser
import Data.Text.IO as TIO
import Data.Text as T
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Data.Aeson.Lens
import Data.Map as Map
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)

main :: IO ()
main = do
  workingMemory <- B.readFile "./ext/data_source.json"
  rules <- parseRule <$> TIO.readFile "./ext/rules.txt"
  -- print $ workingMemory^@..members
  -- print "Rules"
  -- pPrint rules
  pPrint $ applyRules rules (workingMemory^@..members)
  return ()

applyRules ruleList workingMemory =
  case ruleList of
    Left e -> error e
    Right rules ->
      Prelude.map runRules rules
  where
    runRules rule = Prelude.map (\wm -> applyRule rule wm) workingMemory
    applyRule rule (jsonEntity, t) =
      if (e == jsonEntity)
      then ruleCompare comp (t ^..values . key f . _String) act
      else error "rule entity and json entity does not match"
      where
        e = rule ^. (condition . entity)
        f = rule ^. (condition . field)
        comp = rule ^. (condition . comparator)
        act = rule ^. action

ruleCompare :: (T.Text, T.Text) -> [T.Text] -> Fact -> [Fact]
ruleCompare (ord, value) rVal act  =
  Prelude.map(\s ->
                 if (compareCustom (evalOrd(ord)) s (value))
                 then Map.insert "comperator" (T.pack $ show ord) (Map.insert "s" s (Map.insert "value" value act))
                 else Map.empty) rVal

compareCustom :: [Ordering] -> T.Text -> T.Text -> Bool
compareCustom ordList l r =
  case (readMaybe (T.unpack l) :: Maybe Integer) of
    Nothing -> elem (compare l r) ordList
    Just mA -> case (readMaybe (T.unpack r) :: Maybe Integer) of
      Nothing -> elem (compare l r) ordList
      Just mB -> elem (compare mA mB) ordList
