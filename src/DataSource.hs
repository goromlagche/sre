{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module DataSource where

import Data.Text as T
import Data.Map.Strict as Map
import Data.Sequence as Seq
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.ByteString.Lazy as B
import Lib

data Employee = Employee
                { email  :: T.Text
                , salary :: T.Text
                , age    :: T.Text
                } deriving (Show, Generic, ToJSON, FromJSON)

getJSON :: IO B.ByteString
getJSON = B.readFile "./ext/data_source.json"

parseEmployee :: BS.ByteString -> Maybe WorkingMemory
parseEmployee body = decode (body) :: Maybe WorkingMemory
