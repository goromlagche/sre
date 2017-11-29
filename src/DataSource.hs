{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module DataSource where

import qualified Data.Text                     as T
import qualified Data.Aeson                    as A
import qualified GHC.Generics                  as G
import qualified Data.ByteString.Lazy.Internal as BS
import Lib

data Employee = Employee
                { email  :: T.Text
                , salary :: T.Text
                , age    :: T.Text
                } deriving (Show, G.Generic, A.ToJSON, A.FromJSON)

parseEmployee :: BS.ByteString -> Maybe WorkingMemory
parseEmployee body = A.decode (body) :: Maybe WorkingMemory
