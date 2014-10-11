{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module Person where

import Data.Protobuf
--import Data.Protobuf.ProtoTemplate
import Data.ByteString (ByteString)

-- data NameT = Name { firstname :: Maybe ByteString, lastname :: Maybe ByteString }
--   deriving (Show, Eq)
-- protobuf "tests/person.pb.o" ''NameT

-- data PersonT = Person { name :: NameT, id :: Int, email :: Maybe ByteString }
--   deriving (Show, Eq)
-- protobuf "tests/person.pb.o" ''PersonT

protoFile "tests/person.proto"
protobuf "tests/person.pb.o" ''NameT
protobuf "tests/person.pb.o" ''PersonT

