{-# LANGUAGE TemplateHaskell #-}
module Data.Protobuf.ProtoFile
  ( Int32, Int64
  , Word32, Word64
  , ByteString
  , protoFile
  ) where

-- Types we use for field types and need to re-export
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)

import Control.Monad (mplus)
import Data.Char (toUpper)
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (toList)
import Data.Maybe (maybe, fromMaybe, fromJust)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH

import Text.DescriptorProtos.FileDescriptorProto as FDP
import Text.DescriptorProtos.DescriptorProto as DP
import Text.DescriptorProtos.FieldDescriptorProto as Field
import Text.DescriptorProtos.FieldDescriptorProto.Label as Label
import Text.DescriptorProtos.FieldDescriptorProto.Type as PType
import Text.Parsec.Error (ParseError)
import Text.ProtocolBuffers.ProtoCompile.Parser (parseProto)
import Text.ProtocolBuffers.Basic (uToString)

protoFile :: FilePath -> Q [Dec]
protoFile fn = do fdp <- runIO $ proto fn
                  case fdp of
                    Left  e -> error $ "protoFile: no parse.\n" ++ (show e)
                    Right fdp -> return . toList $ fmap dProtoDecl (message_type fdp)

dProtoDecl :: DescriptorProto -> Dec
dProtoDecl dp = let n = maybe err uToString (DP.name dp)
                    tn = mkName $ capT n
                    cn = mkName n
                    fs = field dp
                    fds = fmap fieldDecl fs
                    err = error "protoFile: anonymous messages unsupported."
                in DataD [] tn [] [(RecC cn (toList fds))] [''Show, ''Eq]

fieldDecl :: FieldDescriptorProto -> VarStrictType
fieldDecl f = (mkName fn, NotStrict, tWithMods)
  where -- TODO: field names are apparently optional, how to handle?
        err = error $ "Could not determine type for field " ++ fn
        fn = uToString $ fromJust (Field.name f)
        t = fromMaybe err $ primT `mplus` protoT
        primT = type' f >>= primType
        protoT = type_name f >>= Just . ConT . mkName . capT . uToString
        -- Field labels interpreted as a, Maybe a or [a]
        tWithMods = case fromMaybe LABEL_REQUIRED (Field.label f) of
                      LABEL_REQUIRED -> t
                      LABEL_OPTIONAL -> AppT (ConT ''Maybe) t
                      LABEL_REPEATED -> AppT ListT t
                      -- TODO: Parser doesn't support PACKEDREPEATED (pb 2.1.0)

-- | Map protobuf field types to Haskell types.
-- TODO: Some of these are not quite correct, particularly signed ints.
-- See https://developers.google.com/protocol-buffers/docs/encoding
primType :: PType.Type -> Maybe TH.Type
primType TYPE_MESSAGE = Nothing -- will show up in type_name
primType TYPE_GROUP = error "Group fields are not supported"
primType t = Just . ConT $ case t of
                             TYPE_STRING   -> ''ByteString
                             TYPE_INT32    -> ''Int32
                             TYPE_INT64    -> ''Int64
                             TYPE_BOOL     -> ''Bool
                             TYPE_UINT64   -> ''Word64
                             TYPE_UINT32   -> ''Word32
                             TYPE_FLOAT    -> ''Float
                             TYPE_DOUBLE   -> ''Double
                             -- Wrong or unimplemented below
                             TYPE_SINT32   -> ''Int32 -- arrives ZigZag-encoded
                             TYPE_SINT64   -> ''Int64 -- likewise
                             TYPE_BYTES    -> ''ByteString
                             -- TYPE_ENUM     -> catch these earlier and turn into Dec
                             -- TYPE_FIXED64  -> Data.Fixed or Data.Binary.Fixed
                             -- TYPE_FIXED32  ->   would work here, probably
                             -- TYPE_SFIXED32 ->
                             -- TYPE_SFIXED64 ->

-- | Type name from protobuf message type name
capT :: String -> String
capT (x:xs) = toUpper x : xs ++ "T"
capT [] = error "Unnamed message type"

-- shorthand
proto :: FilePath -> IO (Either ParseError FileDescriptorProto)
proto a = LB.readFile a >>= return . parseProto a

