{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Database.Persist.Postgis.Geography.MultiLineString
    ( MultiLineString(MultiLineString), MultiLineStringDB
    , multiLineStringToDB, renderMultiLineString, renderMultiLineStringFull
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (intercalate)
import Data.Text (pack)
import Database.Persist.Sql
    ( PersistField, PersistFieldSql, PersistValue(PersistDbSpecific), SqlType(SqlOther)
    , fromPersistValue, sqlType, toPersistValue
    )
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData, parseUrlPiece, readTextData, toUrlPiece)

import Database.Persist.Postgis.Geography.LineString

newtype MultiLineString = MultiLineString [LineString]
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance FromHttpApiData MultiLineString where
    parseUrlPiece = readTextData

instance ToHttpApiData MultiLineString where
    toUrlPiece = pack . show

newtype MultiLineStringDB = MultiLineStringDB ByteString

renderMultiLineString :: MultiLineString -> String
renderMultiLineString (MultiLineString ml)
    = "(" ++ intercalate "),(" (renderLineString <$> ml) ++ ")"

renderMultiLineStringFull :: MultiLineString -> String
renderMultiLineStringFull ml = "MULTILINESTRING(" ++ renderMultiLineString ml ++ ")"

multiLineStringToDB :: MultiLineString -> MultiLineStringDB
multiLineStringToDB = MultiLineStringDB . B.pack . renderMultiLineStringFull

instance PersistField MultiLineStringDB where
    toPersistValue (MultiLineStringDB x) = PersistDbSpecific x

    fromPersistValue (PersistDbSpecific x) = Right $ MultiLineStringDB x
    fromPersistValue _ = Left "MultiLineStringDB values must be converted from PersistDbSpecific"

instance PersistFieldSql MultiLineStringDB where
    sqlType _ = SqlOther "geography(MultiLineString,4326)"
