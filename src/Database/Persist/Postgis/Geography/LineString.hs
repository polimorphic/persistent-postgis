{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Database.Persist.Postgis.Geography.LineString
    ( LineString(LineString), LineStringDB, lineStringToDB, renderLineString, renderLineStringFull
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

import Database.Persist.Postgis.Geography.Point

newtype LineString = LineString [Point]
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance FromHttpApiData LineString where
    parseUrlPiece = readTextData

instance ToHttpApiData LineString where
    toUrlPiece = pack . show

newtype LineStringDB = LineStringDB ByteString

renderLineString :: LineString -> String
renderLineString (LineString ps) = intercalate "," $ renderPoint <$> ps

renderLineStringFull :: LineString -> String
renderLineStringFull ls = "LINESTRING(" ++ renderLineString ls ++ ")"

lineStringToDB :: LineString -> LineStringDB
lineStringToDB = LineStringDB . B.pack . renderLineStringFull

instance PersistField LineStringDB where
    toPersistValue (LineStringDB x) = PersistDbSpecific x

    fromPersistValue (PersistDbSpecific x) = Right $ LineStringDB x
    fromPersistValue _ = Left "LineStringDB values must be converted from PersistDbSpecific"

instance PersistFieldSql LineStringDB where
    sqlType _ = SqlOther "geography(LineString,4326)"
