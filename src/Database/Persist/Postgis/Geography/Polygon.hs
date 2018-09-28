{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Database.Persist.Postgis.Geography.Polygon
    ( Polygon(Polygon), PolygonDB, polygonToDB, renderPolygon, renderPolygonFull
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

newtype Polygon = Polygon [LineString]
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance FromHttpApiData Polygon where
    parseUrlPiece = readTextData

instance ToHttpApiData Polygon where
    toUrlPiece = pack . show

newtype PolygonDB = PolygonDB ByteString

renderPolygon :: Polygon -> String
renderPolygon (Polygon ls) = "(" ++ intercalate "),(" (renderLineString <$> ls) ++ ")"

renderPolygonFull :: Polygon -> String
renderPolygonFull p = "POLYGON(" ++ renderPolygon p ++ ")"

polygonToDB :: Polygon -> PolygonDB
polygonToDB = PolygonDB . B.pack . renderPolygonFull

instance PersistField PolygonDB where
    toPersistValue (PolygonDB x) = PersistDbSpecific x

    fromPersistValue (PersistDbSpecific x) = Right $ PolygonDB x
    fromPersistValue _ = Left "PolygonDB values must be converted from PersistDbSpecific"

instance PersistFieldSql PolygonDB where
    sqlType _ = SqlOther "geography(Polygon,4326)"
