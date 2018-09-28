{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Database.Persist.Postgis.Geography.Geography
    ( Geography(GeoBase, GeoCollection), GeographyDB
    , geometryToDB, renderGeography
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (pack)
import Database.Persist.Sql
    ( PersistField, PersistFieldSql, PersistValue(PersistDbSpecific), SqlType(SqlOther)
    , fromPersistValue, sqlType, toPersistValue
    )
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData, parseUrlPiece, readTextData, toUrlPiece)

import Database.Persist.Postgis.Geography.GeographyBase
import Database.Persist.Postgis.Geography.GeometryCollection

data Geography
    = GeoBase GeographyBase
    | GeoCollection GeometryCollection
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance FromHttpApiData Geography where
    parseUrlPiece = readTextData

instance ToHttpApiData Geography where
    toUrlPiece = pack . show

newtype GeographyDB = GeographyDB ByteString

renderGeography :: Geography -> String
renderGeography (GeoBase gb) = renderGeographyBase gb
renderGeography (GeoCollection gc) = renderGeometryCollectionFull gc

geometryToDB :: Geography -> GeographyDB
geometryToDB = GeographyDB . B.pack . renderGeography

instance PersistField GeographyDB where
    toPersistValue (GeographyDB x) = PersistDbSpecific x

    fromPersistValue (PersistDbSpecific x) = Right $ GeographyDB x
    fromPersistValue _ = Left "GeographyDB values must be converted from PersistDbSpecific"

instance PersistFieldSql GeographyDB where
    sqlType _ = SqlOther "geography"
