{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Database.Persist.Postgis.Geography.GeometryCollection
    ( GeometryCollection(GeometryCollection), GeometryCollectionDB
    , geometryCollectionToDB, renderGeometryCollection, renderGeometryCollectionFull
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

import Database.Persist.Postgis.Geography.GeographyBase

newtype GeometryCollection = GeometryCollection [GeographyBase]
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance FromHttpApiData GeometryCollection where
    parseUrlPiece = readTextData

instance ToHttpApiData GeometryCollection where
    toUrlPiece = pack . show

newtype GeometryCollectionDB = GeometryCollectionDB ByteString

renderGeometryCollection :: GeometryCollection -> String
renderGeometryCollection (GeometryCollection gs)
    = "(" ++ intercalate "), (" (renderGeographyBase <$> gs) ++ ")"

renderGeometryCollectionFull :: GeometryCollection -> String
renderGeometryCollectionFull gc = "GEOMETRYCOLLECTION(" ++ renderGeometryCollection gc ++ ")"

geometryCollectionToDB :: GeometryCollection -> GeometryCollectionDB
geometryCollectionToDB = GeometryCollectionDB . B.pack . renderGeometryCollectionFull

instance PersistField GeometryCollectionDB where
    toPersistValue (GeometryCollectionDB x) = PersistDbSpecific x

    fromPersistValue (PersistDbSpecific x) = Right $ GeometryCollectionDB x
    fromPersistValue _ = Left "GeometryCollectionDB values must be converted from PersistDbSpecific"

instance PersistFieldSql GeometryCollectionDB where
    sqlType _ = SqlOther "geography(GeometryCollection,4326)"
