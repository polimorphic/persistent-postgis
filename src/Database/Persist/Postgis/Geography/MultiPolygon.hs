{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Database.Persist.Postgis.Geography.MultiPolygon
    ( MultiPolygon(MultiPolygon), MultiPolygonDB
    , multiPolygonToDB, renderMultiPolygon, renderMultiPolygonFull
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

import Database.Persist.Postgis.Geography.Polygon

newtype MultiPolygon = MultiPolygon [Polygon]
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance FromHttpApiData MultiPolygon where
    parseUrlPiece = readTextData

instance ToHttpApiData MultiPolygon where
    toUrlPiece = pack . show

newtype MultiPolygonDB = MultiPolygonDB ByteString

renderMultiPolygon :: MultiPolygon -> String
renderMultiPolygon (MultiPolygon ps) = "(" ++ intercalate "), (" (renderPolygon <$> ps) ++ ")"

renderMultiPolygonFull :: MultiPolygon -> String
renderMultiPolygonFull mp = "MULTIPOLYGON(" ++ renderMultiPolygon mp ++ ")"

multiPolygonToDB :: MultiPolygon -> MultiPolygonDB
multiPolygonToDB = MultiPolygonDB . B.pack . renderMultiPolygonFull

instance PersistField MultiPolygonDB where
    toPersistValue (MultiPolygonDB x) = PersistDbSpecific x

    fromPersistValue (PersistDbSpecific x) = Right $ MultiPolygonDB x
    fromPersistValue _ = Left "MultiPolygonDB values must be converted from PersistDbSpecific"

instance PersistFieldSql MultiPolygonDB where
  sqlType _ = SqlOther "geography(MultiPolygon,4326)"
