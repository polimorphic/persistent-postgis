{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase #-}

module Database.Persist.Postgis.Geography.GeographyBase
    ( GeographyBase
        ( GeoPoint
        , GeoLineString
        , GeoPolygon
        , GeoMultiPoint
        , GeoMultiLineString
        , GeoMultiPolygon
        )
    , renderGeographyBase
    ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Web.HttpApiData
    ( FromHttpApiData, ToHttpApiData, parseUrlPiece, readTextData, showTextData, toUrlPiece )

import Database.Persist.Postgis.Geography.LineString
import Database.Persist.Postgis.Geography.MultiLineString
import Database.Persist.Postgis.Geography.MultiPoint
import Database.Persist.Postgis.Geography.MultiPolygon
import Database.Persist.Postgis.Geography.Point
import Database.Persist.Postgis.Geography.Polygon

data GeographyBase
    = GeoPoint Point
    | GeoLineString LineString
    | GeoPolygon Polygon
    | GeoMultiPoint MultiPoint
    | GeoMultiLineString MultiLineString
    | GeoMultiPolygon MultiPolygon
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance FromHttpApiData GeographyBase where
    parseUrlPiece = readTextData

instance ToHttpApiData GeographyBase where
    toUrlPiece = showTextData

renderGeographyBase :: GeographyBase -> String
renderGeographyBase = \case
    GeoPoint p -> renderPointFull p
    GeoLineString l -> renderLineStringFull l
    GeoPolygon p -> renderPolygonFull p
    GeoMultiPoint mp -> renderMultiPointFull mp
    GeoMultiLineString ml -> renderMultiLineStringFull ml
    GeoMultiPolygon mp -> renderMultiPolygonFull mp
