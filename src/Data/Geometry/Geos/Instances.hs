{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}

module Data.Geometry.Geos.Instances () where

import Data.Aeson (FromJSON, ToJSON)
import Data.Geometry.Geos.Types
    ( Coordinate(Coordinate2, Coordinate3)
    , LineString(LineString)
    , LinearRing(LinearRing)
    , MultiLineString(MultiLineString)
    , MultiPoint(MultiPoint)
    , MultiPolygon(MultiPolygon)
    , Point(Point)
    , Polygon(Polygon)
    )
import GHC.Generics (Generic)

deriving instance Generic Coordinate
deriving instance Generic Point
deriving instance Generic LineString
deriving instance Generic LinearRing
deriving instance Generic Polygon
deriving instance Generic MultiPoint
deriving instance Generic MultiLineString
deriving instance Generic MultiPolygon

deriving instance FromJSON Coordinate
deriving instance FromJSON Point
deriving instance FromJSON LineString
deriving instance FromJSON LinearRing
deriving instance FromJSON Polygon
deriving instance FromJSON MultiPoint
deriving instance FromJSON MultiLineString
deriving instance FromJSON MultiPolygon

deriving instance ToJSON Coordinate
deriving instance ToJSON Point
deriving instance ToJSON LineString
deriving instance ToJSON LinearRing
deriving instance ToJSON Polygon
deriving instance ToJSON MultiPoint
deriving instance ToJSON MultiLineString
deriving instance ToJSON MultiPolygon
