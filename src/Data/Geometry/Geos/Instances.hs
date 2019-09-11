{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}

module Data.Geometry.Geos.Instances () where

import Data.Aeson (FromJSON, ToJSON)
import Data.Geometry.Geos.Geometry
    ( Coordinate(Coordinate2, Coordinate3)
    , LineString(LineString)
    , LinearRing(LinearRing)
    , MultiLineString(MultiLineString)
    , MultiPoint(MultiPoint)
    , MultiPolygon(MultiPolygon)
    , Point(Point)
    , Polygon(Polygon)
    )
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
