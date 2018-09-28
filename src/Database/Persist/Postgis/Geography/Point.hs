{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Database.Persist.Postgis.Geography.Point
    ( Point(Point), PointDB, pointToDB, renderPoint, renderPointFull
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

data Point = Point
    { lon :: Double
    , lat :: Double
    } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance FromHttpApiData Point where
    parseUrlPiece = readTextData

instance ToHttpApiData Point where
    toUrlPiece = pack . show

newtype PointDB = PointDB ByteString

renderPoint :: Point -> String
renderPoint p = show (lon p) ++ " " ++ show (lat p)

renderPointFull :: Point -> String
renderPointFull p = "POINT(" ++ renderPoint p ++ ")"

pointToDB :: Point -> PointDB
pointToDB = PointDB . B.pack . renderPointFull

instance PersistField PointDB where
    toPersistValue (PointDB x) = PersistDbSpecific x

    fromPersistValue (PersistDbSpecific x) = Right $ PointDB x
    fromPersistValue _ = Left "PointDB values must be converted from PersistDbSpecific"

instance PersistFieldSql PointDB where
    sqlType _ = SqlOther "geography(Point,4326)"
