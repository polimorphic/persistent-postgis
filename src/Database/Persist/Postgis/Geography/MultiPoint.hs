{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}

module Database.Persist.Postgis.Geography.MultiPoint
    ( MultiPoint(MultiPoint), MultiPointDB, multiPointToDB, renderMultiPoint, renderMultiPointFull
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (pack)
import Data.List (intercalate)
import Database.Persist.Sql
    ( PersistField, PersistFieldSql, PersistValue(PersistDbSpecific), SqlType(SqlOther)
    , fromPersistValue, sqlType, toPersistValue
    )
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData, parseUrlPiece, readTextData, toUrlPiece)

import Database.Persist.Postgis.Geography.Point

newtype MultiPoint = MultiPoint [Point]
    deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

instance FromHttpApiData MultiPoint where
    parseUrlPiece = readTextData

instance ToHttpApiData MultiPoint where
    toUrlPiece = pack . show

newtype MultiPointDB = MultiPointDB ByteString

renderMultiPoint :: MultiPoint -> String
renderMultiPoint (MultiPoint ps) = "(" ++ intercalate "),(" (renderPoint <$> ps) ++ ")"

renderMultiPointFull :: MultiPoint -> String
renderMultiPointFull mp = "MULTIPOINT(" ++ renderMultiPoint mp ++ ")"

multiPointToDB :: MultiPoint -> MultiPointDB
multiPointToDB = MultiPointDB . B.pack . renderMultiPointFull

instance PersistField MultiPointDB where
    toPersistValue (MultiPointDB x) = PersistDbSpecific x

    fromPersistValue (PersistDbSpecific x) = Right $ MultiPointDB x
    fromPersistValue _ = Left "MultiPointDB values must be converted from PersistDbSpecific"

instance PersistFieldSql MultiPointDB where
    sqlType _ = SqlOther "geography(MultiPoint,4326)"
