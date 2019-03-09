{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, StandaloneDeriving #-}

module Database.Persist.Postgis.Geography
    ( Geography
        ( PointGeography
        , LineStringGeography
        , LinearRingGeography
        , PolygonGeography
        , MultiPointGeography
        , MultiLineStringGeography
        , MultiPolygonGeography
        )
    ) where

import Data.Aeson
    ( FromJSON, ToJSON, eitherDecode, encode, object, parseJSON, toJSON, withObject, (.:), (.=) )
import Data.Bifunctor (first)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Geometry.Geos.Instances ()
import Data.Geometry.Geos.Serialize (readHex, writeHex)
import Data.Geometry.Geos.Types
    ( Geometry
        ( LineStringGeometry
        , LinearRingGeometry
        , MultiLineStringGeometry
        , MultiPointGeometry
        , MultiPolygonGeometry
        , PointGeometry
        , PolygonGeometry
        )
    , LineString
    , LinearRing
    , MultiLineString
    , MultiPoint
    , MultiPolygon
    , Point
    , Polygon
    , Some(Some)
    )
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8Builder)
import Database.Persist.Sql
    ( PersistField, PersistFieldSql, PersistValue(PersistDbSpecific), SqlType(SqlOther)
    , fromPersistValue, sqlType, toPersistValue
    )
import Web.HttpApiData (FromHttpApiData, ToHttpApiData, parseUrlPiece, toUrlPiece)


data Geography a where
    PointGeography :: Point -> Geography Point
    LineStringGeography :: LineString -> Geography LineString
    LinearRingGeography :: LinearRing -> Geography LinearRing
    PolygonGeography :: Polygon -> Geography Polygon
    MultiPointGeography :: MultiPoint -> Geography MultiPoint
    MultiLineStringGeography :: MultiLineString -> Geography MultiLineString
    MultiPolygonGeography :: MultiPolygon -> Geography MultiPolygon

deriving instance Eq (Geography a)
instance Eq (Some Geography) where
    Some (PointGeography a) == Some (PointGeography b) = a == b
    Some (LineStringGeography a) == Some (LineStringGeography b) = a == b
    Some (LinearRingGeography a) == Some (LinearRingGeography b) = a == b
    Some (PolygonGeography a) == Some (PolygonGeography b) = a == b
    Some (MultiPointGeography a) == Some (MultiPointGeography b) = a == b
    Some (MultiLineStringGeography a) == Some (MultiLineStringGeography b) = a == b
    Some (MultiPolygonGeography a) == Some (MultiPolygonGeography b) = a == b
    _ == _ = False

deriving instance Show (Geography a)
deriving instance Show (Some Geography)


instance FromJSON (Geography Point) where
    parseJSON v = parseJSON v >>= \case
        Some (g@(PointGeography _)) -> pure g
        _ -> fail "Expected a Point Geography"

instance FromJSON (Geography LineString) where
    parseJSON v = parseJSON v >>= \case
        Some (g@(LineStringGeography _)) -> pure g
        _ -> fail "Expected a LineString Geography"

instance FromJSON (Geography LinearRing) where
    parseJSON v = parseJSON v >>= \case
        Some (g@(LinearRingGeography _)) -> pure g
        _ -> fail "Expected a LinearRing Geography"

instance FromJSON (Geography Polygon) where
    parseJSON v = parseJSON v >>= \case
        Some (g@(PolygonGeography _)) -> pure g
        _ -> fail "Expected a Polygon Geography"

instance FromJSON (Geography MultiPoint) where
    parseJSON v = parseJSON v >>= \case
        Some (g@(MultiPointGeography _)) -> pure g
        _ -> fail "Expected a MultiPoint Geography"

instance FromJSON (Geography MultiLineString) where
    parseJSON v = parseJSON v >>= \case
        Some (g@(MultiLineStringGeography _)) -> pure g
        _ -> fail "Expected a MultiLineString Geography"

instance FromJSON (Geography MultiPolygon) where
    parseJSON v = parseJSON v >>= \case
        Some (g@(MultiPolygonGeography _)) -> pure g
        _ -> fail "Expected a MultiPolygon Geography"

instance FromJSON (Some Geography) where
    parseJSON = withObject "Geography Point" $ \v -> do
        tag <- v .: "tag"
        case tag of
            "PointGeography" -> Some . PointGeography <$> v .: "contents"
            "LineStringGeography" -> Some . LineStringGeography <$> v .: "contents"
            "LinearRingGeography" -> Some . LinearRingGeography <$> v .: "contents"
            "PolygonGeography" -> Some . PolygonGeography <$> v .: "contents"
            "MultiPointGeography" -> Some . MultiPointGeography <$> v .: "contents"
            "MultiLineStringGeography" -> Some . MultiLineStringGeography <$> v .: "contents"
            "MultiPolygonGeography" -> Some . MultiPolygonGeography <$> v .: "contents"
            _ -> fail $ "Unkown tag: " ++ tag


instance ToJSON (Geography a) where
    toJSON (PointGeography p) =
        object ["tag" .= ("PointGeography" :: String), "contents" .= p]
    toJSON (LineStringGeography ls) =
        object ["tag" .= ("LineStringGeography" :: String), "contents" .= ls]
    toJSON (LinearRingGeography lr) =
        object ["tag" .= ("LinearRingGeography" :: String), "contents" .= lr]
    toJSON (PolygonGeography pl) =
        object ["tag" .= ("PolygonGeography" :: String), "contents" .= pl]
    toJSON (MultiPointGeography mp) =
        object ["tag" .= ("MultiPointGeography" :: String), "contents" .= mp]
    toJSON (MultiLineStringGeography mls) =
        object ["tag" .= ("MultiLineStringGeography" :: String), "contents" .= mls]
    toJSON (MultiPolygonGeography mpl) =
        object ["tag" .= ("MultiPolygonGeography" :: String), "contents" .= mpl]

instance ToJSON (Some Geography) where
    toJSON (Some g) = toJSON g


instance FromHttpApiData (Geography Point) where
    parseUrlPiece = first pack . eitherDecode . toLazyByteString . encodeUtf8Builder

instance FromHttpApiData (Geography LineString) where
    parseUrlPiece = first pack . eitherDecode . toLazyByteString . encodeUtf8Builder

instance FromHttpApiData (Geography LinearRing) where
    parseUrlPiece = first pack . eitherDecode . toLazyByteString . encodeUtf8Builder

instance FromHttpApiData (Geography Polygon) where
    parseUrlPiece = first pack . eitherDecode . toLazyByteString . encodeUtf8Builder

instance FromHttpApiData (Geography MultiPoint) where
    parseUrlPiece = first pack . eitherDecode . toLazyByteString . encodeUtf8Builder

instance FromHttpApiData (Geography MultiLineString) where
    parseUrlPiece = first pack . eitherDecode . toLazyByteString . encodeUtf8Builder

instance FromHttpApiData (Geography MultiPolygon) where
    parseUrlPiece = first pack . eitherDecode . toLazyByteString . encodeUtf8Builder

instance FromHttpApiData (Some Geography) where
    parseUrlPiece = first pack . eitherDecode . toLazyByteString . encodeUtf8Builder


instance ToHttpApiData (Geography a) where
    toUrlPiece = decodeUtf8 . toStrict . encode

instance ToHttpApiData (Some Geography) where
    toUrlPiece = decodeUtf8 . toStrict . encode


instance PersistField (Geography Point) where
    toPersistValue g = toPersistValue $ Some g
    fromPersistValue v = fromPersistValue v >>= \case
        Some (g@(PointGeography _)) -> Right g
        _ -> Left "Invalid Geography Point hex"

instance PersistField (Geography LineString) where
    toPersistValue g = toPersistValue $ Some g
    fromPersistValue v = fromPersistValue v >>= \case
        Some (g@(LineStringGeography _)) -> Right g
        _ -> Left "Invalid Geography LineString hex"

instance PersistField (Geography LinearRing) where
    toPersistValue g = toPersistValue $ Some g
    fromPersistValue v = fromPersistValue v >>= \case
        Some (g@(LinearRingGeography _)) -> Right g
        _ -> Left "Invalid Geography LinearRing hex"

instance PersistField (Geography Polygon) where
    toPersistValue g = toPersistValue $ Some g
    fromPersistValue v = fromPersistValue v >>= \case
        Some (g@(PolygonGeography _)) -> Right g
        _ -> Left "Invalid Geography Polygon hex"

instance PersistField (Geography MultiPoint) where
    toPersistValue g = toPersistValue $ Some g
    fromPersistValue v = fromPersistValue v >>= \case
        Some (g@(MultiPointGeography _)) -> Right g
        _ -> Left "Invalid Geography MultiPoint hex"

instance PersistField (Geography MultiLineString) where
    toPersistValue g = toPersistValue $ Some g
    fromPersistValue v = fromPersistValue v >>= \case
        Some (g@(MultiLineStringGeography _)) -> Right g
        _ -> Left "Invalid Geography MultiLineString hex"

instance PersistField (Geography MultiPolygon) where
    toPersistValue g = toPersistValue $ Some g
    fromPersistValue v = fromPersistValue v >>= \case
        Some (g@(MultiPolygonGeography _)) -> Right g
        _ -> Left "Invalid Geography MultiPolygon hex"

instance PersistField (Some Geography) where
    toPersistValue (Some g) = case g of
        PointGeography p ->
            PersistDbSpecific . writeHex $ PointGeometry p (Just 4326)
        LineStringGeography ls -> 
            PersistDbSpecific . writeHex $ LineStringGeometry ls (Just 4326)
        LinearRingGeography lr ->
            PersistDbSpecific . writeHex $ LinearRingGeometry lr (Just 4326)
        PolygonGeography pl ->
            PersistDbSpecific . writeHex $ PolygonGeometry pl (Just 4326)
        MultiPointGeography mp ->
            PersistDbSpecific . writeHex $ MultiPointGeometry mp (Just 4326)
        MultiLineStringGeography mls ->
            PersistDbSpecific . writeHex $ MultiLineStringGeometry mls (Just 4326)
        MultiPolygonGeography mpl ->
            PersistDbSpecific . writeHex $ MultiPolygonGeometry mpl (Just 4326)
    fromPersistValue (PersistDbSpecific b) = case readHex b of
        Just (Some (PointGeometry p (Just 4326))) ->
            Right . Some $ PointGeography p
        Just (Some (LineStringGeometry ls (Just 4326))) ->
            Right . Some $ LineStringGeography ls
        Just (Some (LinearRingGeometry lr (Just 4326))) ->
            Right . Some $ LinearRingGeography lr
        Just (Some (PolygonGeometry pl (Just 4326))) ->
            Right . Some $ PolygonGeography pl
        Just (Some (MultiPointGeometry mp (Just 4326))) ->
            Right . Some $ MultiPointGeography mp
        Just (Some (MultiLineStringGeometry mls (Just 4326))) ->
            Right . Some $ MultiLineStringGeography mls
        Just (Some (MultiPolygonGeometry mpl (Just 4326))) ->
            Right . Some $ MultiPolygonGeography mpl
        _ -> Left "Invalid Geography hex"
    fromPersistValue _ = Left "Geography values must be converted from PersistDbSpecific"


instance PersistFieldSql (Geography Point) where
    sqlType _ = SqlOther "geography(Point,4326)"

instance PersistFieldSql (Geography LineString) where
    sqlType _ = SqlOther "geography(LineString,4326)"

instance PersistFieldSql (Geography LinearRing) where
    sqlType _ = SqlOther "geography(LinearRing,4326)"

instance PersistFieldSql (Geography Polygon) where
    sqlType _ = SqlOther "geography(Polygon,4326)"

instance PersistFieldSql (Geography MultiPoint) where
    sqlType _ = SqlOther "geography(MultiPoint,4326)"

instance PersistFieldSql (Geography MultiLineString) where
    sqlType _ = SqlOther "geography(MultiLineString,4326)"

instance PersistFieldSql (Geography MultiPolygon) where
    sqlType _ = SqlOther "geography(MultiPolygon,4326)"

instance PersistFieldSql (Some Geography) where
    sqlType _ = SqlOther "geography"
