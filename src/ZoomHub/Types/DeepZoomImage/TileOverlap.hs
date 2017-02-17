module ZoomHub.Types.DeepZoomImage.TileOverlap
  ( TileOverlap(..)
  , fromString
  , fromInteger
  ) where

import           Prelude                          hiding (fromInteger)

import           Data.Aeson                       (ToJSON, Value (Number),
                                                   toJSON)
import           Database.SQLite.Simple           (SQLData (SQLInteger))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)

data TileOverlap = TileOverlap0 | TileOverlap1
  deriving (Bounded, Enum, Eq)

fromString :: String -> Maybe TileOverlap
fromString "1" = Just TileOverlap1
fromString "0" = Just TileOverlap0
fromString _ = Nothing

fromInteger :: Integer -> Maybe TileOverlap
fromInteger 1 = Just TileOverlap1
fromInteger 0 = Just TileOverlap0
fromInteger _ = Nothing

-- Tile overlap: Show
instance Show TileOverlap where
  show TileOverlap1 = "1"
  show TileOverlap0 = "0"

-- Tile overlap: JSON
instance ToJSON TileOverlap where
  toJSON TileOverlap1 = Number 1
  toJSON TileOverlap0 = Number 0

-- Tile overlap: SQLite
instance ToField TileOverlap where
  toField TileOverlap1 = SQLInteger 1
  toField TileOverlap0 = SQLInteger 0

instance FromField TileOverlap where
  fromField (Field (SQLInteger 1) _) = Ok TileOverlap1
  fromField (Field (SQLInteger 0) _) = Ok TileOverlap0
  fromField f =
    returnError ConversionFailed f "invalid Deep Zoom image tile overlap"
