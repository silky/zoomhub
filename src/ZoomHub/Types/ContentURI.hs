{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZoomHub.Types.ContentURI
  ( ContentURI
  , ContentURI'(ContentURI)
  , ContentURIColumn
  , pContentURI
  , toColumn
  ) where

import           Data.Aeson                           (ToJSON, Value (String),
                                                       toJSON)
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as PGS
import           Database.SQLite.Simple               (SQLData (SQLText))
import           Database.SQLite.Simple.FromField     (FromField, ResultError (ConversionFailed),
                                                       fromField, returnError)
import           Database.SQLite.Simple.Internal      (Field (Field))
import           Database.SQLite.Simple.Ok            (Ok (Ok))
import           Database.SQLite.Simple.ToField       (ToField, toField)
import           Opaleye                              (Column, PGText,
                                                       QueryRunnerColumnDefault,
                                                       fieldQueryRunnerColumn,
                                                       pgStrictText,
                                                       queryRunnerColumnDefault)
import           Servant                              (FromText, fromText)

newtype ContentURI' a = ContentURI { unContentURI :: a }
  deriving (Eq)
type ContentURI = ContentURI' Text

instance Show ContentURI where
  show = T.unpack . unContentURI

-- Functor
instance Functor ContentURI' where
  fmap f (ContentURI a) = ContentURI (f a)

-- Text
instance FromText ContentURI where
  fromText t
    | "http://" `T.isPrefixOf` t             = Just (ContentURI t)
    | "https://" `T.isPrefixOf` t            = Just (ContentURI t)
    | "zoomit://thumbnail/" `T.isPrefixOf` t = Just (ContentURI t)
    | otherwise = Nothing

-- JSON
instance ToJSON ContentURI where
  toJSON = String . unContentURI

-- SQLite
instance ToField ContentURI where
  toField = SQLText . unContentURI

instance FromField ContentURI where
  fromField field@(Field (SQLText t) _) =
    case fromText t of
      Just r  -> Ok r
      Nothing -> returnError ConversionFailed field "Invalid `ContentURI`"
  fromField field = returnError ConversionFailed field "Invalid `ContentURI`"

-- PostgreSQL
type ContentURIColumn = ContentURI' (Column PGText)
$(makeAdaptorAndInstance "pContentURI" ''ContentURI')

toColumn :: ContentURI -> ContentURIColumn
toColumn = fmap pgStrictText

instance PGS.FromField ContentURI where
  fromField f mdata = PGS.fromField f mdata >>= parseContentURI
    where
      parseContentURI t = case fromText t of
        Just r  -> return r
        Nothing -> PGS.returnError PGS.ConversionFailed f "invalid content URI"

instance QueryRunnerColumnDefault PGText ContentURI where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
