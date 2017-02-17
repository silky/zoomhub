{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZoomHub.Types.ContentState
  ( ContentState(..)
  , ContentStateColumn
  , fromString
  ) where

import           Data.Profunctor.Product.Default      (Default, def)
import           Data.Text                            (Text)
import qualified Database.PostgreSQL.Simple.FromField as PGS
import           Database.SQLite.Simple               (SQLData (SQLText))
import           Database.SQLite.Simple.FromField     (FromField, ResultError (ConversionFailed),
                                                       fromField, returnError)
import           Database.SQLite.Simple.Internal      (Field (Field))
import           Database.SQLite.Simple.Ok            (Ok (Ok))
import           Database.SQLite.Simple.ToField       (ToField, toField)
import           Opaleye                              (Column,
                                                       Constant (Constant),
                                                       PGText,
                                                       QueryRunnerColumnDefault,
                                                       fieldQueryRunnerColumn,
                                                       pgStrictText,
                                                       queryRunnerColumnDefault)

data ContentState =
  Initialized |
  Active |
  CompletedSuccess |
  CompletedFailure
  deriving (Eq, Show)

fromString :: String -> Maybe ContentState
fromString "initialized" = Just Initialized
fromString "active" = Just Active
fromString "completed:success" = Just CompletedSuccess
fromString "completed:failure" = Just CompletedFailure
fromString _ = Nothing

-- SQLite
instance ToField ContentState where
  toField Initialized      = SQLText "initialized"
  toField Active           = SQLText "active"
  toField CompletedSuccess = SQLText "completed:success"
  toField CompletedFailure = SQLText "completed:failure"

instance FromField ContentState where
  fromField (Field (SQLText "initialized") _) = Ok Initialized
  fromField (Field (SQLText "active") _) = Ok Active
  fromField (Field (SQLText "completed:success") _) = Ok CompletedSuccess
  fromField (Field (SQLText "completed:failure") _) = Ok CompletedFailure
  fromField f = returnError ConversionFailed f "invalid content state"

-- PostgreSQL
type ContentStateColumn = Column PGText

instance PGS.FromField ContentState where
  fromField f mdata = PGS.fromField f mdata >>= parseContentState
    where
      parseContentState :: Maybe Text -> PGS.Conversion ContentState
      parseContentState r = case r of
        Just "initialized"       -> return Initialized
        Just "active"            -> return Active
        Just "completed:success" -> return CompletedSuccess
        Just "completed:failure" -> return CompletedFailure
        Just _  -> PGS.returnError PGS.ConversionFailed f "unrecognized content state"
        Nothing -> PGS.returnError PGS.UnexpectedNull f "empty content state"

instance QueryRunnerColumnDefault PGText ContentState where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant ContentState ContentStateColumn where
  def = Constant def'
    where
      def' :: ContentState -> ContentStateColumn
      def' Initialized      = pgStrictText "initialized"
      def' Active           = pgStrictText "active"
      def' CompletedSuccess = pgStrictText "completed:success"
      def' CompletedFailure = pgStrictText "completed:failure"
