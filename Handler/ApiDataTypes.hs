{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.ApiDataTypes where

import           Data.Aeson.Types as AT
import           Import

-- | Type declaration

data TableName = TableName Text
  deriving (Show, Generic)

data ColumnName = ColumnName Text
  deriving (Show, Generic)

data Operator = Operator Text
  deriving(Show, Generic)

data Action = Action Text
  deriving (Show, Generic)

data Format = Format
  { getFormat :: Int
  } deriving (Show, Eq)

type DateVal = Text

data FieldValue = IntField Int | TextField Text | DateField DateVal deriving (Show, Generic)

data JoinTable = JoinTable
  { tablename          :: TableName
  , field              :: ColumnName
  , operator           :: Operator
  , withTable          :: TableName
  , withField          :: ColumnName
  , whereConditionJoin :: Text
  } deriving (Show, Generic)

data WhereCondition = WhereCondition
  { whereTableName  :: TableName
  , whereField      :: ColumnName
  , whereOperator   :: Operator
  , whereFieldValue :: FieldValue
  } deriving (Show, Generic)


data SqlQuery = SqlQuery
  { format         :: Int
  , action         :: Action
  , selectName     :: TableName
  , joinTables     :: [JoinTable]
  , whereCondition :: [WhereCondition]
  } deriving (Show, Generic)

data SqlResultQuery = SqlResultQuery
  { getAction         :: Action
  , getSelectTable    :: TableName
  , getJoins          :: [JoinTable]
  , getWhereCondition :: [WhereCondition]
  } deriving (Show, Generic)


-- | Instances

instance FromJSON TableName
instance ToJSON TableName

instance FromJSON Action
instance ToJSON Action

instance FromJSON Operator
instance ToJSON Operator

instance FromJSON  JoinTable
instance ToJSON  JoinTable

instance FromJSON  ColumnName
instance ToJSON ColumnName

instance FromJSON SqlQuery
instance ToJSON SqlQuery

instance FromJSON SqlResultQuery
instance ToJSON SqlResultQuery

instance FromJSON FieldValue
instance ToJSON FieldValue

instance ToJSON   WhereCondition
instance FromJSON WhereCondition where
    parseJSON (Object v)
        =  WhereCondition
        <$> v .: "whereTableName"
        <*> v .: "whereField"
        <*> v .: "whereOperator"
        <*> (   IntField  <$> (v .: "whereFieldValue")
            <|> TextField <$> (v .: "whereFieldValue")
            <|> DateField <$> (v .: "whereFieldValue")

            )
    parseJSON _  = mzero

