{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This provides types that represent sql query structure
module Data.ApiDataTypes where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text           as T
import           Generics.Deriving

-- | Type declaration

-- | Represents db table name
data TableName = TableName Text
  deriving (Show, Generic)

-- | Represents db table column name
data ColumnName = ColumnName Text
  deriving (Show, Generic)

-- | Represents operator (=, >, <, like, not like etc.)
data Operator = Operator Text
  deriving(Show, Generic)

-- | Represents the main action for the sql query (SELECT,INSERT, UPDATE, DELETE)
data Action = Action Text
  deriving (Show, Generic)

-- | Represents return format (for now this is only raw sql string)
data Format = Format
  { getFormat :: Int
  } deriving (Show, Eq)

-- | Represents Date field value
type DateVal = Text

-- | Represents field value which can be integer, string or date
data FieldValue = IntField Int | TextField Text | DateField DateVal deriving (Show, Generic)

-- | Represents set fields for the sql insert query
data SetFields  = SetFields
  { columnName           :: ColumnName
  , fieldValue           :: FieldValue 
  } deriving (Show, Generic)


-- | Represents join table for the sql query
data JoinTable = JoinTable
  { tablename          :: TableName
  , field              :: ColumnName
  , operator           :: Operator
  , withTable          :: TableName
  , withField          :: ColumnName
  } deriving (Show, Generic)

-- | Represents main where condition
data WhereCondition = WhereCondition
  { whereTableName  :: TableName
  , whereField      :: ColumnName
  , whereOperator   :: Operator
  , whereFieldValue :: FieldValue
  } deriving (Show, Generic)

-- | Represents intermediate type for receiving json
data SqlQuery = SqlQuery
  { format         :: Int
  , action         :: Action
  , selectName     :: TableName
  , joinTables     :: [JoinTable]
  , whereCondition :: [WhereCondition]
  , set :: [SetFields]

  } deriving (Show, Generic)

-- | Represents type that is the result of the json "computation"
data SqlResultQuery = SqlResultQuery
  { getAction         :: Action
  , getSelectTable    :: TableName
  , getJoins          :: [JoinTable]
  , getWhereCondition :: [WhereCondition]
  } deriving (Show, Generic)


-- | Aeson Instances

instance FromJSON TableName
instance ToJSON TableName

instance FromJSON Action
instance ToJSON Action

instance FromJSON Operator
instance ToJSON Operator

instance FromJSON SetFields 
instance ToJSON  SetFields 

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
