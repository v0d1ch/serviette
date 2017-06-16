{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This provides types that represent sql query structure
module ApiDataTypes where

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

-- | Represents join table for the sql query
data JoinTable = JoinTable
  { tablename          :: TableName
  , field              :: ColumnName
  , operator           :: Operator
  , withTable          :: TableName
  , withField          :: ColumnName
  , whereConditionJoin :: Text
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
  } deriving (Show, Generic)

-- | Represents type that is the result of the json "computation"
data SqlResultQuery = SqlResultQuery
  { getAction         :: Action
  , getSelectTable    :: TableName
  , getJoins          :: [JoinTable]
  , getWhereCondition :: [WhereCondition]
  } deriving (Show, Generic)


-- | Instances

-- | Json instance for TableName
instance FromJSON TableName

-- | Json instance for TableName
instance ToJSON TableName

-- | aeson instance for Action
instance FromJSON Action

-- | aeson instance for Action
instance ToJSON Action

-- | aeson instance for Operator
instance FromJSON Operator

-- | aeson instance for Operator
instance ToJSON Operator

-- | aeson instance for JoinTable
instance FromJSON  JoinTable

-- | aeson instance for JoinTable
instance ToJSON  JoinTable

-- | aeson instance for ColumnName
instance FromJSON  ColumnName

-- | aeson instance for ColumnName
instance ToJSON ColumnName

-- | aeson instance for SqlQuery
instance FromJSON SqlQuery

-- | aeson instance for SqlQuery
instance ToJSON SqlQuery

-- | aeson instance for SqlResultQuery
instance FromJSON SqlResultQuery

-- | aeson instance for SqlResultQuery
instance ToJSON SqlResultQuery

-- | aeson instance for FieldValue
instance FromJSON FieldValue

-- | aeson instance for FieldValue
instance ToJSON FieldValue

-- | aeson instance for WhereCondition
instance ToJSON   WhereCondition

-- | aeson instance for WhereCondition
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
