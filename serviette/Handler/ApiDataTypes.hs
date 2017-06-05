{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.ApiDataTypes where

import           Data.Aeson.Types as AT
import           Import

-- | Type declaration

data TableName = TableName Text
  deriving (Show, Generic)

data ColumnName = ColumnName Text
  deriving (Show, Generic)

data FieldValue = Either String Int
  deriving (Show, Generic)

data Operator = Operator Text
  deriving(Show, Generic)

data Action = Action Text
  deriving (Show, Generic)

data Command
  = SELECTT TableName
  | INSERTT TableName
  | UPDATET TableName
  | DELETET TableName
  deriving (Show, Generic)

data Format = Format
  { getFormat :: Int
  } deriving (Show, Eq)


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
  { getAction      :: Action
  , getSelectTable :: TableName
  , getJoins       :: [JoinTable]
  } deriving (Show, Generic)

data SqlRaw = SqlRaw Command TableName [JoinTable]


-- | Instances

instance FromJSON TableName
instance ToJSON TableName

instance FromJSON Action
instance ToJSON Action

instance FromJSON Command
instance ToJSON Command

instance FromJSON Operator
instance ToJSON Operator

instance FromJSON  JoinTable
instance ToJSON  JoinTable

instance FromJSON  ColumnName
instance ToJSON ColumnName

instance FromJSON  WhereCondition
instance ToJSON   WhereCondition

instance FromJSON SqlQuery
instance ToJSON SqlQuery

instance FromJSON SqlResultQuery
instance ToJSON SqlResultQuery

instance ToJSON FieldValue
instance FromJSON FieldValue

