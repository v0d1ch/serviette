{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api where

import qualified Data.Aeson       as A
import           Data.Aeson.Types as AT
import           Import


-- | Type declaration

data TableName = TableName Text
  deriving (Show, Generic)

data ColumnName = ColumnName Text
  deriving (Show, Generic)

data FieldValue = FieldValue Text
  deriving (Show, Generic)

data Operator = Operator Text
  deriving(Show, Generic)

data OperatorData
  = Equals
  | NotEquals
  | LargerThan
  | LessThan
  | NotNull
  | Null
  deriving (Show, Generic)

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
  , whereConditionJoin :: Text
  } deriving (Show, Generic)

data Where  = Where TableName ColumnName Operator FieldValue
  deriving (Show, Generic)

data SqlQuery = SqlQuery
  { format         :: Int
  , action         :: Action
  , selectName     :: TableName
  , joinTables     :: [JoinTable]
  , whereCondition :: !Array
  } deriving (Show, Generic)

data SqlResultQuery = SqlResultQuery
  { getAction      :: Action
  , getSelectTable :: TableName
  , getJoins       :: [JoinTable]
  } deriving (Show, Generic)

data SqlRaw = SqlRaw Command TableName [JoinTable]

-- | Various Getters

extractAction :: Action -> Text
extractAction (Action t) = t

extractTableName :: TableName -> Text
extractTableName (TableName t) = t


getActionArg :: SqlQuery -> Action
getActionArg q = action q

getSelectTableArg :: SqlQuery -> TableName
getSelectTableArg q = selectName q

getJoinTableArg :: SqlQuery -> [JoinTable]
getJoinTableArg q =  joinTables q

getFormatArg :: SqlQuery -> Int
getFormatArg q =  getFormat $ Format $ format q

rawSqlStr :: SqlResultQuery -> Text
rawSqlStr sql = (extractAction $ getAction sql) ++ " " ++ (extractTableName $ getSelectTable sql)

-- | Handlers

getApiR :: Handler Value
getApiR = do
  return $ A.String "Serviette - SQL JSON API"

postApiR :: Handler Value
postApiR = do
  sql <- requireJsonBody :: Handler SqlQuery
  let sqlR = SqlResultQuery (getActionArg sql) (getSelectTableArg sql) (getJoinTableArg sql)
  return $ A.String $ rawSqlStr sqlR
  -- return $ A.String $ "Serviette"


-- | Instances

instance FromJSON TableName
instance ToJSON TableName

instance FromJSON Action
instance ToJSON Action

instance FromJSON Command
instance ToJSON Command

instance FromJSON Operator
instance ToJSON Operator

instance FromJSON OperatorData
instance ToJSON OperatorData

instance FromJSON  JoinTable
instance ToJSON  JoinTable

instance FromJSON  ColumnName
instance ToJSON ColumnName


instance FromJSON SqlQuery
instance ToJSON SqlQuery

instance FromJSON SqlResultQuery
instance ToJSON SqlResultQuery


-- | JSON Example
{-
   received JSON example
   {
     sql:{
      format:"raw",
      command : "SELECT | INSERT | UPDATE | DELETE",
      selectName: "users",
      join:[
          {tableName:"contracts",field:"contractField",operator:"",withTable:"users", withField:"usersField"},
          {tableName:"commissions",field:"contractField",operator:"",withTable:"contracts", withField:"usersField"}

      ],
      whereCondition:[
          {tableName:"commissions",field:"contractField",operator:"", fieldValue:1}
      ],
     }
   }

-}
