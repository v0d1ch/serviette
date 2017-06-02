{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api where

import qualified Data.Aeson          as A
import           Data.Aeson.Types    as AT
import           Import


-- | Type declaration

data TableName =
  TableName Text
  deriving (Show, Generic)

data ColumnName =
  ColumnName Text
  deriving (Show, Generic)

data FieldValue
  = Int
  | String
  deriving (Show, Generic)

data Operator
  = Equals
  | NotEquals
  | LargerThan
  | LessThan
  | NotNull
  | Null
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

data JoinTableList = JoinTableList
  { jTable :: A.Array
  } deriving (Show, Generic)

data JoinTable = JoinTable
  { tablename :: Text
  , field :: Text
  , operator :: Text
  , withTable :: Text
  , whereConditionJoin :: Text
  } deriving (Show, Generic)

data Where =
  Where TableName
        ColumnName
        Operator
        FieldValue
  deriving (Show, Generic)

data SqlQuery = SqlQuery
  { format :: Int
  , command :: Text
  , selectName :: Text
  , joinTables :: !Array
  , whereCondition :: !Array
  } deriving (Show)

data SqlResultQuery =
  SqlResultQuery Command
                 TableName
                 JoinTableList
  deriving (Show, Generic)

data SqlRaw =
  SqlRaw Command
         TableName
         JoinTableList


-- | Instances

instance FromJSON JoinTableList
instance ToJSON JoinTable

instance FromJSON JoinTable where
  parseJSON (Object v) = parserJoinTable v
  parseJSON _          = empty

instance FromJSON SqlQuery where
    parseJSON = withObject "story" $ \o -> do
    format         <- o .: "format"
    command        <- o .: "command"
    selectName     <- o .: "selectName"
    joinTables     <- o .: "join"
    whereCondition <- o .: "whereCondition"
    return SqlQuery{..}


instance ToJSON SqlResultQuery where
  toJSON (SqlResultQuery (SELECTT (TableName a)) (TableName b) (JoinTableList c)) =
    object ["command" .= A.String a, "selectName" .= A.String b, "joins" .= c]
  toJSON (SqlResultQuery (INSERTT (TableName a)) (TableName b) (JoinTableList c)) =
    object ["command" .= A.String a, "selectName" .= A.String b, "joins" .= c ]
  toJSON (SqlResultQuery (UPDATET (TableName a)) (TableName b) (JoinTableList c)) =
    object ["command" .= A.String a, "selectName" .= A.String b, "joins" .= c]
  toJSON (SqlResultQuery (DELETET (TableName a)) (TableName b) (JoinTableList c )) =
    object ["command" .= A.String a, "selectName" .= A.String b, "joins" .= c]


-- | Parsers

parseJoinTableList :: Value -> Parser JoinTableList
parseJoinTableList (Object o) = JoinTableList <$> (o .: "join")
parseJoinTableList _ = mzero

parserSqlQuery :: Object -> Parser SqlQuery
parserSqlQuery o = SqlQuery <$>
    o .: "format"      <*>
    o .: "command"     <*>
    o .: "selectName"  <*>
    o .: "join"        <*>
    o .: "whereCondition"


parserJoinTable :: Object -> Parser JoinTable
parserJoinTable o = JoinTable <$>
    o .: "tableName" <*>
    o .: "field"     <*>
    o .: "operator"  <*>
    o .: "withTable" <*>
    o .: "withField"


-- | Various Getters

getCommandArg :: SqlQuery -> Command
getCommandArg q =
    case c of
      TableName "SELECT" -> SELECTT c
      TableName "INSERT" -> INSERTT c
      TableName "UPDATE" -> UPDATET c
      TableName "DELETE" -> DELETET c
      _                  -> SELECTT c

    where c = TableName $ command q

getSelectTableArg :: SqlQuery -> TableName
getSelectTableArg q = TableName $ selectName q

getJoinTableArg :: SqlQuery -> JoinTableList
getJoinTableArg q =  JoinTableList $ joinTables q

getFormatArg :: SqlQuery -> Int
getFormatArg q =  getFormat $ Format $ format q


getJoinTablesStr :: A.Array -> Text
getJoinTablesStr joins = undefined 

formatRawSql :: SqlQuery -> Text
formatRawSql sql = command sql ++ " " ++ selectName sql ++ joins
   where joins = getJoinTablesStr $  joinTables sql


-- | Handlers

getApiR :: Handler Value
getApiR = do
  return $ A.String "Serviette - SQL JSON API"

postApiR :: Handler Value
postApiR = do
  sql <- requireJsonBody :: Handler SqlQuery
  let f = getFormatArg sql
  case f of
    1 -> return $ A.String $ formatRawSql sql
    _ -> return $
         A.toJSON $
         SqlResultQuery
           (getCommandArg sql)
           (getSelectTableArg sql)
           (getJoinTableArg sql)

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
