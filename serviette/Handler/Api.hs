{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api where

import qualified Data.Aeson          as A
import           Data.Aeson.Types    as AT
import qualified Data.Vector as V
import           Import
import Control.Monad.State.Strict as ST

-- | Type declaration

data TableName  = TableName Text deriving (Show, Generic)
data ColumnName = ColumnName Text deriving (Show, Generic)
data FieldValue = Int | String deriving (Show, Generic)
data Operator   = Equals | NotEquals | LargerThan | LessThan | NotNull | Null deriving (Show, Generic)
data Format     = Format {getFormat :: Int} deriving (Show, Eq)
data Command    = SELECTT TableName | INSERTT TableName | UPDATET TableName | DELETET TableName deriving (Show, Generic)
data JoinTableList = JoinTableList A.Array deriving (Show, Generic)

data JoinTable = JoinTable
  { tablename :: Text
  , field :: Text
  , operator :: Text
  , withTable :: Text
  , whereConditionJoin :: Text
  } deriving (Show, Generic)

data Where      = Where TableName ColumnName Operator FieldValue deriving (Show, Generic)
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
data SqlRaw = SqlRaw  Command  TableName JoinTableList


-- | Instances

instance FromJSON JoinTableList
instance ToJSON JoinTable

instance FromJSON JoinTable where
    parseJSON = withObject "joins" $ \o -> do
    a <- o .: "tableName"
    b <- o .: "field"
    c <- o .: "operator"
    d <- o .: "withTable"
    e <- o .: "withField"
    return $ JoinTable a b c d e


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

parseJoinTable :: Value -> Parser JoinTable
parseJoinTable = withObject "object" $ \o -> do
    a <- o .: "tableName"
    b <- o .: "field"
    c <- o .: "operator"
    d <- o .: "withTable"
    e <- o .: "withField"
    return $ JoinTable a b c d e

parseJoinTableList :: Value -> Parser JoinTableList
parseJoinTableList (Object o) = JoinTableList <$> (o .: "join")
parseJoinTableList _ = mzero

parserJoinTable :: Value -> Parser JoinTable
parserJoinTable (Object o) =  do
    a <- o .: "tableName"
    b <- o .: "field"
    c <- o .: "operator"
    d <- o .: "withTable"
    e <- o .: "withField"
    return $ JoinTable a b c d e
parserJoinTable _ = mzero


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

getJoinTablesList :: A.Array -> Parser (Vector JoinTable)
getJoinTablesList joins =  mapM parseJoinTable joins


getJoinTablesStr :: Parser (Vector JoinTable) -> Maybe Text
getJoinTablesStr joins = do
  j <- joins
  let tn = map tablename  (V.toList j)
  return $ intercalate " " tn

formatRawSql :: SqlQuery -> Text
formatRawSql sql = command sql ++ " " ++ selectName sql ++  joins
  where joins = getJoinTablesList $ joinTables sql


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
