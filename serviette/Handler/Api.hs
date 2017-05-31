{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api where

import qualified Data.Aeson          as A
import           Data.Aeson.Types    as AT
import           Import
{-
   received JSON example
   {
     sql:{
      command : "SELECT | INSERT | UPDATE | DELETE",
      selectName: "users",
      join:[
          {tableName:"contracts",field:"contractField",operator:"",withTable:"users", withField:"usersField"},
          {tableName:"commissions",field:"contractField",operator:"",withTable:"contracts", withField:"usersField"}

      ],
      whereCondition:[
          {tableName:"commissions",field:"contractField",operator:"", fieldValue:1}
      ],
      groupByFields:[

      ],
      orderByFields:[

      ]
     }
   }

-}

-- | Type declaration
data TableName  = TableName Text deriving (Show, Generic)
data ColumnName = ColumnName Text deriving (Show, Generic)
data FieldValue = Int | String deriving (Show, Generic)
data Operator   = Equals | NotEquals | LargerThan | LessThan | NotNull | Null deriving (Show, Generic)
data Command    = SELECTT TableName | INSERTT TableName | UPDATET TableName | DELETET TableName deriving (Show, Generic)
newtype JoinTableList = JoinTableList {jtList :: [JoinTable]} deriving (Show, Generic)
data JoinTable  = JoinTable Text Text Text Text Text deriving (Show, Generic)
data Where      = Where TableName ColumnName Operator FieldValue deriving (Show, Generic)
data Groupby    = Groupby ColumnName deriving (Show, Generic)
data Orderby    = Orderby ColumnName deriving (Show, Generic)

instance FromJSON JoinTableList
instance ToJSON JoinTable

data SqlQuery = SqlQuery
  { command        :: Text
  , selectName     :: Text
  , joinTables     :: JoinTableList
  , whereCondition :: !Array
  , groupByFields  :: !Array
  , orderByFields  :: !Array

  } deriving (Show)

data SqlResultQuery =  SqlResultQuery Command  TableName JoinTableList  deriving (Show, Generic)

-- (Maybe [JoinTable]) [Where] (Maybe Groupby) (Maybe Orderby)

instance ToJSON SqlResultQuery where
  toJSON (SqlResultQuery (SELECTT (TableName a)) (TableName b) (JoinTableList c)) =
    object ["command" .= A.String a, "selectName" .= A.String b, "joins" .= c]
  toJSON (SqlResultQuery (INSERTT (TableName a)) (TableName b) (JoinTableList c)) =
    object ["command" .= A.String a, "selectName" .= A.String b, "joins" .= c ]
  toJSON (SqlResultQuery (UPDATET (TableName a)) (TableName b) (JoinTableList c)) =
    object ["command" .= A.String a, "selectName" .= A.String b, "joins" .= c]
  toJSON (SqlResultQuery (DELETET (TableName a)) (TableName b) (JoinTableList c )) =
    object ["command" .= A.String a, "selectName" .= A.String b, "joins" .= c]

parseJoinTable :: Value -> Parser JoinTable
parseJoinTable = withObject "object" $ \o -> do
    a <- o .: "tableName"
    b <- o .: "field"
    c <- o .: "operator"
    d <- o .: "withTable"
    e <- o .: "withField"
    return $ JoinTable a b c d e

-- instance FromJSON JoinTableList where
--   parseJSON (Object o) = JoinTableList <$> (o .: "join")
--   parseJSON _ = error "do a better job!" 

parseJoinTableList :: Value -> Parser JoinTableList
parseJoinTableList (Object o) = JoinTableList <$> (o .: "join")
parseJoinTableList _ = mzero

instance FromJSON JoinTable where
    parseJSON = withObject "joins" $ \o -> do
    a <- o .: "tableName"
    b <- o .: "field"
    c <- o .: "operator"
    d <- o .: "withTable"
    e <- o .: "withField"
    return $ JoinTable a b c d e

parserJoinTable :: Value -> Parser JoinTable
parserJoinTable (Object o) =  do
    a <- o .: "tableName"
    b <- o .: "field"
    c <- o .: "operator"
    d <- o .: "withTable"
    e <- o .: "withField"
    return $ JoinTable a b c d e
parserJoinTable _ = mzero

instance FromJSON SqlQuery where
    parseJSON = withObject "story" $ \o -> do
    command        <- o .: "command"
    selectName     <- o .: "selectName"
    joinTables     <- o .: "join"
    whereCondition <- o .: "whereCondition"
    groupByFields  <- o .: "groupByFields"
    orderByFields  <- o .: "orderByFields"
    return SqlQuery{..}

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
getJoinTableArg q =  joinTables q


getApiR :: Handler Value
getApiR = do
  return $ A.String "Serviette - SQL JSON API"

postApiR :: Handler Value
postApiR = do
  sql <- requireJsonBody :: Handler SqlQuery
  return $ A.toJSON $  SqlResultQuery  (getCommandArg sql)  (getSelectTableArg  sql) (getJoinTableArg sql)
