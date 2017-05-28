{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api where

import qualified Data.Aeson          as A
import           Data.Aeson.Types
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

-- | Types declaration
type TableName  = Text
type ColumnName = Text
data FieldValue = Int | String
data Operator   = Equals | NotEquals | LargerThan | LessThan | NotNull | Null
data Command    = Command Text deriving (Show, Generic) -- SELECT TableName | INSERT TableName | UPDATE TableName | DELETE TableName deriving (Show, Generic)
data JoinTable  = JoinTable Text Text Text Text Text
data Where      = Where TableName ColumnName Operator FieldValue
data Groupby    = Groupby ColumnName
data Orderby    = Orderby ColumnName


data SqlQuery = SqlQuery
  { command        :: Command
  , selectName     :: Text
  , joinTables     :: !Array
  , whereCondition :: !Array
  , groupByFields  :: !Array
  , orderByFields  :: !Array

  } deriving (Show)

instance FromJSON Command
instance ToJSON Command

parseJoinTable :: Value -> Parser JoinTable
parseJoinTable = withObject "object" $ \o -> do
    a <- o .: "tableName"
    b <- o .: "field"
    c <- o .: "operator"
    d <- o .: "withTable"
    e <- o .: "withField"
    return $ JoinTable a b c d e


instance FromJSON SqlQuery where
    parseJSON = withObject "story" $ \o -> do
    command        <- o .: "command"
    selectName     <- o .: "selectName"
    joinTables     <- o .: "join"
    whereCondition <- o .: "whereCondition"
    groupByFields  <- o .: "groupByFields"
    orderByFields  <- o .: "orderByFields"
    return SqlQuery{..}


-- instance ToJSON SqlQuery where
--   toJSON SqlQuery {..} =
--     object [name .= "name", author .= "author", authorBorn .= 5]

-- data SqlSelectQuery = SqlSelectQuery Command (Maybe [JoinTable]) (Maybe Groupby) (Maybe Orderby)

getApiR :: Handler Value
getApiR = do
  return $ A.String "Serviette - SQL JSON API"

postApiR :: Handler Value
postApiR = do
  sql <- requireJsonBody :: Handler SqlQuery
  print sql
  return $ A.String "Serviette - SQL JSON API"





