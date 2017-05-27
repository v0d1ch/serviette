{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api where

import qualified Data.Aeson          as A
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
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
      where:[
          {tableName:"commissions",field:"contractField",operator:"", fieldValue:1}
      ],
      groupBy:[

      ],
      orderBy:[

      ]
     }

   }

-}

-- | Types declaration
type TableName  = Text
type ColumnName = Text
data FieldValue = Int | String
data Operator   = Equals | NotEquals | LargerThan | LessThan | NotNull | Null
data Command    = SELECT TableName | INSERT TableName | UPDATE TableName | DELETE TableName deriving (Show, Generic)
data JoinTable  = JoinTable Text Text Text Text Text
data Where      = Where TableName ColumnName Operator FieldValue
data Groupby    = Groupby ColumnName
data Orderby    = Orderby ColumnName


data SqlQuery = SqlQuery
  { command :: Text
  , selectName :: Text
  , joinTables :: [Text]
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

parseJoinList :: Value -> Parser [JoinTable]
parseJoinList = withArray "array" $ \arr ->
    mapM parseJoinTable (V.toList arr)
  
instance FromJSON SqlQuery where
    parseJSON = withObject "story" $ \o -> do
    command <- o .: "command"
    selectName <- o .: "selectName"
    parseJoinList <- o .: "join"
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


parseObject :: Monad m =>  Value -> m (Text, Text, Text, Text, Text)
parseObject (Object obj) = do

  tableName <- case HM.lookup "tableName" obj of
    Just (A.String x) -> return x
    Just _            -> fail "expected a string"
    Nothing           -> fail "no field 'tableName'"

  field <- case HM.lookup "field" obj of
    Just (A.String x) -> return x
    Just _            -> fail "expected a string"
    Nothing           -> fail "no field 'field'"

  operator <- case HM.lookup "operator" obj of
    Just (A.String x) -> return x
    Just _            -> fail "expected a string"
    Nothing           -> fail "no field 'operator'"

  withTable <- case HM.lookup "withTable" obj of
    Just (A.String x) -> return x
    Just _            -> fail "expected a string"
    Nothing           -> fail "no field 'withTable'"

  withField <- case HM.lookup "withField" obj of
    Just (A.String x) -> return x
    Just _            -> fail "expected a string"
    Nothing           -> fail "no field 'withField'"


  return (tableName, field, operator,withTable, withField)

parseObject _    = fail "expected Object "



parseArray :: Monad m => Value -> m [(Text, Text, Text, Text, Text)]
parseArray (Array arr) = mapM parseObject (V.toList arr)
parseArray _           = fail "expected an array"

