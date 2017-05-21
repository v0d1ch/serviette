module Handler.Api where

import qualified Data.Aeson              as A
import           Data.Aeson.Types
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO       as T
import qualified Data.Vector             as V
import           Import
{-
   received JSON example

   {
     sql:{
      command : "SELECT | INSERT | UPDATE | DELETE",
      tableName: "users",
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
type TableName  = String
type ColumnName = String
data FieldValue = Int | String
data Operator   = Equals | NotEquals | LargerThan | LessThan | NotNull | Null
data Command    = SELECT TableName | INSERT TableName | UPDATE TableName | DELETE TableName
data JoinTable  = JoinTable TableName
data Where      = Where TableName ColumnName Operator FieldValue
data Groupby    = Groupby ColumnName
data Orderby    = Orderby ColumnName

data SqlSelectQuery = SqlSelectQuery Command (Maybe [JoinTable]) (Maybe Groupby) (Maybe Orderby)

getApiR :: Handler Value
getApiR = do
  return $ A.String "Serviette - SQL JSON API"

postApiR :: Handler Value
postApiR = do
  sql <- lookupPostParam "sql"
  case sql of
    Nothing ->   return $ A.String "missing root key 'sql' !"
    Just s  ->   return $ A.String "got key 'sql' !"

parseRootObject :: FromJSON a => Value -> Parser a
parseRootObject = withObject "sql" $ \o -> do
  c <- o .: "command"
  return c

