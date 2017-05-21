module Handler.Api where

import qualified Data.Aeson as J
import           Import

{-
   received JSON example

   {
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
  return $ J.String "get api"

postApiR :: Handler Value
postApiR = do
  return $ J.String "post api"

