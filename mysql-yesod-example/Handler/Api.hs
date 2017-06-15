module Handler.Api where

import qualified Data.Aeson           as A
import           Handler.ApiDataTypes
import           Import


-- | Various Getters

extractAction :: Action -> Text
extractAction (Action t) = t ++ " "

extractTableName :: TableName -> Text
extractTableName (TableName t) = t

extractColumnName :: ColumnName -> Text
extractColumnName (ColumnName t) = t

extractOperator :: Operator -> Text
extractOperator (Operator t) = t

getActionArg :: SqlQuery -> Action
getActionArg q = action q

getSelectTableArg :: SqlQuery -> TableName
getSelectTableArg q = selectName q

getJoinTableArg :: SqlQuery -> [JoinTable]
getJoinTableArg q =  joinTables q

getWhereConditionArg :: SqlQuery -> [WhereCondition]
getWhereConditionArg q =  whereCondition q

getFormatArg :: SqlQuery -> Int
getFormatArg q =  getFormat $ Format $ format q

formatJoinStr :: JoinTable -> Text
formatJoinStr j = " join "
  ++ (extractTableName $ tablename j) ++ " on "
  ++ (extractColumnName $ field j) ++ " "
  ++ (extractOperator $ operator j) ++ " "
  ++ (extractTableName $ withTable j) ++ "."
  ++ (extractColumnName $ withField j) ++ " "

formatFieldValue :: FieldValue -> String
formatFieldValue a =
  case a of
    IntField x  -> show x
    TextField x -> show x
    DateField x -> show x

formatWhereConditionStr :: WhereCondition -> Text
formatWhereConditionStr j = " where "
  ++ (extractTableName $ whereTableName j) ++ "."
  ++ (extractColumnName $ whereField j) ++ " "
  ++ (extractOperator $ whereOperator j) ++ " "
  ++ (pack $ formatFieldValue $ whereFieldValue j)


rawSqlStr :: SqlResultQuery -> Text
rawSqlStr sql = (extractAction $ getAction sql)
  ++  (extractTableName $ getSelectTable sql)
  ++  (concat . map formatJoinStr $ getJoins sql )
  ++  (concat . map formatWhereConditionStr $ getWhereCondition sql)

-- | Handlers

getApiR :: Handler Value
getApiR = do
  return $ A.String "Serviette - SQL JSON API"

postApiR :: Handler Value
postApiR = do
  sql <- requireJsonBody :: Handler SqlQuery
  let sqlR = SqlResultQuery (getActionArg sql) (getSelectTableArg sql) (getJoinTableArg sql) (getWhereConditionArg sql)
  return $ A.String $ rawSqlStr sqlR
