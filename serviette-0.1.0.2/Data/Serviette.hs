{-# LANGUAGE OverloadedStrings #-}
-- | Define functions needed for Json to Text manipulation
-- current version supports only raw sql string result
-- next version will implement database query result in the json format
-- as well as errors in the json structure if any

module Data.Serviette (SqlQuery, SqlResultQuery, rawSqlStr) where

import           Data.ApiDataTypes
import           Data.Text         hiding (concat, foldl, map)


-- | Various Getters

-- | Extracts the action and appends space
extractAction :: Action -> Text
extractAction (Action t) = append t  " "

-- | Extracts table name to Text
extractTableName :: TableName -> Text
extractTableName (TableName t) = t

-- | Extracts table column name to Text
extractColumnName :: ColumnName -> Text
extractColumnName (ColumnName t) = t

-- | Extracts Operator to Text
extractOperator :: Operator -> Text
extractOperator (Operator t) = t

-- | Fetches the Action from SqlQuery
getActionArg :: SqlQuery -> Action
getActionArg q = action q

-- | Gets the main table that Action is performed on
getSelectTableArg :: SqlQuery -> TableName
getSelectTableArg q = selectName q

-- | Retrieves the join list from the SqlQuery
getJoinTableArg :: SqlQuery -> [JoinTable]
getJoinTableArg q =  joinTables q

-- | Gets the where condition list
getWhereConditionArg :: SqlQuery -> [WhereCondition]
getWhereConditionArg q =  whereCondition q

-- | Retrieves the result Format
getFormatArg :: SqlQuery -> Int
getFormatArg q =  getFormat $ Format $ format q

-- | Formats the join table list
formatJoinStr :: JoinTable -> Text
formatJoinStr j = foldl append "" (" join " : [(extractTableName $ tablename j) , " on " , (extractColumnName $ field j) , " " ,(extractOperator $ operator j) , " " , (extractTableName $ withTable j)  , "." , (extractColumnName $ withField j), " " ])

-- | Fetches field value depending on field type
formatFieldValue :: FieldValue -> String
formatFieldValue a =
  case a of
    IntField x  -> show x
    TextField x -> show x
    DateField x -> show x

-- | Formats WhereCondition to Text
formatWhereConditionStr :: WhereCondition -> Text
formatWhereConditionStr j = foldl append " " (" where " : [ (extractTableName $ whereTableName j), "." , (extractColumnName $ whereField j) , " " ,(extractOperator $ whereOperator j) , " " , (pack $ formatFieldValue $ whereFieldValue j)])


-- | Creates final SqlResultQuery type
formatToSqlResultQueryType sql = SqlResultQuery (getActionArg sql) (getSelectTableArg sql) (getJoinTableArg sql) (getWhereConditionArg sql)

-- | Returns raw sql query string
rawSqlStr :: SqlQuery -> Text
rawSqlStr s =
  foldl append "" [(extractAction $ getAction sql) ,(extractTableName $ getSelectTable sql) , joins , whereConditions ]
  where joins = foldl append "" $ fmap formatJoinStr $ getJoins sql
        whereConditions = foldl append "" $ fmap formatWhereConditionStr $ getWhereCondition sql
        sql = formatToSqlResultQueryType s
