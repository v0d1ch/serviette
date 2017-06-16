{-# LANGUAGE OverloadedStrings #-}

module Serviette (SqlQuery, SqlResultQuery, rawSqlStr) where

import           ApiDataTypes
import           Data.Text    hiding (concat, map)


-- | Various Getters

extractAction :: Action -> Text
extractAction (Action t) = append t  " "

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
formatJoinStr j = Prelude.foldl append "" (" join " : [(extractTableName $ tablename j) , " on " , (extractColumnName $ field j) , " " ,(extractOperator $ operator j) , " " , (extractTableName $ withTable j)  , "." , (extractColumnName $ withField j), " " ])

formatFieldValue :: FieldValue -> String
formatFieldValue a =
  case a of
    IntField x  -> show x
    TextField x -> show x
    DateField x -> show x

formatWhereConditionStr :: WhereCondition -> Text
formatWhereConditionStr j = Prelude.foldl append " " (" where " : [ (extractTableName $ whereTableName j), "." , (extractColumnName $ whereField j) , " " ,(extractOperator $ whereOperator j) , " " , (pack $ formatFieldValue $ whereFieldValue j)])


formatToSqlResultQueryType sql = SqlResultQuery (getActionArg sql) (getSelectTableArg sql) (getJoinTableArg sql) (getWhereConditionArg sql)

rawSqlStr :: SqlQuery -> Text
rawSqlStr s =
  Prelude.foldl append "" [(extractAction $ getAction sql) ,(extractTableName $ getSelectTable sql) , joins , whereConditions ]
  where joins = Prelude.foldl append "" $ fmap formatJoinStr $ getJoins sql
        whereConditions = Prelude.foldl append "" $ fmap formatWhereConditionStr $ getWhereCondition sql
        sql = formatToSqlResultQueryType s
