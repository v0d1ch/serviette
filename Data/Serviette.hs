{-# LANGUAGE OverloadedStrings #-}
-- | Define functions needed for Json to Text manipulation
-- current version supports only raw sql string result
-- next version will implement database query result in the json format
-- as well as errors in the json structure if any

module Data.Serviette (SqlQuery, SqlResultQuery, rawSqlStr) where

import           Data.ApiDataTypes
import           Data.Text         hiding (concat, foldl, map)
import           Data.Aeson
import           TextShow


-- | Extracts the action and appends space
extractAction :: Action -> Text
extractAction (Action t) =
  if t == "SELECT"
    then  append t  " "
  else if t == "DELETE"
    then  append t  " FROM "
  else if t == "INSERT"
    then  append t  " INTO "
  else error "Action parameter id wrong"

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

-- | Retrieves the set list from the SqlQuery
getSetFieldsArg :: SqlQuery -> Maybe [SetField]
getSetFieldsArg q =  set q

-- | Retrieves the join list from the SqlQuery
getJoinTableArg :: SqlQuery -> Maybe [JoinTable]
getJoinTableArg q =  joinTables q

-- | Gets the where condition list
getWhereConditionArg :: SqlQuery -> Maybe [WhereCondition]
getWhereConditionArg q =  whereCondition q

-- | Retrieves the result Format
getFormatArg :: SqlQuery -> Int
getFormatArg q =  getFormat $ Format $ format q

-- | Formats the join table list
formatSetStr :: SetField -> Text
formatSetStr j = (foldl append "" (" set " : [(extractColumnName $ columnName j) , " = " , (formatFieldValue $ setFieldValue j) , " ," ]))


-- | Formats the join table list
formatJoinStr :: JoinTable -> Text
formatJoinStr j = foldl append "" (" join " : [(extractTableName $ tablename j) , " on " , (extractColumnName $ field j) , " " ,(extractOperator $ operator j) , " " , (extractTableName $ withTable j)  , "." , (extractColumnName $ withField j), " " ])

-- | Fetches field value depending on field type
formatFieldValue :: FieldValue -> Text
formatFieldValue a =
  case a of
    IntField x  -> showt x
    TextField x -> showt x
    DateField x -> showt x

-- | Formats WhereCondition to Text
formatWhereConditionStr :: WhereCondition -> Text
formatWhereConditionStr j = foldl append " " (" where " : [ (extractTableName $ whereTableName j), "." , (extractColumnName $ whereField j) , " " ,(extractOperator $ whereOperator j) , " " , (formatFieldValue $ whereFieldValue j)])


-- | Creates final SqlResultQuery type
formatToSqlResultQueryType sql = SqlResultQuery (getActionArg sql) (getSelectTableArg sql) (getSetFieldsArg sql) (getJoinTableArg sql) (getWhereConditionArg sql)

-- | Returns raw sql query string
rawSqlStr :: SqlQuery -> Text
rawSqlStr s =
  foldl append "" [(extractAction $ getAction sql) ,(extractTableName $ getSelectTable sql) , setFields , joins , whereConditions ]
  where setFields = case  getSetFields sql of
                      Just x -> Data.Text.init $  foldl append "" $ fmap formatSetStr x
                      Nothing -> ""
        joins = case getJoins sql of
                     Just x -> foldl append "" $ fmap formatJoinStr x
                     Nothing -> ""
        whereConditions = case getWhereCondition sql of
                            Just x -> foldl append "" $ fmap formatWhereConditionStr x
                            Nothing -> ""
        sql = formatToSqlResultQueryType s
